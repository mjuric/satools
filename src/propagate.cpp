#include <astro/asteroids/catalog.h>
#include <astro/system/preferences.h>
#include <astro/system.h>
#include <astro/system/log.h>
#include <astro/system/error.h>
#include <astro/exceptions.h>
#include <astro/util.h>
#include <astro/net/remoteinstance.h>

#include <fstream>
#include <vector>
#include <algorithm>
#include <memory>

#include <time.h>
#include <stdio.h>

#include "observationcalculator.h"
#include "orbfitlib.h"

#include <iostream>

using namespace std;

using namespace peyton;
using namespace peyton::exceptions;
using namespace peyton::net;
using namespace peyton::asteroids;
using namespace peyton::system;

Preferences pref;

inline void propagateAsteroid(Asteroid &obj, MJD t1)
{
	double ele[6];
	proele_("KEP", obj.t0, obj.elements, t1, ele, 3);

	memcpy(obj.elements, ele, sizeof(double)*6);
	obj.t0 = t1;
}

template<typename T>
inline RemoteInstance &operator <<(RemoteInstance &r, std::vector<T> &v)
{
	r << (int)v.size();
	for(int i = 0; i != v.size(); i++) { r << v[i]; }
	return r;
}

template<typename T>
inline RemoteInstance &operator >>(RemoteInstance &r, std::vector<T> &v)
{
	int size; T o;
	v.clear();
	r >> size;
	while(size--) { r >> o; v.push_back(o); }
	return r;
}

#define SIMPLE(T) \
	int write(RemoteInstance &r) { return r.write(this, sizeof(T)); } \
	int read(RemoteInstance &r) { return r.read(this, sizeof(T)); }

#define DECLARE_SIMPLE(T) \
	template< > inline RemoteInstance & operator<<(RemoteInstance &r, T &v) { r.write(&v, sizeof(T)); return r; } \
	template< > inline RemoteInstance & operator>>(RemoteInstance &r, T &v) { r.read(&v, sizeof(T)); return r; }

DECLARE_SIMPLE(Asteroid);

#define CMD_PROP	1
#define CMD_RESULTS	2
#define CMD_QUIT	3

class CMDPropagate {
public:
	vector<Asteroid> objects;
	vector<MJD> epochs;
public:
	int write(RemoteInstance &r) { r << CMD_PROP << objects << epochs; return 0; }
	int read(RemoteInstance &r) { r >> objects >> epochs; return 0; }
};

class CMDResults {
public:
	vector<Asteroid> objects;
	
	int write(RemoteInstance &r) { r << CMD_RESULTS << objects; return 0; }
	int read(RemoteInstance &r) { r >> objects; return 0; }
};

class Client : public RemoteInstance {
protected:
	enum { idle, propagating, quit } state;
	int command;

	CMDPropagate p;
public:
	void propagate() {
		state = propagating;

		vector<Asteroid>::iterator o = p.objects.begin();
		CMDResults res;
		while(o != p.objects.end()) {
			for(vector<MJD>::iterator t1 = p.epochs.begin(); t1 != p.epochs.end(); t1++) {
				propagateAsteroid(*o, *t1);
				res.objects.push_back(*o);
			}
			o++;

			if(yield()) {
				state = idle;
				return;
			}
		}

//		cout << "Finished propagating.\n";
		
		state = idle;
		*this << res;
	}

	bool yield() {
		if(command == 0 && !(state != idle ? tryRead(command) : read(command))) return false;			// nothing in queue
		if(command != 0 && state != idle) { return true; }			// we're interrupting something

		int cmd = command;
		command = 0;		// yield() is reentrant

		switch(cmd) {
		case CMD_PROP:	*this >> p; propagate(); break;
		case CMD_QUIT:	state = quit; break;
		default:	state = quit; break;
		}
		
		return true;
	}

	virtual int run() {
		OrbfitPropagLibrary::initialize();

		state = idle;
		command = 0;

		while(state != quit) { yield(); }

		return 0;
	}
};

template<typename K, typename V>
class vptr_map : public map<K, V> {
public:
	~vptr_map() {
		for(typename map<K,V>::iterator i = this->begin(); i != this->end(); i++) { delete (*i).second; }
	}
};

template<typename V, typename O> bool exists(V &v, O &o) { return v.find(o) != v.end(); }

#include <map>
#include <list>
#include <stack>

#define IGNORE_EXCEPTIONS(x) try {x} catch (EAny &e) { e.print(); }

class Server : public ServerInstance {
protected:
	struct slice {
		int range[2];
	};
	stack<slice> todo;
	vptr_map<MJD, Catalog *> catalogs;
	time_t t0;
	
	class Data : public InstanceData {
	public:
		slice s;
		time_t started;

		Data() : started(0) {}
	};
	#define DATA (*((Data *)((*cli)->userData())))
public:
	virtual void callbackInitializeSpawnedInstance(RemoteInstance *ri) { ri -> userData() = new Data; }

	virtual int run() {
		const char *ws = System::workspace();
		OrbfitPropagLibrary::initialize();

		const char *satools = getenv("SATOOLS");
		if(satools == NULL) {
			cout << "Please set your $SATOOLS environment variable to point to the root of SATOOLS distribution you are using";
			return -1;
		}
		
		if(argc != 3) {
			cout << "Usage #1: " << argv[0] << " <runset> <astorb>\n";
			cout << "Usage #2: " << argv[0] << " <epochs_file.txt> <astorb>\n";
			cout << "Purpose: propagates the entire ASTORB catalog to a set of MJDs given as runSet (epochs.txt file) or one-MJD-per-line.txt file.\n\n";
			cout << "Input catalog must be in ASTORB2 format. Outputs will be in NATIVE format, and will be stored in " << ws << "/tmp/propagated/<astorb>.<mjd>.obj, one file per mjd\n";
			return -1;
		}

		const char *runSet = argv[1];
		const char *astorb = argv[2];

		time(&t0);

		char buf[1000];
		sprintf(buf, "%s/catalogs/astorb.dat.%s", ws, astorb);
		auto_ptr<Catalog> cat(Catalog::open(buf, "ASTORB2"));

		//
		// open and load epochs file
		//
		vector<MJD> epochs;
		sprintf(buf, "%s/input/%s.d/epochs.txt", ws, runSet);
		if(access(buf, R_OK) != 0) {
			strcpy(buf, runSet);
			if(access(buf, R_OK) != 0) {
				std::cerr << "Cannot find runset or file named '" << buf << "'\n";
				return -1;
			}
		}
		ifstream f(buf);
		while(!f.eof()) {
			MJD t0;
			f.getline(buf, 1000);
			if(sscanf(buf, "%lf", &t0) != 1) continue;
			epochs.push_back(t0);
		}
		sort(epochs.begin(), epochs.end());
		reverse(epochs.begin(), epochs.end());

		//
		// prepare output catalogs
		//
		FOREACH(epochs) {
			sprintf(buf, "%s/tmp/propagated/%s.%5.0f.obj", ws, astorb, *i);
			catalogs[*i] = Catalog::open(buf, "NATIVE", "w");
		}

		//
		// prepare slices
		//
//		vector<slice> xx;
		const int ssize = 300;
		for(int i = ssize; i < cat->recordCount() + ssize; i += ssize) {
			slice s = {i - ssize, min(i, cat->recordCount())};
			todo.push(s);
//			xx.push_back(s);
		}
//		reverse(xx.begin(), xx.end());
//		FOREACH(xx) { todo.push(*i); }

		//
		// spawn remote instances
		//
		string exe = std::string(satools) + "/env.sh /bin/nice -19 " + satools + "/bin/propagate.x";
		spawnFlock("satools-propagate", exe);
		if(!clients.size()) THROW(EAny, "No birds in flock (none of the remote clients started up)!");
		cout << "Number of slaves : " << clients.size() << "\n";
		
		// 5. Propagate
		CMDPropagate p;
		p.epochs = epochs;
		vector<RemoteInstance *> signaled;
		while(clients.size() && (todo.size() || haveBusyInstances())) {
			// Make'm busy
			if(todo.size()) {
				FOREACHj(cli, clients) {
					if(DATA.started) continue;

					DATA.s = todo.top();
					cat->read(p.objects, DATA.s.range[0], DATA.s.range[1]);
					time(&DATA.started);
					(**cli) << p;
				//	cout << "[ " << time(NULL) - t0 << " sec ] " << (*cli)->host << " [" << DATA.s.range[0] << " - " << DATA.s.range[1] << "] started\n";

					todo.pop();
				}
			}

			// Wait for them to phone home with the results
			waitFor(signaled, clients);
			processCompleted(signaled);
		}
		if(todo.size()) { THROW(EAny, "Premature exit. All the birds in the flock died ?!"); }

		FOREACH(clients) { IGNORE_EXCEPTIONS((**i) << CMD_QUIT;) }
		
		return 0;
	}

	bool haveBusyInstances() { FOREACHj(cli, clients) { if(DATA.started) { return true; }; } return false; }

	void processCompleted(vector<RemoteInstance *> &signaled) {
		CMDResults r;
		FOREACHj(cli, signaled) {
			int command;
			try {
				(**cli) >> command;
				
				switch(command) {
				case CMD_RESULTS: {
					(**cli) >> r;
//					FOREACH(r.objects) { cout << (*i).t0 << " " << (*i).name << "\n"; }
					FOREACHj(a, r.objects) { catalogs[(*a).t0]->write(*a, (*a).id); }
					DATA.started = 0;

					cout << "[ ";
					{ FOREACHj(cli, clients) { cout << ((DATA.started) ? "+" : "-"); } }
					char buf[40]; sprintf(buf, "%30s", (*cli)->host.c_str());
					cout << " | " << time(NULL) - t0 << " sec ] " << buf << " [" << DATA.s.range[0] << " - " << DATA.s.range[1] << "] done.";
					cout << "\n";
					cout.flush();
					} break;
				default:
					cout << "Unknown command ! Panic ! Horror ! Exit !!";
					exit(-1);
					break;
				}
			} catch(EEOF &e) {
				e.print();
				cout << "[ " << time(NULL) - t0 << " sec ] " << (*cli)->host << " died.";
				if(DATA.started) {
					todo.push(DATA.s);
					cout << " Returning slice [" << DATA.s.range[0] << " - " << DATA.s.range[1] << "]";
				}
				cout << "\n";
				delete *cli;
			}
		}
	}
};

MAIN(Server, Client);
