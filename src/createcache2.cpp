#include <fstream>

#include <astro/system/preferences.h>
#include <astro/system/error.h>
#include <astro/system.h>

#include <astro/asteroids/catalog.h>
#include <astro/system/log.h>
#include <astro/util.h>
#include <astro/net/remoteinstance.h>
#include <astro/sdss/rungeometry.h>

#include "observationcache.h"
#include "observationcalculator.h"

#include "orbfitlib.h"

#include <iostream>	

using namespace peyton;
using namespace peyton::exceptions;
using namespace peyton::net;
using namespace peyton::asteroids;
using namespace peyton::sdss;

using namespace std;

peyton::system::Preferences pref;

const int CMD_CATALOG = 1;
const int CMD_CALCULATE = 2;
const int CMD_QUIT = 3;
const int CMD_RESULTS = 4;

#define IGNORE_EXCEPTIONS(x) try {x} catch (EAny &e) { e.print(); }

class Client : public RemoteInstance {
protected:
	pair<char *, size_t> cdata;
	Catalog *cat;
public:
	void load_catalog() {
		delete cat;
		delete [] cdata.first;

		(*this) >> cdata.second; // length of data to follow
		DEBUG(verbose, "About to recv size: " << cdata.second);

		cdata.first = new char[cdata.second];
		read(cdata.first, cdata.second);
		cat = Catalog::open((char *)&cdata, "NATIVE-MEMORY");
		if(cat == NULL) {
			DEBUG(verbose, "Could not load NATIVE-MEMORY catalog");
			exit(-1);
		}
		DEBUG(verbose, "Records: " << cat->recordCount());
	}

	void calculate() {
		if(cat == NULL) {
			DEBUG(verbose, "Catalog not preloaded - cannot calculate sky without the catalog!");
			exit(-1);
		}

		int run;
		*this >> run;

		DEBUG(verbose, "CLI Processing run:" << run);

		// Load run
		RunGeometryDB geoms;
		RunGeometry geom;
		geoms.getGeometry(run, geom);

		Radians ra0 = geom.ra;
		DEBUG(verbose, "CLI Loaded run geometry.");

		// calculate
		pair<char *, size_t> sky;

		ObservationCache *cache = new ObservationCache((char *)&sky, "wm");
		ObservationCalculator oc;
		cache->create(geom, cat, &oc);
		delete cache;
		
		// now the cache binary is in sky pair
		// send it back
		DEBUG(verbose, "CLI Writing results back: " << sky.second);
		(*this) << CMD_RESULTS << sky.second;
		write(sky.first, sky.second);

		// delete the cache data
		free(sky.first);
	}

	virtual int run() {
		OrbfitPropagLibrary::initialize();

		cdata.first = NULL; cdata.second = 0; cat = NULL;

		int cmd;
		while(cmd != CMD_QUIT) {
			*this >> cmd;
			switch(cmd) {
			case CMD_CATALOG: load_catalog(); break;
			case CMD_CALCULATE: calculate(); break;
			case CMD_QUIT: break;
			default:
				DEBUG(verbose, "Unknown command [" << cmd << "]! Exiting!");
				exit(-1);
			}
		}

		return 0;
	}
	
	virtual ~Client() {
		delete cat;
		delete [] cdata.first;
	}
};







class Server : public ServerInstance {
protected:
	typedef struct { MJD epoch; int run; } Run;
	vector<Run> runs;

	time_t t0;

	class Data : public InstanceData {
	public:
		Run run;
		time_t started;

		Data() : started(0) { run.epoch = 0; run.run = 0; }
	};
	#define DATA (*((Data *)((*(cli))->userData())))

	char *astorb;
public:
	virtual void callbackInitializeSpawnedInstance(RemoteInstance *ri) { ri -> userData() = new Data; }

	void start_calculation(RemoteInstance *ri, bool sameCatalog)
	{
		RemoteInstance **cli = &ri;
//		DEBUG(verbose, "Same cat " << sameCatalog);
		if(!sameCatalog) {
			string scat = string(System::workspace()) + "/tmp/propagated/" 
				+ astorb + "." + util::str(int(DATA.run.epoch)) + ".obj";

			// find out the file size
			ifstream cat(scat.c_str(), ios::binary);
			cat.seekg(0, ios::end);
			size_t length = cat.tellg();
			cat.seekg(0, ios::beg);

//			DEBUG(verbose, "Length : " << length << " " << scat.c_str());

			// load the file			
			char *buf = new char[length];
			cat.read(buf, length);
			cat.close();

//			DEBUG(verbose, "File loaded " << length);

			// send the file
			*ri << CMD_CATALOG << length;
			ri->write(buf, length);
			delete [] buf;
		}

		// order the client to process the run given
		*ri << CMD_CALCULATE << DATA.run.run;
	}

	virtual int run()
	{
		const char *satools = getenv("SATOOLS");
		if(satools == NULL) {
			cout << "Please set your $SATOOLS environment variable to point to the root of SATOOLS distribution you are using";
			return -1;
		}
		
		if(argc != 3) {
			cout << "Usage: " << argv[0] << " <runset> <astorb>\n";
			cout << "Purpose: Calculates asteroid sky positions for all runs in a given runset\n";
			cout << "The catalogs for the runset must have already been calculated using propagate.x\n";
			return -1;
		}

		time(&t0);
		
		const char *runSet = argv[1];
		astorb = argv[2];

		//
		// open and load epochs file
		//
		string epfile = string(System::workspace()) + "/input/" + runSet + ".d/epochs.txt";
		ifstream f(epfile.c_str());

		string line;
		while(!f.eof()) {
			char colon;
			Run run;
			
			getline(f, line);
			istringstream set(line);
			set >> run.epoch >> colon;
			if(colon != ':') continue;
			
			set >> run.run;
			while(!set.eof()) {
				runs.push_back(run);
				set >> run.run;
			}
		}

		//
		// spawn remote instances
		//
		string exe = string("/bin/nice -19 ") + satools + "/bin/createcache2.x";
		spawnFlock("satools-createcache", exe);
		if(!clients.size()) THROW(EAny, "No birds in flock (none of the remote clients started up)!");
		cout << "Number of slaves : " << clients.size() << "\n";

		//
		// do calculations
		//
		vector<RemoteInstance *> signaled;
		while(clients.size() && (runs.size() || haveBusyInstances())) {
			// Make'm busy
			FOREACHj(cli, clients)
			{
				makeBusy(&(*cli));
			}

			// Wait for them to phone home with the results
			waitFor(signaled, clients);
			processCompleted(signaled);
		}
		if(runs.size()) { THROW(EAny, "Premature exit. All the birds in the flock died ?!"); }

		FOREACH(clients) { IGNORE_EXCEPTIONS((**i) << CMD_QUIT;) }

		return 0;
	}

	void makeBusy(RemoteInstance **cli)
	{
		if(!runs.size()) return;
		if(DATA.started) return;

		// assign and start up run
		DATA.run = runs.back();
		time(&DATA.started);
		start_calculation(*cli, false);

		runs.pop_back();
	}

	bool haveBusyInstances() { FOREACHj(cli, clients) { if(DATA.started) { return true; }; } return false; }

	void processCompleted(vector<RemoteInstance *> &signaled) {
		FOREACHj(cli, signaled) {
			int command;
			try {
				(**cli) >> command;

				switch(command) {
				case CMD_RESULTS: {
					string cache = string(System::workspace()) + "/tmp/skies/" + util::str(DATA.run.run) + ".cache";

					{ // write the file
						ofstream out(cache.c_str());
						if(!out.good()) { THROW(EAny, "Cannot open cache file [" + cache + "] for output"); }
						
						size_t length;
						(**cli) >> length;

						char *buf = new char[length];
						(*cli)->read(buf, length);
						out.write(buf, length);
						delete [] buf;
					}

					// mark this client as being free for future use
					DATA.started = 0;

					// some friendly UI info
					cout << "[ ";
					{ FOREACHj(cli, clients) { cout << ((DATA.started) ? "+" : "-"); } }
					char buf[40]; sprintf(buf, "%30s", (*cli)->host.c_str());
					cout << " | " << time(NULL) - t0 << " sec ] " << buf << " [ run " << DATA.run.run << " ] done.";
					cout << "\n";
					cout.flush();

					// immeadeately try to make the client busy
					makeBusy(&*cli);
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
					runs.push_back(DATA.run);
					cout << " Returning run [" << DATA.run.run << "]";
				}
				cout << "\n";
				delete *cli;
			}
		}
	}
};

MAIN(Server, Client);
