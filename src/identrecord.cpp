#include "identrecord.h"

#include <astro/constants.h>
#include <astro/system/error.h>
#include <astro/system.h>
#include <astro/util.h>
#include <astro/system/log.h>
#include <astro/exceptions.h>

#include <iomanip>
#include <sstream>
#include <vector>

#include <stdio.h>
#include <math.h>
#include <unistd.h>

#include <string>
#include <map>

using namespace std;

using namespace peyton;
using namespace peyton::asteroids;
using namespace peyton::exceptions;

const long long Dra = 3600 * 360; // dimension of ra space, in 1" pixels
const long long Ddec = 3600 * 180; // dimension of dec space, in 1" pixels
const long long Drun = 10000;     // dimension of run space, in runs :)

///////////////////

class binary_istream {
public:
	istream &in;
	int binary;
public:
	binary_istream(istream &i, int bin = ios::binary) : in(i), binary(bin) {}
	// delegation
	bool eof () const { return in.eof(); }
	binary_istream& read(char* s, streamsize n) { in.read(s, n); return *this; }
	bool good () const { return in.good(); }
};

template<typename T>
binary_istream &operator >>(binary_istream &in, T &scalar)
{
	if(!in.binary) { in.in >> scalar; return in; }
	return in.read((char *)&scalar, sizeof(scalar));
}

binary_istream &operator >>(binary_istream &in, char *s)
{
	if(!in.binary) { in.in >> s; return in; }

	int length;
	in >> length;
	in.read(s, length);
	s[length] = 0;
	return in;
}

binary_istream &operator >>(binary_istream &in, std::string &s)
{
	if(!in.binary) { in.in >> s; return in; }

	int length;
	in >> length;
	char *buf = new char[length+1];
	in.read(buf, length);
	buf[length] = 0;
	s = buf;
	delete [] buf;
	return in;
}

///////////////////

class binary_ostream {
public:
	bool emptyString(const char *c, int length) {
		FOR(0, length) { if(ignore.find(c[i]) == string::npos) return false; }
		return true;
	}
public:
	ostream &out;
	string ignore;
	int binary;
public:
	binary_ostream(ostream &i, int bin = ios::binary) : binary(bin), out(i), ignore(" \t,\r\n") {}
	// delegation
	bool eof () const { return out.eof(); }
	binary_ostream&  write ( const char* str , streamsize n ) { out.write(str, n); return *this; }
	bool good () const { return out.good(); }
};

template<typename T>
binary_ostream &operator <<(binary_ostream &out, const T scalar)
{
	if(!out.binary) { out.out << scalar; return out; }

	return out.write((char *)&scalar, sizeof(scalar));
}

binary_ostream &operator <<(binary_ostream &out, const std::string &s)
{
	if(!out.binary) { out.out << s; return out; }
	
	// do not write whitespace-only strings
	int length = s.length();
	if(out.emptyString(s.c_str(), length)) return out;

	out << length;
	return out.write(s.c_str(), length);
}

binary_ostream &operator <<(binary_ostream &out, const char *s)
{
	if(!out.binary) { out.out << s; return out; }
	
	int length = strlen(s);
	if(out.emptyString(s, length)) return out;

	out << length;
	return out.write(s, length);
}

binary_ostream &operator <<(binary_ostream &out, const char c)
{
	if(!out.binary) { out.out << c; return out; }

	if(out.emptyString(&c, 1)) return out;
	
	return out.write(&c, 1);
}

///////////////////

class ObjectIDDatabase {
protected:
	static long long sec(double v) {
		int sign = v > 0 ? 1 : -1;
		v *= sign;
		int d = int(v); v -= d; v *= 60;
		int m = int(v); v -= m; v *= 60;
		return sign * (int(v) + 60 * m + 3600 * d);
	}
	static long long getHash(int run, long long ra, long long dec) {
		return ra + (dec + Ddec/2) *Dra + run*Dra*Ddec;
	}
	static long long getHash(int run, double ra, double dec) {
		return getHash(run, sec(ra), sec(dec));
	}
	struct ids {
		int run; double ra, dec; ObjectID *id;
		long long getHash() { return ObjectIDDatabase::getHash(run, ra, dec); }
	};
	typedef map<long long, ids> idMap_t;
	idMap_t db;
	int counter;
	bool initialized, dirty;
	int dbMode;
public:
	ObjectIDDatabase() { initialized = false; dirty = false; dbMode = ios::binary; }

	void setBinary(bool binary) { dbMode = binary ? ios::binary : 0; }
	void setDirty(bool d) { dirty = d; }

	void initialize() {
		if(initialized) return;

		string idFile(System::workspace());
		idFile += (dbMode == ios::binary) ? "/lib/ids.dat" : "/lib/ids.txt";

		DEBUG(verbose, "DB: " << idFile);

		if(access(idFile.c_str(), R_OK) != 0) {
			THROW(EAny, std::string("Fatal error - cannot open ObjectID database file : '") + idFile.c_str() + "'");
		}

		ifstream fraw(idFile.c_str(), (std::ios::openmode)(ios::in | dbMode));
		binary_istream f(fraw, dbMode);

		f >> counter;
		ObjectID id; ids s;
		while(!f.eof()) {
			s.run = -1;
			f >> s.run >> s.ra >> s.dec >> id;
//			cout << s.run << " " << s.ra << " " << s.dec << "\n"; cout.flush();
			if(s.run == -1) break;

			s.id = new ObjectID(id);
			db[s.getHash()] = s;
		}
		DEBUG(verbose, "DB: Loaded " << db.size() << " objects, counter = " << counter);

		initialized = true;
	}

	ObjectID getId(int run, Radians ra, Radians dec) {
		initialize();
		ra /= ctn::d2r; dec /= ctn::d2r;

		long long ras0 = sec(ra), decs0 = sec(dec), ras, decs, h;
		idMap_t::iterator it;
		for(int i = -1; i != 2; i++) {
			ras = (ras0 + i) % Dra;
			for(int j = -1; j != 2; j++) {
				decs = (decs0 + j) % Ddec;

				if((it = db.find(h = getHash(run, ras, decs))) != db.end()) {
					ids &id = (*it).second;
					if(fabs(id.ra - ra) < 1./3600.) {
						if(fabs(id.dec - dec) < 1./3600.) {
							return *id.id;
						}
					}
				}
			}
		}

		// generate new
		dirty = true;

		ids s = { run, ra, dec, new ObjectID(counter++) };
		db[s.getHash()] = s;
		return *s.id;
	}

	void commit() {
		if(!initialized) return;

		string idFile(System::workspace());
		idFile += (dbMode == ios::binary) ? "/lib/ids.dat" : "/lib/ids.txt";
		
		ofstream fraw(idFile.c_str(), (std::ios::openmode)(ios::trunc | ios::out | dbMode));
		binary_ostream f(fraw, dbMode);

		if(!f.good()) {
			THROW(EAny, string("Fatal error - cannot open ObjectID database file for writing : '") + idFile.c_str() + "'");
		}

		f << counter << "\n";
		for(idMap_t::iterator i = db.begin(); i != db.end(); i++) {
			ids &s = (*i).second;
//			char buf[1000];
//			sprintf(buf, "%4d %12.8f %12.8f", s.run, s.ra, s.dec);
//			f << buf << " " << *s.id << "\n";
			f << s.run << " " << s.ra << " " << s.dec << " " << *s.id << "\n";
		}
		DEBUG(verbose, "Committed " << db.size() << " ids to ID database, counter=" << counter);
	}

	~ObjectIDDatabase() {
		if(!initialized) return;

		if(dirty) commit();
		for(idMap_t::iterator i = db.begin(); i != db.end(); i++) {
			delete (*i).second.id;
		}
	}
};

static ObjectIDDatabase db;

// utility function for converting the database between two formats
// parameters : from/to : 1 -- binary, 0 -- ascii
// eg, convertdb(1, 0) converts binary to ascii database
void convertdb(int from, int to) { db.setBinary(from); db.initialize(); db.setBinary(to); db.setDirty(1); }

ObjectID ObjectID::generate(int run, double ra, double dec)
{
	db.initialize();
	return db.getId(run, ra, dec);
}

ObjectID::ObjectID(const int n)
{
	// 0 is a special value
	if(n == 0) { strcpy(id, "000000"); return; }

	// format ID in hexadecimal, 5 places
	// make the leading digit 's'+digit, because temporary designations in MPC
	// report cards must start with a letter
	id[5] = n & 0x0000000F;
	id[4] = (n & 0x000000F0) >> 4;
	id[3] = (n & 0x00000F00) >> 8;
	id[2] = (n & 0x0000F000) >> 12;
	id[1] = (n & 0x000F0000) >> 16;
	id[0] = (n & 0x00F00000) >> 20;
	for(int i = 5; i != 0; i--) {
		id[i] += id[i] > 9 ? -10 + 'a' : '0';
	}
	id[0] += 's';
	
	id[6] = 0; // make it a string
}


bool IdentRecord::write(ostream &f, const char sep) const
{
	MJD timeTAI            = time - peyton::ctn::TDTminusTAI;
	MJD calculated_timeTAI = calculated_time - peyton::ctn::TDTminusTAI;

	f << setiosflags(ios::fixed) << setprecision(18)
		// identification
		<< run << sep << camCol << sep << field << sep << id << sep << setprecision(3) << rowc << sep << colc << sep
		// catalog identification
		<< ast << sep << name << sep << numeration << sep << type << sep

		// SDSS measured
		<< setprecision(6)		// .01 second precision
		<< timeTAI << sep
		<< setprecision(6)		// 0.004 arcsec precision
		<< observed.ra/ctn::d2r << sep << observed.dec/ctn::d2r << sep
		<< mu/ctn::d2r << sep << nu/ctn::d2r << sep

		<< setprecision(3)
		<< u << sep << uErr << sep 
		<< g << sep << gErr << sep 
		<< r << sep << rErr << sep 
		<< i << sep << iErr << sep 
		<< z << sep << zErr << sep 
		<< setprecision(4)		// 0.4 arcsec precision
		<< vmu << sep << vmuErr << sep
		<< vnu << sep << vnuErr << sep
		<< vra << sep << vdec << sep

		// calculated
		<< setprecision(7)		// .01 second precision
		<< calculated_timeTAI << sep
		<< setprecision(6)		// 0.004 arcsec precision
		<< calculatedTDI.ra/ctn::d2r << sep << calculatedTDI.dec/ctn::d2r << sep << magc << sep << umagc << sep	// positions & magnitudes at time calculated_time
		<< setprecision(4)		// 0.4 arcsec precision
		<< vrac/ctn::d2r << sep << vdecc/ctn::d2r << sep			// velocities
		<< setprecision(6)		// 0.004 arcsec precision
		<< calculatedT.ra/ctn::d2r << sep << calculatedT.dec/ctn::d2r << sep	// position calculated at time of observation

		// identification errors
		<< setprecision(2)		// 0.01 arcsec precision
		<< raErr/ctn::s2r << sep << decErr/ctn::s2r << sep

		// catalog information
		<< setprecision(3)		// 3 decimal places
		<< H << sep << G << sep
		<< arc << sep 
		<< setprecision(5)		// 1 second precision
		<< epoch << sep
		<< setprecision(6)		// 6 decimal places
		<< a << sep << e << sep << inc/ctn::d2r << sep
		<< lan/ctn::d2r << sep << aop/ctn::d2r << sep << M/ctn::d2r << sep

		// heliocentric distance and phase
		<< setprecision(3)		// 3 decimal places
		<< R << sep << dist << sep << phase/ctn::d2r << sep

		// position in ecliptic coordinates, distance from opposition
		<< setprecision(6)		// 0.004 arcsec precision
		<< lambda/ctn::d2r << sep << beta/ctn::d2r << sep
		<< phi/ctn::d2r

		// EOL
		<< "\n";

	return true;
}

bool IdentRecord::read(istream &f)
{
	phi = 1E10;
	oid = 0;

	// jump over anything not starting with a digit after whitespace
	char buf[1000]; int dummy;
	f.getline(buf, 1000);
	if(sscanf(buf, "%d", &dummy) != 1) return false;

	istringstream s(buf);

	s
		// identification
		>> run >> camCol >> field >> id >> rowc >> colc
		// catalog identification
		>> ast >> name >> numeration >> type
		
		// SDSS measured
		>> time
		>> observed.ra >> observed.dec >> mu >> nu

		>> u >> uErr 
		>> g >> gErr 
		>> r >> rErr 
		>> i >> iErr 
		>> z >> zErr 
		>> vmu >> vmuErr
		>> vnu >> vnuErr
		>> vra >> vdec

		// calculated
		>> calculated_time >> calculatedTDI.ra >> calculatedTDI.dec >> magc >> umagc	// positions/mags at calculated_time
		>> vrac >> vdecc			// velocities
		>> calculatedT.ra >> calculatedT.dec	// position calculated at time of observation

		// identification errors
		>> raErr >> decErr

		// catalog information
		>> H >> G
		>> arc 
		>> epoch
		>> a >> e >> inc
		>> lan >> aop >> M

		// heliocentric distance and phase
		>> R >> dist >> phase

		// position in ecliptic coordinates, distance from opposition
		>> lambda >> beta
		>> phi;

//	cout << beta << " " << phi << "\n"; exit(-1);

	if(phi == 1E10) return false; // incomplete record

	// convert times from TAI to TDT
	time += peyton::ctn::TDTminusTAI;
	calculated_time += peyton::ctn::TDTminusTAI;

	observed.ra *= ctn::d2r; observed.dec *= ctn::d2r;
	calculatedTDI.ra *= ctn::d2r; calculatedTDI.dec *= ctn::d2r;
	calculatedT.ra *= ctn::d2r; calculatedT.dec *= ctn::d2r;
	vrac *= ctn::d2r; vdecc *= ctn::d2r;
	raErr *= ctn::s2r; decErr *= ctn::s2r;
	inc *= ctn::d2r; lan *= ctn::d2r; aop *= ctn::d2r; M *= ctn::d2r;
	phase *= ctn::d2r;
	lambda *= ctn::d2r; beta *= ctn::d2r;
	phi *= ctn::d2r;

	return true;
}

bool IdentRecord::header(ostream &f, const char sep)
{
	const int num_cols = 58;

	static char *header[num_cols] = {
		"run", "camCol", "field", "id", "rowc", "colc",
		"ast", "name", "numeration", "type",
		"time", "rao", "deco", "mu", "nu",
		"u", "uErr", "g", "gErr", "r", "rErr", "i", "iErr", "z", "zErr",
		"vmu", "vmuErr", "vnu", "vnuErr", "vra", "vdec",
		"timec", "racTDI", "deccTDI", "magc", "umagc",
		"vrac", "vdecc", 
		"racT", "deccT", 
		"raErr", "decErr", 
		"H", "G", 
		"arc", "epoch", "a", "e", "inc", "lan", "aop", "M", 
		"R", "dist", "phase",
		"lambda", "beta", "phi" };

	// header
	for(int i = 0; i != num_cols; i++) {
		if(i != 0) f << sep;
		f << header[i] << " ";
	}
	f << "\n";

	return true;
}
