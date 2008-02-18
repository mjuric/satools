#include "version.h"

#include <iostream>
#include <fstream>
#include <iomanip>
#include <string>

#include <stdio.h>

#include <astro/system/preferences.h>
#include <astro/util.h>
#include <astro/coordinates.h>

#include "observationcalculator.h"
#include "observationcache.h"

using namespace std;

Preferences pref;

///////////////////////////////////////////////////////////////////////////////////

ofstream out;

void printMatch0(MJD time,
				double err,
				Asteroid *ast,
				Observation &obs, 
				SkyPoint &target,
				int run, int sloanID, 
				double rowv, double colw,
				double r_mag)
{
	char ras[100], decs[100];
	Util::formatCoord(ras, decs, obs.ra/ctn::d2r, obs.dec/ctn::d2r);

	double l, b;
	Coordinates::equecl(target.ra, target.dec, l, b);

	double phi;
	double antisun = Util::approxSunLongitude(time) - ctn::pi;
	if(antisun < 0) antisun += ctn::pi2;
	phi = l - antisun;
	if(phi > ctn::pi) phi -= ctn::pi2;

	out << setprecision(12)
		// identification
		<< run << "\t" << sloanID << "\t" << ast->id << "\t" << time << "\t"
		<< obs.name << "\t"
		<< err << "\t" << ast->arc << "\t"

		// calculated
		<< obs.ra/ctn::d2r << "\t" << obs.dec/ctn::d2r << "\t" << obs.mag << "\t" 
		<< obs.dra/ctn::d2r << "\t" << obs.ddec/ctn::d2r << "\t"

		// SDSS measured
		<< target.ra/ctn::d2r  << "\t" << target.dec/ctn::d2r << "\t" << r_mag << "\t" 
		<< rowv << "\t" << colw << "\t"

		// in ecliptic coordinates
		<< l/ctn::d2r << "\t" << b/ctn::d2r << "\t"
		<< phi/ctn::d2r

		// EOL
		<< "\n";
}

string sql(const double d)
{
	static char buf[500];
	sprintf(buf, "%15f", d);
	return string(buf);
}

string sql(const int d)
{
	static char buf[500];
	sprintf(buf, "%d", d);
	return string(buf);
}
/*
string sql(const char *c)
{
	static char buf[500];
	if(!strlen(c)) { return "NULL"; }

	buf[0] = '\'';
	int cnt = 1;
	while(*c) {
		buf[cnt] = *c; cnt++;
		if(*c == '\'') {buf[cnt] = '\''; cnt++; }
		c++;
	}
	buf[cnt] = '\'';
	return string(buf);
}*/

void printMatch(MJD time,
				double errRa, double errDec,
				Asteroid *ast,
				Observation &obs, 
				SkyPoint &target,
				int run, int sloanID, 
				double rowv, double colw,
				double r_mag)
{
	char ras[100], decs[100];
	Util::formatCoord(ras, decs, obs.ra/ctn::d2r, obs.dec/ctn::d2r);

	double l, b;
	Coordinates::equecl(target.ra, target.dec, l, b);

	double phi;
	double antisun = Util::approxSunLongitude(time) - ctn::pi;
	if(antisun < 0) antisun += ctn::pi2;
	phi = l - antisun;
	if(phi > ctn::pi) phi -= ctn::pi2;

/*	out << "insert into ident values("
		// identification
		<< sql(run) << "," << sql(sloanID) << "," << sql(ast->id) << "," << sql(time) << ","
//		<< sql(obs.name) << ","
		<< sql(errRa) << "," << sql(errDec) << ","

		// calculated
		<< sql(obs.ra/ctn::d2r) << "," << sql(obs.dec/ctn::d2r) << "," << sql(obs.mag) << "," 
		<< sql(obs.dra/ctn::d2r) << "," << sql(obs.ddec/ctn::d2r) << ","

		// SDSS measured
		<< sql(target.ra/ctn::d2r) << "," << sql(target.dec/ctn::d2r) << "," << sql(r_mag) << "," 
		<< sql(rowv) << "," << sql(colw) << ","

		// in ecliptic coordinates
		<< sql(l/ctn::d2r) << "," << sql(b/ctn::d2r) << ","
		<< sql(phi/ctn::d2r)

		// EOL
		<< ");\n";
*/
	out
		// identification
		<< run << "\t" << sloanID << "\t" << ast->id << "\t" << time << "\t"
		<< errRa << "\t" << errDec << "\t"

		// calculated
		<< obs.ra/ctn::d2r << "\t" << obs.dec/ctn::d2r << "\t" << obs.mag << "\t" 
		<< obs.dra/ctn::d2r << "\t" << obs.ddec/ctn::d2r << "\t"

		// SDSS measured
		<< target.ra/ctn::d2r << "\t" << target.dec/ctn::d2r << "\t" << r_mag << "\t" 
		<< rowv << "\t" << colw << "\t"

		// in ecliptic coordinates
		<< l/ctn::d2r << "\t" << b/ctn::d2r << "\t"
		<< phi/ctn::d2r

		// EOL
		<< "\n";
}

int main(int argc, char* argv[])
{
	PRINT_VERSION_IF_ASKED(argc, argv);

	if(argc != 6) {
		cout << "Usage: " << argv[0] << " <output_file> <radius> <input_file> <catalog> <cache>\n";
		cout << "Catalog must be in NATIVE format\n";
		return -1;
	}

	out.open(argv[1]);
	const double matchRadius = atof(argv[2]);
	ifstream f(argv[3]);
	Catalog *cat = Catalog::open(argv[4], "NATIVE");
	ObservationCache cache(argv[5], "r");

	cout << "Radius  : " << matchRadius << "\"\n";
	cout << "Catalog : " << argv[4] << "\n";
	cout << "Cache   : " << argv[5] << "\n";

	ObservationCalculator oc;
	vector<Asteroid> o;
	vector<Observation> obsv;
	int i;

	// unidentified observation dummy object
	Observation unmached;
	unmached.name[0] = 0; unmached.ra = unmached.dec = unmached.ddec = unmached.dra = unmached.mag = 0;
	Asteroid unmachedAst;

	// header
	out << "#run\tsloanId\tastorbId\ttime\tname\terrRa\terrDec\tra\tdec\tmag\tdra\tddec\ttra\ttswc\ttmag\ttdra\ttddec\tl\tb\tphi\n";
	cout << setiosflags( ios::fixed );

	MJD t2;
	SkyPoint target;
	double v, rowv, colw;
	int sloanID, run;
	double r_mag;
	int plus = 0, minus = 0;

	while(!f.eof()) {
		f >> r_mag >> run >> sloanID >> t2 >> v >> rowv >> colw >> target.ra >> target.dec;
		target.ra *= ctn::d2r; target.dec *= ctn::d2r;

		double err = 1E5;

		if(cache.getCandidates(obsv, t2, target, .2*ctn::d2r) > 0) {
			// load Asteroids for all candidates
			int *ids = new int[obsv.size()];
			for(i = 0; i != obsv.size(); i++) ids[i] = obsv[i].id;
			cat->read(o, ids, obsv.size());
			delete [] ids;

			// calculate exact position
			oc.calculateObservations(obsv, t2, o, ObsFlags::pos | ObsFlags::vel, CalcFlags::twoBody);

			// find best
			int bestMatch;
			for(i = 0; i != obsv.size(); i++) {
				double d = target.distance(SkyPoint(obsv[i].ra, obsv[i].dec));
				if(d < err) { bestMatch = i; err = d; }
			}

			// show our best candidate
			err /= ctn::s2r;
			if(err < matchRadius) {
				double errRa, errDec;
				errRa = (obsv[bestMatch].ra - target.ra) / ctn::s2r;
				errDec = (obsv[bestMatch].dec - target.dec) / ctn::s2r;
				printMatch(t2, errRa, errDec, &o[bestMatch], obsv[bestMatch], target, run, sloanID, rowv, colw, r_mag);
				plus++;
			}
		}

		// absolutely nothing was found
		if(err >= matchRadius) {
			printMatch(t2, 0, 0, &unmachedAst, unmached, target, run, sloanID, rowv, colw, r_mag);
			minus++;
		}

		// progress info
		if((plus + minus) % 50 == 0) {
			cout << "   [" << plus << "/" << minus << "/" << plus+minus << " : " << setprecision(1) << r_mag << "m]\n";
		}
		cout << "#"; cout.flush();
	}
	cout << "\nTotals (+/-/total) : " << plus << "/" << minus << "/" << plus+minus << "\n";

	delete cat;

	return 0;
}
