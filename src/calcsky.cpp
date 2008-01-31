#include <astro/system/preferences.h>
#include <astro/system/error.h>
#include <astro/system.h>
#include <astro/exceptions.h>
#include <astro/util.h>

#include "observationcache.h"
#include "observationcalculator.h"

#include "orbfitlib.h"

#include <iomanip>
#include <fstream>
#include <iostream>

using namespace std;
using namespace peyton;
using namespace peyton::exceptions;
using namespace peyton::asteroids;
using namespace peyton::sdss;

peyton::system::Preferences pref;

inline Radians dist(Radians a, Radians b) {
	Radians d = a - b;
	Radians d2 = d > 1. ? d - ctn::pi2 : ( d < -1. ? d + ctn::pi2 : d);
	//if (d != d2) { cerr << d << " " << d2 << "\n"; }
	return d2;
}

main(int argc, char **argv)
{
	try {
		if(argc != 5) {
			cout << "Usage: " << argv[0] << " <output_file.txt> <mjd> <catalog> <ASTORB2|NATIVE>\n";
			cout << "Catalog must be in ASTORB format\n";
			return -1;
		}
	
		const char *ws = System::workspace();
		char scat[1000], sout[1000];
		sprintf(sout, "%s", argv[1]);
		MJD d0 = atof(argv[2]);
		MJD d1 = d0 + 600./(3600.*24.);

		string format = Util::toupper(argv[4]);
		if(format == "ASTORB2") {
			sprintf(scat, "%s/catalogs/astorb.dat.%s", ws, argv[3]);
		} else if(format == "NATIVE") {
			sprintf(scat, "%s/tmp/propagated/%s.obj", ws, argv[3]);
		} else {
			THROW(EAny, "Unknown catalog format [" + format);
		}

		OrbfitPropagLibrary::initialize();

		cout << "Time    : " << d0 << "\n";
		cout << "Catalog : " << scat << "\n";
		cout << "Cache   : " << sout << "\n";

		Catalog *cat = Catalog::open(scat, format.c_str());
		cout << "Objects : " << cat->recordCount() << "\n";

		cout.flush();
		
		ObservationCalculator oc;
		const int blockSize = 1000;
		ofstream out(sout);

		int recordCount = cat->recordCount();
		out << setprecision(10);

		// calculate sky
		vector<Asteroid> obj;
		vector<Observation> obsv, obsv1;
		int i = 0, prog = 0;
		while(i < recordCount) {
			int read = cat->read(obj, i, i+blockSize);

			// calculate positions for the time of TDI scan start (0th approximation)
			oc.calculateObservations(obsv, d0, obj, ObsFlags::pos | ObsFlags::vel, CalcFlags::twoBody);
			oc.calculateObservations(obsv1, d1, obj, ObsFlags::pos, CalcFlags::twoBody);

			// output the result
			for(int j = 0; j != obsv.size(); j++) {
				Observation &o = obsv[j];
				Asteroid &a = obj[j];
				char *c = o.name;
			//	cout << j << " " << c << "\n";
			//	continue;
				while(*c) {
					if(*c == ' ') *c = '_';
					c++;
				}
				out
					<< o.id << "\t"
					<< o.name << "\t"
					<< o.t0 << "\t"
					<< o.ra / ctn::d2r << "\t"
					<< o.dec / ctn::d2r << "\t"
					<< o.mag << "\t"
				//	<< o.dra / ctn::d2r << "\t"
				//	<< o.ddec / ctn::d2r << "\t"
					<< dist(o.ra, obsv1[j].ra) / (d0 - d1) / ctn::d2r << "\t"
					<< dist(o.dec, obsv1[j].dec) / (d0 - d1) / ctn::d2r << "\t"
					<< a.h << "\t"
					<< o.R << "\t" << o.dist << "\t"
					<< a.arc << "\t" << a.t0;
				for(int i = 0; i != 2; i++) out << "\t" << a.elements[i];
				for(int i = 2; i != 6; i++) out << "\t" << a.elements[i]/ctn::d2r;
//				out	<< "\t"
//					<< (o.ra - obsv1[j].ra) / (d0 - d1) / ctn::d2r << "\t"
//					<< (o.dec - obsv1[j].dec) / (d0 - d1) / ctn::d2r;

				out
					<< "\n";
			}

			i += read;
			cerr << "#"; cerr.flush();
		}
	} catch(EAny &e) {
		e.print();
		exit(-1);
	}

	return 0;
}
