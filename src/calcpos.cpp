#include <astro/system/preferences.h>
#include <astro/system/error.h>
#include <astro/system.h>
#include <astro/exceptions.h>
#include <astro/util.h>
#include <astro/math.h>
#include <astro/coordinates.h>

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
using namespace peyton::math;

peyton::system::Preferences pref;
Catalog *cat = NULL;

inline Radians dist(Radians a, Radians b) {
	Radians d = a - b;
	Radians d2 = d > 1. ? d - ctn::pi2 : ( d < -1. ? d + ctn::pi2 : d);
	//if (d != d2) { cerr << d << " " << d2 << "\n"; }
	return d2;
}

void calculate_position(double mjd, const std::string &desig)
{
	static ObservationCalculator oc;
	static const double vdt = 600./(3600.*24.);
	Asteroid ast;
	Observation o, o1;

	// locate asteroid
	if(cat->read(ast, desig.c_str()) == -1)
	{
		//std::cerr << "[" << desig << "]: No asteroid of such name in the catalog\n";
		o.ra = o.dec = o1.ra = o1.dec = 0;
	}
	else
	{
		// calculate position and velocity vectors
		oc.calculateObservation(o, mjd, ast, ObsFlags::pos | ObsFlags::vel);
		oc.calculateObservation(o1, mjd+vdt, ast, ObsFlags::pos);
	}

	// print out
	std::cout
		<< setprecision(12) << mjd << "\t"
		<< desig << "\t"
		<< setprecision(10) << o.ra / ctn::d2r << "\t"
		<< setprecision(10) << o.dec / ctn::d2r << "\t"
		<< setprecision(7) << dist(o1.ra, o.ra) / vdt / ctn::d2r << "\t"
		<< setprecision(7) << dist(o1.dec, o.dec) / vdt / ctn::d2r << "\n"
		;
}

main(int argc, char **argv)
{
	try {
		if(argc != 3 && argc != 5) {
			cout << "Usage 1: " << argv[0] << " <desig> <mjd> <catalog> <ASTORB2|NATIVE>\n";
			cout << "Usage 2: " << argv[0] << " <catalog> <ASTORB2|NATIVE>\n";
			cout << "  Calculates the (ra, dec) of an asteroid, given time, designation and orbit catalog.\n";
			cout << "  If used in second form, desig and mjd are read from stdin.\n";
			return -1;
		}

		const char *ws = System::workspace();
		char scat[1000];
		
		std::string catname, cattype, desig; double mjd = 0;
		if(argc == 3)
		{
			catname = argv[1];
			cattype = argv[2];
		}
		else
		{
			mjd = atof(argv[1]);
			desig = argv[2];
			catname = argv[3];
			cattype = argv[4];
		}

		string format = Util::toupper(cattype);
		if(format == "ASTORB2") {
			sprintf(scat, "%s/catalogs/astorb.dat.%s", ws, catname.c_str());
		} else if(format == "NATIVE") {
			sprintf(scat, "%s/tmp/propagated/%s.obj", ws, catname.c_str());
		} else {
			THROW(EAny, "Unknown catalog format [" + format + "]");
		}

		OrbfitPropagLibrary::initialize();

		cerr << "Catalog   : " << scat << "\n";

		cat = Catalog::open(scat, format.c_str());
		cerr << "Objects   : " << cat->recordCount() << "\n";
		cerr << "\n";

		ObservationCalculator oc;

		if(!desig.empty())
		{
			calculate_position(mjd, desig);
		}
		else
		{
			cin >> mjd;
			while(cin >> desig)
			{
				calculate_position(mjd, desig);
				cin >> mjd;
			}
		}

	} catch(EAny &e) {
		e.print();
		exit(-1);
	}

	return 0;
}
