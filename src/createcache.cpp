#include <astro/system/preferences.h>
#include <astro/system/error.h>
#include <astro/system.h>
#include <astro/sdss/rungeometry.h>

#include "observationcache.h"
#include "observationcalculator.h"

#include "orbfitlib.h"

#include <iostream>

using namespace std;
using namespace peyton;
using namespace peyton::asteroids;
using namespace peyton::sdss;

peyton::system::Preferences pref;

main(int argc, char **argv)
{

	if(argc != 4) {
		cout << "Usage: " << argv[0] << " <cache_file> <run> <catalog>\n";
		cout << "Catalog must be in NATIVE format\n";
		return -1;
	}
	
	int run = atoi(argv[2]);

	// Load run
	RunGeometryDB geoms;
	RunGeometry geom;
	geoms.getGeometry(run, geom);
	Radians ra0 = geom.ra;

	const char *ws = System::workspace();
	char scat[1000], sout[1000];
	sprintf(scat, "%s/tmp/propagated/%s.obj", ws, argv[3]);
	sprintf(sout, "%s/tmp/skies/%s.cache", ws, argv[1]);

	OrbfitPropagLibrary::initialize();

	cout << "Run     : " << run << "\n";
	cout << "Time    : " << geom.tstart << "\n";
	cout << "Ra      : " << geom.ra / ctn::d2r << "\n";
	cout << "Catalog : " << scat << "\n";
	cout << "Cache   : " << sout << "\n";

	ObservationCache cache(sout, "w");
	Catalog *cat = Catalog::open(scat, "NATIVE");
//	Catalog *cat = Catalog::open("/disk4/mjuric/projects/satools/workspace/catalogs/astorb.dat.20070910", "ASTORB2");

//	MJD t = atof(argv[1]);
//	t += 27.04/(24.*3600.);
//	Radians ra0 = atof(argv[2])*ctn::d2r;

	ObservationCalculator oc;
	cache.create(geom, cat, &oc);
}
