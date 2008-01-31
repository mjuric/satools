#include <iostream>
#include <stdlib.h>
#include <astro/photometry.h>
#include <astro/catalogs/catalog.h>
#include <astro/time.h>
#include <astro/system/preferences.h>
#include <iomanip>

#include "observationcalculator.h"

Preferences pref;

int main(int argc, char *argv[])
{
	if(argc < 7) {
		cout << "Usage: " << argv[0] << " <g-r> <r> <mon> <day> <h> <name>";
		return -1;
	}
	
	double g = atof(argv[1]);
	double r = atof(argv[2]);

	g += r;

	double B, V;
	Photometry::johnson(V, B, r, g);
	
	ObservationCalculator oc;
	Catalog *cat = Catalog::open("94.obj", "NATIVE");

	int y = 1997;
	int mon = atoi(argv[3]);
	int day = atoi(argv[4]);
	double h = atof(argv[5]);
	char *name = argv[6];

	MJD t = Time::calToMJD(y, mon, day, h);

	cerr << setiosflags(ios::fixed) << setprecision(5);

	cerr << "Name : " << name << "\n";
	cerr << "Time : " << 1997 << " " << mon << " " << day << " " << h << " [" << t << "]\n";
	cerr << "Cat  : " << cat->recordCount() << "\n";

	Asteroid a;
	Observation obs;
	if(cat->read(a, name) == 0) {
		oc.calculateObservation(obs, t, a, ObsFlags::pos | ObsFlags::vel);
	} else {
		cout << "Cannot find " << name << " in catalog.\n";
	}

	cout << name << " " << V << "  " << B << " " << obs.mag << " " << V - obs.mag << "\n";
}
