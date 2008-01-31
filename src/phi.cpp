#include <astro/system/preferences.h>

#include <astro/coordinates.h>
#include <astro/util.h>

#include "observationcache.h"
#include "observationcalculator.h"

#include "orbfitlib.h"

#include <stdio.h>

#include <iostream>

using namespace peyton;
using namespace peyton::coordinates;
using namespace std;

peyton::system::Preferences pref;

main(int argc, char **argv)
{

	if(argc != 4) {
		cout << "Usage: " << argv[0] << "  <t0> <ra> <dec>\n";
		cout << "The program outputs phi (distance from antisun)\n";
		return -1;
	}

	double time;
	SkyPoint target;
	sscanf(argv[1], "%lf", &time);
	sscanf(argv[2], "%lf", &target.ra);
	sscanf(argv[3], "%lf", &target.dec);

	target.ra *= ctn::d2r;
	target.dec *= ctn::d2r;

	double l, b;
	Coordinates::equecl(target.ra, target.dec, l, b);

	double phi;
	double antisun = util::approxSunLongitude(time) - ctn::pi;
	if(antisun < 0) antisun += ctn::pi2;
	phi = l - antisun;
	if(phi > ctn::pi) phi -= ctn::pi2;

	cout << phi / ctn::d2r;
}
