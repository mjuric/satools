#include <stdio.h>

#include <iomanip>

#include <astro/system/preferences.h>
#include <astro/coordinates.h>

#include "observationcache.h"
#include "observationcalculator.h"

#include "orbfitlib.h"

#include <iostream>

using namespace peyton;
using namespace peyton::asteroids;
using namespace std;

peyton::system::Preferences pref;

main(int argc, char **argv)
{

	if(argc != 2 && argc != 6) {
		cout << "Usage: " << argv[0] << "  <cache_file> [<ra_lo> <dec_lo> <ra_hi> <dec_hi>]\n";
		return -1;
	}

	ObservationCache cache(argv[1], "r");

	std::vector<Observation> obs;
	cache.getObservations(obs, 0, cache.getObservationCount());

	double ralo, rahi, declo, dechi;
	bool limits = argc != 2;
	if(limits) {
		sscanf(argv[2], "%lf", &ralo);
		sscanf(argv[3], "%lf", &declo);
		sscanf(argv[4], "%lf", &rahi);
		sscanf(argv[5], "%lf", &dechi);

		while(ralo < 0) ralo += 360;
		while(rahi < 0) rahi += 360;
		while(ralo >= 360) ralo -= 360;
		while(rahi >= 360) rahi -= 360;
		
		ralo *= ctn::d2r;
		rahi *= ctn::d2r;
		declo *= ctn::d2r;
		dechi *= ctn::d2r;
	}

	cout << setprecision(18);

	for(int i = 0; i != obs.size(); i++) {
		Observation &o = obs[i];
		if(limits) {
			if(!Coordinates::inBox(o.ra, o.dec, ralo, declo, rahi, dechi)) continue;
		}
		cout
			<< i << " [" << o.name << "]" << "\t"
			<< o.R << "\t" << o.dist << "\t"
			<< o.id << "\t"
			<< o.t0 << "\t"
			<< o.ra / ctn::d2r << "\t"
			<< o.dec / ctn::d2r << "\t"
			<< o.mag << "\t"
			<< o.dra / ctn::d2r << "\t"
			<< o.ddec / ctn::d2r << "\n";
	}
}
