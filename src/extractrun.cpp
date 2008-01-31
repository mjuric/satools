#include <stdio.h>

#include <iomanip>
#include <fstream>

#include <astro/system/preferences.h>
#include <astro/coordinates.h>

#include "observationcache.h"
#include "observationcalculator.h"

#include "orbfitlib.h"

#include <iostream>

using namespace peyton;
using namespace peyton::coordinates;
using namespace peyton::asteroids;
using namespace peyton::sdss;
using namespace std;

peyton::system::Preferences pref;

struct Limits {
	Radians ralo, rahi, declo, dechi;
};

main(int argc, char **argv)
{

	if(argc != 4) {
		cout << "Description: Extracts parts of the sky, given in maskfiles, from caches of TDI sky. "
				"Map files are in format <ra_lo> <ra_hi> <dec_lo> <dec_hi> per line, with arbitrary number of lines.\n";
		cout << "Usage: " << argv[0] << " <out_file> <mask_file> <cache_file>\n";
		return -1;
	}

	ObservationCache cache(argv[3], "r");

	std::vector<Observation> obs;
	cache.getObservations(obs, 0, cache.getObservationCount());

	ifstream mf(argv[2]);
	Limits l;
	vector<Limits> limits;

	char buf[1001];
	while(!mf.eof()) {
		mf.getline(buf, 1000);

//		cout << buf << "\n";
		if(sscanf(buf, "%lf %lf %lf %lf", &l.ralo, &l.rahi, &l.declo, &l.dechi) != 4) { continue; }

		while(l.ralo < 0) l.ralo += 360;
		while(l.rahi < 0) l.rahi += 360;
		while(l.ralo >= 360) l.ralo -= 360;
		while(l.rahi >= 360) l.rahi -= 360;
		
		l.ralo *= ctn::d2r;
		l.rahi *= ctn::d2r;
		l.declo *= ctn::d2r;
		l.dechi *= ctn::d2r;

		limits.push_back(l);
	}

	cout << limits.size() << " mask areas loaded.\n";
/*	for(int i = 0; i != limits.size(); i++) {
		l = limits[i];
		cout << l.ralo/ctn::d2r << " " << l.rahi/ctn::d2r << " " << l.declo/ctn::d2r << " " << l.dechi/ctn::d2r << "\n";
	}
*/
	cout << setprecision(18);

	ObservationCache out(argv[1], "w");
	ObservationCache::Header h = cache.getHeader();
	h.len = 0;
	out.setHeader(h);

	for (int i = 0; i != obs.size(); i++) {
		Observation &o = obs[i];
		int j;

		// check if it's in the masked area
		for (j = 0; j != limits.size(); j++) {
			Limits &l = limits[j];
			if (Coordinates::inBox(o.ra, o.dec, l.ralo, l.declo, l.rahi, l.dechi)) break;
		}
		if (j == limits.size()) continue;

		// write this observation
//		cout << "#";
		out.append(&o, 1);
	}
}
