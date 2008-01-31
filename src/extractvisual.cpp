#include <stdio.h>

#include <iomanip>
#include <fstream>

#include <astro/system/preferences.h>
#include <astro/coordinates.h>

#include "observationcache.h"
#include "observationcalculator.h"

#include "orbfitlib.h"

Preferences pref;

main(int argc, char **argv)
{

	if(argc != 4) {
		cout << "Description: Extracts lists of asteroids from caches of TDI sky. "
				"List files are in old .y format.\n";
		cout << "Usage: " << argv[0] << " <out_file> <list_file> <cache_file>\n";
		return -1;
	}

	ObservationCache cache(argv[3], "r");

	std::vector<Observation> obs;
	cache.getObservations(obs, 0, cache.getObservationCount());

	ifstream mf(argv[2]);
	vector<string> a;

	char buf[1001], name[100];
	while(!mf.eof()) {
		mf.getline(buf, 1000);

		if(sscanf(buf, "%*d %*f %*f %*f %*f %*f %*f %*f %*f %*d %*f %s", name) != 1) { continue; }
		cout << name << "\n";

		a.push_back(name);
	}

	cout << limits.size() << " mask areas loaded.\n";
	for(int i = 0; i != a.size(); i++) {
		cout << a[i] << "\n";
	}

	cout << setprecision(18);

	ObservationCache out(argv[1], "w");
	ObservationCache::Header h = cache.getHeader();
	h.len = 0;
	out.setHeader(h);

	for (int i = 0; i != obs.size(); i++) {
		Observation &o = obs[i];
		int j;

		// check if it's one of our asteroids
		for (j = 0; j != a.size(); j++) {
			string name = a[j];
			if(!strcmp(name, o.name)) {
				// write this observation
				out.append(&o, 1);
				cout << "#";
			};
		}
	}
}
