#include "version.h"

#include <stdlib.h>

#include <fstream>

#include <astro/system/preferences.h>

#include "sloanobservation.h"

using namespace std;
using namespace peyton;
using namespace peyton::coordinates;


peyton::system::Preferences pref;

int main(int argc, char *argv[])
{
	PRINT_VERSION_IF_ASKED(argc, argv);

	if(argc != 3) {
		cout << "Description: Find nearest object in all.in\n";
		cout << "Usage: " << argv[0] << " <ra> <dec>\n";
		return -1;
	}

	SkyPoint p;
	p.ra = atof(argv[1]) * ctn::d2r;
	p.dec = atof(argv[2]) * ctn::d2r;

	Radians dist = 1E10;

	ifstream f("all.in");
	SloanObservation sobs, best;
	while(!f.eof()) {
		char buf[1000];
		f.getline(buf, 1000);
		if(!sobs.read(buf)) break;
		
		Radians d = p.distance(sobs.p);
		if(d < dist) {
			best = sobs;
			dist = d;
		}
	}
	
	cout << "Best match: " << best.run << " " << best.camCol << " " << best.field << " " << best.id << " : "
		<< best.p.ra/ctn::d2r << " " << best.p.dec/ctn::d2r << " : " << dist/ctn::s2r << "\n";
}
