#include "version.h"

#include "safile.h"

#include <astro/system/log.h>
#include <astro/system/preferences.h>
#include <astro/constants.h>
#include <astro/util.h>

#include <stdio.h>
#include <iostream>

using namespace peyton;
using namespace std;
using namespace peyton::coordinates;
using namespace peyton::system;

peyton::system::Preferences pref;

using namespace std;
int main(int argc, char *argv[])
{
	PRINT_VERSION_IF_ASKED(argc, argv);

	if(argc != 4) {
		cout << "Description: Extract designation of object at the given position\n";
		cout << "Usage      : " << argv[0] << " <mpec.sdss file> <ra> <dec>\n";
		return -1;
	}

	SAFile mpec(argv[1]);
	if(mpec.problem) { Log::write("Problem loading MPEC.SDSS file. Bailing out."); return -1; }
	cout << mpec.size() << " records loaded\n";

	double ra = atof(argv[2])*ctn::d2r;
	double dec = atof(argv[3])*ctn::d2r;
	SkyPoint p(ra, dec);

	double radius = 1*ctn::s2r;

	// find objects withing 1 arcsec
	int cnt = 0;
	for(SAFile::ii i = mpec.begin(); i != mpec.end(); i++) {
		if((*i).p.distance(p) < radius) {
			(*i).prettyPrint(cout);
			cout << "\n";
			cnt++;
		}
	}
	if(cnt == 0) cout << "Nothing found.\n";
	else cout << cnt << " objects found.\n";
}
