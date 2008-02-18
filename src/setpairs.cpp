#include "version.h"

#include "mpecfile.h"

#include <astro/system/error.h>
#include <astro/system/preferences.h>
#include <astro/constants.h>
#include <astro/util.h>

#include <stdio.h>
#include <iomanip>

Preferences pref;

using namespace std;
int main(int argc, char *argv[])
{
	PRINT_VERSION_IF_ASKED(argc, argv);

	if(argc != 4) {
		cout << "Description : Takes a list of pairs from two runs gives them the same designation (the one from first run)\n";
		cout << "Usage       : " << argv[0] << " <run1.sdss> <run2.sdss> <list>\n";
		cout << "            : The list is in <ra1> <dec1> <ra2> <dec2> format\n";
		return -1;
	}

	MPECFile m1(argv[1]);
	if(m1.problem) { Error::error(-1, "Problem loading 1st MPEC.SDSS file. Bailing out."); return -1; }
	cout << m1.size() << " records loaded\n";

	MPECFile m2(argv[2]);
	if(m2.problem) { Error::error(-1, "Problem loading 2nd MPEC.SDSS file. Bailing out."); return -1; }
	cout << m2.size() << " records loaded\n";

	ifstream f(argv[3]);

	cout << setiosflags(ios::fixed);

	SkyPoint p1, p2;
	while(!f.eof()) {
		f >> p1.ra >> p1.dec >> p2.ra >> p2.dec;
		if(f.eof()) break;
		
		p1.toRad(); p2.toRad();
		
		// find records
		MPECRecord *a = m1.at(p1), *b = m2.at(p2);
		if(a == NULL) { cout << setprecision(6) << p1.ra/ctn::d2r << " " << p1.dec/ctn::d2r << " not present in first MPEC\n"; continue; }
		if(b == NULL) { cout << setprecision(6) << p2.ra/ctn::d2r << " " << p2.dec/ctn::d2r << " not present in second MPEC\n"; continue; }
		
		// modify oids and discovery asterix
		ObjectID old = b->oid;
		cout << old << " -> ";
		
		// find other asteroids with the same name (other band) and redo the modification
		for(MPECFile::ii i = m2.begin(); i != m2.end(); i++) {
			MPECRecord *b = &(*i);
			if(b->oid == old) {
				b->oid = a->oid;
				b->asterix = ' ';
				strcpy(b->name, a->oid);
				cout << b->name << " ";
			}
		}
		cout << "\n";
	}

	m2.commit();
}
