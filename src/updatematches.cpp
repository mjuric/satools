#include "version.h"

#include <astro/system/preferences.h>
#include <astro/coordinates.h>

#include "sarecord.h"

#include <vector>

#include <unistd.h>
#include <time.h>
#include <stdio.h>

#include <iostream>

using namespace std;
using namespace peyton;
using namespace peyton::asteroids;

peyton::system::Preferences pref;

struct ms {
	double ra1, dec1; char desig1[7];
	double ra2, dec2; char desig2[7];
};

vector<SARecord> db;
vector<ms> matches;

int main(int argc, char *argv[])
{
	PRINT_VERSION_IF_ASKED(argc, argv);

	if(argc != 5) {
		cout << "Usage : " << argv[0] << " <db.sdss> <run1> <run2> <positions.match>\n";
		return -1;
	}

	int run1 = atoi(argv[2]);
	int run2 = atoi(argv[3]);

	char *dbfile = argv[1];
	char *mfile = argv[4];

	char tmpfile[1000], bkpfile[1000];
	sprintf(tmpfile, "%s.tmp", dbfile);
	sprintf(bkpfile, "%s.%d.mupdate", dbfile, time(NULL));

	// load the database file
	{
		ifstream f(dbfile);
		SARecord r;
		char buf[1000];

		while(!f.eof()) {
			f.getline(buf, 1000);
			if(!r.parse(buf)) continue;

			db.push_back(r);
		}
	}

	// load matches file
	int nomatch = 0;
	{
		ifstream f(mfile);
		ms m;
		while(!f.eof()) {
			m.ra1 = -1; m.desig1[0] = m.desig2[0] = 0;
			f >> m.ra1 >> m.dec1 >> m.ra2 >> m.dec2;
			if(m.ra1 == -1) continue;
			RAD(m.ra1); RAD(m.dec1); RAD(m.ra2); RAD(m.dec2);

			for(int i = 0; i != db.size(); i++) {
				SARecord &r = db[i];
				if(r.run == run1 && Coordinates::distance(r.p.ra, r.p.dec, m.ra1, m.dec1) < 1*ctn::s2r) {
					strcpy(m.desig1, (const char *)r.oid);
				}
				if(r.run == run2 && Coordinates::distance(r.p.ra, r.p.dec, m.ra2, m.dec2) < 1*ctn::s2r) {
					strcpy(m.desig2, (const char *)r.oid);
				}
			}

			if(m.desig1[0] && m.desig2[0]) {
				matches.push_back(m);
			} else {
				nomatch++;
			}
		}
	}

	cout << db.size() << " asteroids in " << dbfile << "\n";
	cout << matches.size() << " valid matches in " << mfile << ", " << nomatch << " matches rejected\n";

	// do the matchup
	int rep = 0;
	{
		ofstream out(tmpfile);
		char buf[1000];
		for(int i = 0; i != db.size(); i++) {
			SARecord &r = db[i];
			if(r.run == run2) {
				int j;
				for(j = 0; j != matches.size(); j++) {
					if(!strcmp(matches[j].desig2, (const char *)r.oid) && r.detName[0] == '-') {
						strcpy(r.detName, matches[j].desig1);
						rep++;
						break;
					}
				}
			}

			out << r.toString(buf) << "\n";
		}
	}

	// rename files
	rename(dbfile, bkpfile);
	rename(tmpfile, dbfile);

	cout << rep << " replacements made, old file backed up as " << bkpfile << "\n";
	cout.flush();

	return 0;
}
