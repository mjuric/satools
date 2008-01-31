#include "identrecord.h"

#include <astro/system/preferences.h>
#include <astro/photometry.h>
#include <astro/constants.h>

#include <fstream>
#include <math.h>
#include <stdio.h>

#include <math.h>

#include <list>

using namespace std;

Preferences pref;

class IdentExt : public IdentRecord {
public:
	double V, B;
};

typedef list<IdentExt> Idents;
typedef Idents::iterator ii;

#define WRITE(lst, f) { ofstream file(f); copy(lst.begin(), lst.end(), ostream_iterator<IdentRecord>(file, "")); }

#include "analysis.h"

main(int argc, char *argv[])
{
//	if(argc != 2) { cout << "Usage : " << argv[0] << " <file.a.out>\n"; return -1; }

	ifstream in("../all.a.out"/*argv[1]*/);

	IdentExt r;

	Idents all, det, undet;

	while(!in.eof()) { if(!r.read(in)) continue; Photometry::johnson(r.V, r.B, r.r, r.g); /*r.V = rint(r.V*10)/10*/; all.push_back(r); }

	cout << "Loaded " << all.size() << " objects\n";

	CONSTRAIN_TO(all, 14, magc, 21.5);
	CONSTRAIN_TO(all, -15, phi/ctn::d2r, 15);

	//
	//   H distribution of detected asteroids
	//
	Bins bins;
	CUMULATIVE_HISTOGRAM(all, H, bins, 10, 20, 15);

	WRITE_HIST(bins, "h_cum_histo.a.txt");

	//
	//   Magnitude distribution
	//

	int b = 65;

	// all asteroids - calculated and observed V
	HISTOGRAM(all, magc, bins, 12, 25, b);
	WRITE_HIST(bins, "mag_histo.all.a.txt");
	HISTOGRAM(all, V, bins, 12, 25, b);
	WRITE_HIST(bins, "mag_histo.all.s.txt");

	// detected asteroids - calculated and observed V
	CONSTRAIN_TO(all, 1, camCol, 6);
	HISTOGRAM(all, magc, bins, 12, 25, b);
	WRITE_HIST(bins, "mag_histo.det.a.txt");
	HISTOGRAM(all, V, bins, 12, 25, b);
	WRITE_HIST(bins, "mag_histo.det.s.txt");

	// detected asteroids, H <= 14 - calculated and observed V
	CONSTRAIN_TO(all, 0, H, 14);
	HISTOGRAM(all, magc, bins, 12, 25, b);
	WRITE_HIST(bins, "mag_histo.h14.a.txt");
	HISTOGRAM(all, V, bins, 12, 25, b);
	WRITE_HIST(bins, "mag_histo.h14.s.txt");

}
