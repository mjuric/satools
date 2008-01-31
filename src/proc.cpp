#include "identrecord.h"

#include <astro/system/preferences.h>
#include <astro/constants.h>

#include <fstream>
#include <math.h>
#include <stdio.h>

#include <list>
#include <iostream>
#include <iterator>

using namespace std;
using namespace peyton;

peyton::system::Preferences pref;

typedef list<IdentRecord> Idents;
typedef Idents::iterator ii;

/*
#define CONSTRAIN_TO(lst, low, elem, hi) \
	for(ii i = lst.begin(); i != lst.end(); ) { ii j = i++; if(!(low <= (*j).elem && (*j).elem <= hi)) lst.erase(j); } \
	cout << #lst << " : " << lst.size() << " remained after " #low " <= " #elem " <= " #hi "\n";
*/
#define WRITE(lst, file) copy(lst.begin(), lst.end(), ostream_iterator<IdentRecord>(file, ""));

#include "analysis.h"

double johnsonV(double r, double g)
{
	double r0 = r, g0 = g;
	r -= .11; g += .12;

	double V = .4666667*g + .533333*r;
	double B = 1.419048*g - .419048*r;

	return V;
}

void dumpMagnitudes(Idents &det)
{
	for(ii i = det.begin(); i != det.end(); i++) {
		IdentRecord &r = (*i);
		double v = johnsonV(r.r, r.g);
		cout << r.numeration << " " << r.name << " " << r.H << " " << r.G << " "
			<< r.magc << " " << v << " " << v - r.magc
			<< " " << r.run << " " << r.camCol << " " << r.field << " " << r.id
			<< " " << r.r << " " << r.g << "\n";
	}
}

main(int argc, char *argv[])
{
	if(argc != 2) { cout << "Usage : " << argv[0] << " <run>\n"; return -1; }
	char sin[200], sdet[200], sundet[200], sall[200];
	sprintf(sin, "../%s.a.out", argv[1]);
	sprintf(sall, "../%s.a.all", argv[1]);
	sprintf(sdet, "../%s.a.det", argv[1]);
	sprintf(sundet, "../%s.a.undet", argv[1]);

	ifstream in(sin);
	ofstream oall(sall);
	ofstream odet(sdet);
	ofstream oundet(sundet);

	IdentRecord::header(odet);
	IdentRecord::header(oundet);

	IdentRecord r;

	Idents all, det, undet;

	while(!in.eof()) { if(!r.read(in)) continue; all.push_back(r); }

	cout << "Loaded " << all.size() << " objects\n";

	CONSTRAIN_TO(all, 14, magc, 21.5);
	CONSTRAIN_TO(all, -15, phi/ctn::d2r, 15);
//	CONSTRAIN_TO(all, 300, arc, 10000000);

	det = undet = all;

	CONSTRAIN_TO(det, 1, camCol, 6);
	CONSTRAIN_TO(undet, 0, camCol, 0);

	WRITE(all, oall);
	WRITE(det, odet);
	WRITE(undet, oundet);

//	dumpMagnitudes(det);
}
