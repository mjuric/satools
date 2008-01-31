#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>

#include <list>
#include <vector>
#include <algorithm>

#include <math.h>

#include <astro/coordinates.h>
#include <astro/system/preferences.h>
#include <astro/constants.h>

#include "sloanobservation.h"

using namespace std;
using namespace peyton;
using namespace peyton::system;

extern Preferences pref;
bool SloanObservation::read(const char *str)
{
	// length of half of exposure
	static const double toffset = double(pref["time.toffset"])/(24.*3600.);

//	double t[5];
	int n = sscanf(str, "%d %d %d %d"
			" %lf %lf %d"
			" %lf %lf %lf %lf"
			" %lf %lf"
			" %lf %lf %lf"
			" %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf"
//			" %lf %lf %lf %lf %lf",
			" %lf",
		&run, &camCol, &field, &id,
		&rowc, &colc, &type,
		&vmu, &vnu, &vmuErr, &vnuErr,
		&p.ra, &p.dec,
		&gl, &gb, &reddening,
		&u, &uErr, &g, &gErr, &r, &rErr, &i, &iErr, &z, &zErr,
//		&t[0], &t[1], &t[2], &t[3], &t[4]);
		&time);
	p.ra *= ctn::d2r; p.dec *= ctn::d2r;
//	time = t[2] + toffset;
//	cout << "toff : " << toffset*(24.*3600.) << " : " << time << " : ";

	// calculate object observation time
	double time0 = time;

//	time += rowc * 0.396 / (361. * 3600.); // cf. expl. of the mjd[2] database fields
	time += rowc * 0.396 / (360.985647 * 3600.); // cf. expl. of the mjd[2] database fields
	time += toffset; // account for half-exposure time
	time += peyton::ctn::TDTminusTAI; // TAI to TDT conversion -- internally, we keep everything in TDT, but on output we
			                  // convert back to TAI (see IdentRecord::read and ::write for details)
//	std::cerr << (time - time0)*3600*24 << " " << toffset*3600*24 << " " << rowc << " " << time << "\n";

//	cout << time << "\n";

//	return n == 31;
	return n == 27;
}

typedef std::list<SloanObservation> SloanObservations;
typedef SloanObservations::iterator ii;

// rm id2.o cleanup.o id2.x; make id2.x
// ./id2.x test 100 ast.tbl.d/94 test test CAT

int loadObservations(std::vector<SloanObservation> &out, Radians node, Radians inc, ifstream &in)
{
	cout << setiosflags( ios::fixed );
	
	// load
	SloanObservation r; SloanObservations data;
	while(!in.eof()) {
		char buf[1000];
		in.getline(buf, 1000);
//		istringstream s(buf);

		if(!r.read(buf)) continue;
		
		data.push_back(r);
	}

	cout << " ..." << data.size() << " loaded\n"; cout.flush();

	int ndoubles = 0;
	// doubles sanity check
	for(ii i = data.begin(); i != data.end(); i++) {
		SloanObservation &a = (*i);
		ii j = i; j++;
		while(j != data.end()) {
			SloanObservation &b = (*j);
			if(a.run != b.run || a.camCol != b.camCol || a.field != b.field) { j++; continue; }
			if(fabs(a.colc - b.colc) < 1 && fabs(a.rowc - b.rowc) < 1) {
				// double - check which one is better (velocity criteria)
				if(fabs(a.vmu) > fabs(b.vmu)) { a = b; }
				// delete the looser
				ii k = j; j++; data.erase(k);
				ndoubles++;
			} else {
				j++;
			}
		}
	}
	cout << "\tDoubles : " << ndoubles << "\n";
#if 0
	// velocity sanity check
	int nvel = 0;
	int maxVel = 100;
	for(ii i = data.begin(); i != data.end();) {
		SloanObservation &a = (*i);
		if(fabs(a.vnu) > maxVel && fabs(a.vmu) > maxVel) {
			// screwed up velocities
			ii j = i; i++; data.erase(j);
			nvel++;
		} else {
			i++;
		}
	}
	cout << "\tProblematic velocities : " << nvel << "\n";
#endif
	// transform the observations to Gread Circles system
	out.erase(out.begin(), out.end());
	out.reserve(data.size());
	for(ii i = data.begin(); i != data.end(); i++) {
		SloanObservation &a = (*i);
		Coordinates::equgcs(node, inc, a.p.ra, a.p.dec, a.p.ra, a.p.dec);
		out.push_back(a);
	}

	cout << "\tAfter filtering : " << out.size() << "\n";

	return 0;
}
