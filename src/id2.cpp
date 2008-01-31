#include <iostream>
#include <fstream>
#include <iomanip>
#include <string>

#include <math.h>
#include <stdio.h>

#include <astro/system/preferences.h>
#include <astro/util.h>
#include <astro/coordinates.h>
#include <astro/system.h>

#include "orbfitlib.h"

#include "observationcalculator.h"
#include "observationcache.h"
#include "sloanobservation.h"
#include "identrecord.h"

#define stricmp strcasecmp

using namespace std;

using namespace peyton;
using namespace peyton::system;
using namespace peyton::asteroids;
using namespace peyton::sdss;

using peyton::coordinates::SkyPoint;

Preferences pref;

///////////////////////////////////////////////////////////////////////////////////

ofstream out;

struct cat_entry {
	int id;
	SkyPoint p;

	cat_entry *match;
	double dist;

	bool used;
};

typedef vector<cat_entry> cat_list;

inline Radians dist(Radians a, Radians b) {
	Radians d = a - b;
	Radians d2 = d > 1. ? d - ctn::pi2 : ( d < -1. ? d + ctn::pi2 : d);
	//if (d != d2) { cerr << d << " " << d2 << "\n"; }
	return d2;
}

#if 0
bool calculate_position(
	Radians &ra, Radians &dec, Radians &vra, Radians &vdec,
	const double mjd, const Asteroid& ast)
{
	static ObservationCalculator oc;
	static const double vdt = 600./(3600.*24.);
	Observation o, o1;

	// calculate position and velocity vectors
	if(oc.calculateObservation(o, mjd, ast, ObsFlags::pos | ObsFlags::vel) != 0) { return false; }
	if(oc.calculateObservation(o1, mjd+vdt, ast, ObsFlags::pos) != 0) { return false; }

	ra = o.ra;
	dec = o.dec;
	vra =  dist(o1.ra, o.ra);
	vdec = dist(o1.dec, o.dec) / vdt;
	
	return true;
}
#endif

void printMatch(RunGeometry &geom,
				SloanObservation &s,
				Observation &obs,
				Asteroid &ast,
				cat_entry &e)
{
	IdentRecord r;

#if 1
	Observation prev(obs), now(obs);
	if(ast.id > -1)
	{
		// calculate exact position for observation time
		MJD timec = s.camCol ? obs.t0 : s.time;
		MJD obstime = s.camCol ? s.time : obs.t0;
		//obstime = timec;

		static ObservationCalculator oc;
		oc.calculateObservation(now, obstime, ast, ObsFlags::pos | ObsFlags::vel);
		//oc.calculateObservation(now, obstime, ast, ObsFlags::pos | ObsFlags::vel, CalcFlags::twoBody);
		// convert equ. coordinates to GCS to compare to previous position
		Radians mu, nu;
		Coordinates::equgcs(geom.node, geom.inc, now.ra, now.dec, mu, nu);

		// sanity checking
		if(Coordinates::distance(mu, nu, prev.ra, prev.dec) > 1*ctn::s2r)
		{
			cerr << "*** WARNING!: " << ast.name
				<< " tobs-tcalc = " << (obstime - timec)*24*3600
				<< " dra = " << (mu-prev.ra)/ctn::d2r*3600
				<< " ddec = " << (nu-prev.dec)/ctn::d2r*3600
				<< " dmag = " << (now.mag-prev.mag)
				<< " dvra = " << (now.dra-prev.dra)/ctn::d2r*3600
				<< " dvdec = " << (now.ddec-prev.ddec)/ctn::d2r*3600
				<< "\n";
		}
	}
	r.calculatedT = SkyPoint(now.ra, now.dec);
#endif

	// convert coordinates from GCS to equatorial
	SkyPoint sp(s.p), obsp(obs.ra, obs.dec);

	// convert GCS velocity to equatorial
	r.mu = sp.ra; r.nu = sp.dec;
	Coordinates::rot_vel(geom.node, geom.inc, r.mu, r.nu, s.vmu*ctn::d2r, s.vnu*ctn::d2r, r.vra, r.vdec);

	// convert GCS coordinates to equatorial
	Coordinates::gcsequ(geom.node, geom.inc, sp.ra, sp.dec, sp.ra, sp.dec);
	Coordinates::gcsequ(geom.node, geom.inc, obsp.ra, obsp.dec, obsp.ra, obsp.dec);

#define OUTPUT_ECLIPTIC_VELOCITIES 1
#if OUTPUT_ECLIPTIC_VELOCITIES
	// convert equatorial to ecliptic
	Coordinates::rot_vel(0, -23.439291*ctn::d2r, sp.ra, sp.dec, r.vra, r.vdec, r.vra, r.vdec);
#endif

	DEG(r.vra); DEG(r.vdec);


	// prefer observed to calculated value
	if(s.camCol != 0) {
		Coordinates::equecl(sp.ra, sp.dec, r.lambda, r.beta);
	} else {
		Coordinates::equecl(obsp.ra, obsp.dec, r.lambda, r.beta);
	}

	r.time = s.camCol ? s.time : obs.t0;
//	cout << "r.time " << r.time << "\n";
	double antisun = util::approxSunLongitude(r.time) - ctn::pi;
	while(antisun < 0) antisun += ctn::pi2;
	while(antisun >= ctn::pi2) antisun -= ctn::pi2;
	r.phi = r.lambda - antisun;
	if(r.phi > ctn::pi) r.phi -= ctn::pi2;
	if(r.phi < -ctn::pi) r.phi += ctn::pi2;

	if(s.camCol == 0 || ast.id == -1) {
		r.raErr = r.decErr = 0;
	} else {
		r.raErr = sp.ra - obsp.ra, r.decErr = sp.dec - obsp.dec;
		if(r.raErr > ctn::pi) r.raErr = ctn::pi2 - r.raErr;
	}

	strcpy(r.name, ast.name);
	for(int i = 0; i != strlen(r.name); i++) { if(r.name[i] == ' ') r.name[i] = '_'; }

	r.run = s.run; r.camCol = s.camCol; r.field = s.field; r.id = s.id;
	r.ast = ast.id; r.type = ast.type; r.numeration = ast.numeration;
	r.observed = sp; r.colc = s.colc; r.rowc = s.rowc;
	r.u = s.u; r.g = s.g; r.r = s.r; r.i = s.i; r.z = s.z;
	r.uErr = s.uErr; r.gErr = s.gErr; r.rErr = s.rErr; r.iErr = s.iErr; r.zErr = s.zErr;
	r.vmu = s.vmu; r.vnu = s.vnu; r.vmuErr = s.vmuErr; r.vnuErr = s.vnuErr;


	r.calculated_time = obs.t0;
	r.calculatedTDI = SkyPoint(obsp.ra, obsp.dec); r.magc = obs.mag; r.umagc = obs.umag;
	r.vrac = obs.dra; r.vdecc = obs.ddec;
#if OUTPUT_ECLIPTIC_VELOCITIES
	// convert equatorial to ecliptic
	Coordinates::rot_vel(0, -23.439291*ctn::d2r, r.calculatedTDI.ra, r.calculatedTDI.dec, r.vrac, r.vdecc, r.vrac, r.vdecc);
#endif

	r.arc = ast.arc; r.H = ast.h; r.G = ast.g;
	r.epoch = ast.t0; r.a=ast.elements[0]; r.e=ast.elements[1]; r.inc=ast.elements[2]; r.lan=ast.elements[3]; r.aop=ast.elements[4]; r.M=ast.elements[5];
	r.R = obs.R; r.dist = obs.dist; r.phase = obs.phase;

	r.write(out);
}

void match(cat_list &obj, cat_list &cat)
{
	// initialize matches to NULL
	for(cat_list::iterator i = obj.begin(); i != obj.end(); i++) { (*i).match = NULL; (*i).used = false; }
	for(cat_list::iterator i = cat.begin(); i != cat.end(); i++) { (*i).match = NULL; (*i).used = false; }

	bool hadUsed;
	int iter = 0;
	do {
		iter++;
		// find closest
//		int cnt = 0;
		for(cat_list::iterator i = obj.begin(); i != obj.end(); i++) {
//			cnt++;
//			if(cnt % 1000 == 0) { cout << "#"; cout.flush(); }
			
			cat_entry &o = (*i);
			if(o.used) continue;

			o.dist = 1E10;

			for(cat_list::iterator j = cat.begin(); j != cat.end(); j++) {
				cat_entry &c = *j;
				if(c.used) continue;

				double dist;
#define APROX 1
#if APROX
				// calculate approximate distance
				// WARNING: works only near the equator
				// NOTE: ra & dec are in fact mu and nu here, so this _works_ always !!
				double dra = o.p.ra - c.p.ra;
				if(dra > ctn::pi) dra = ctn::pi2 - dra;
				double ddec = o.p.dec - c.p.dec;
				dist = sqr(dra) + sqr(ddec);

				if(dist < o.dist) {
#endif
					// recheck with exact distance
					dist = sqr(o.p.distance(c.p));
					if(dist > o.dist) continue;

					if(c.match != NULL) {
						if(c.match->dist < sqrt(dist)) {
//							cout << "*";
							continue;
						}
					}

					o.dist = dist;
					o.match = &c;
#if APROX
				}
#endif
			}

//			cout << " : ";

			if(o.match != NULL) {
				// clear old match it it has one
				if(o.match->match != NULL) {
					cat_entry &oo = *o.match->match;
//					cout << "Clearing old [" << oo.id << " -> " << oo.match->id << " : " << oo.dist/ctn::s2r << "] : ";
					oo.match = NULL;
				}

				o.dist = sqrt(o.dist);
				o.match->match = &o;

#if 0
				cout << "Match [" << o.id << "(" << o.p.ra/ctn::d2r << "," << o.p.dec/ctn::d2r << ")" << " -> " 
					<< o.match->id << "(" << o.match->p.ra/ctn::d2r << "," << o.match->p.dec/ctn::d2r << ")"
					<< " : " << o.dist/ctn::s2r << "]\n";
#endif
			}
		}

		cout << "Iter #" << iter << " : ";

		int mCount = 0, uCount = 0, usCount = 0;

		// mark matches as used
		hadUsed = false;
		for(cat_list::iterator i = obj.begin(); i != obj.end(); i++) {
			cat_entry &o = (*i);
			if(o.used) { usCount++; continue; }
			if(o.match == NULL) { uCount++; continue; }

			o.used = o.match->used = true;
			mCount++; hadUsed = true;
		}

		cout << "Matched " << mCount << ", ";
		cout << "Unmatched " << uCount << ", ";
		cout << "Used "  << usCount << "\n";
//		cin >> mCount;

	} while(hadUsed);
}

int applyRunMask(std::vector<Observation> &obsv, RunGeometry geom, Radians matchRadius)
{
	std::vector<Observation> tmp;

	Mask mask(geom.muStart, geom.muEnd, geom.nu);
	mask.expand(matchRadius, matchRadius);

//	for(int i = 0; i != 6; i++) {
//		cout << "rect " << mask.start/ctn::d2r << " " << mask.lo(i)/ctn::d2r << " " << mask.end/ctn::d2r << " " << mask.hi(i)/ctn::d2r << "\n";
//	}
	tmp.reserve(obsv.size());
	for(int i = 0; i != obsv.size(); i++) {
		Observation &o = obsv[i];
/*			if(Coordinates::inBox(o.ra, o.dec, 300.0*ctn::d2r, -4*ctn::d2r, 100.0*ctn::d2r, 2.0*ctn::d2r)) {
				tmp.push_back(o); cout << "#"; cout.flush();
			}
*/		for(int j = 0; j != 6; j++) {
			if(Coordinates::inBox(o.ra, o.dec, mask.start, mask.lo(j), mask.end, mask.hi(j))) {
				tmp.push_back(o);
//				cout << "#"; cout.flush();
			}
		}
	}
	obsv = tmp;
	return 0;
}

void deduceAndFixBounds(const std::vector<SloanObservation> &sloan, RunGeometry &geom)
{
	Radians mu0 = 1000., mu1 = -1000.;
	for(std::vector<SloanObservation>::const_iterator i = sloan.begin(); i != sloan.end(); i++)
	{
		mu0 = std::min(mu0, i->p.ra);
		mu1 = std::max(mu1, i->p.ra);
	}

	Radians len = mu1 - mu0;
	if(len > ctn::pi)
	{
		//std::cerr << "**" << mu0 << " " <<  mu1 << "\n";
		mu0 = 1000., mu1 = -1000.;
		for(std::vector<SloanObservation>::const_iterator i = sloan.begin(); i != sloan.end(); i++)
		{
			Radians ra = i->p.ra;
			if(ra > ctn::pi) { ra -= ctn::pi2; }

			mu0 = std::min(mu0, ra);
			mu1 = std::max(mu1, ra);
		}
		len = mu1 - mu0;
		//std::cerr << "**" << mu0 << " " <<  mu1 << "\n";
	}

//	std::cerr << "[mu,nu]_measured, [mu,nu]_rungeom = [" << mu0/ctn::d2r << ", " << mu1/ctn::d2r << "] ["
//		<< geom.muStart/ctn::d2r << ", " << geom.muEnd/ctn::d2r << "]\n";

	if(geom.muStart > geom.muEnd)
	{
		geom.muStart -= 2*ctn::pi;
	}

//	std::cerr << "[mu,nu]_measured, [mu,nu]_rungeom = [" << mu0/ctn::d2r << ", " << mu1/ctn::d2r << "] ["
//		<< geom.muStart/ctn::d2r << ", " << geom.muEnd/ctn::d2r << "]\n";

	if(geom.muStart > mu0 || geom.muEnd < mu1)
	{
		std::cout << "***  Bounds not big enough! Correcting...\n";
		std::cout << "***  Old [mu,nu]_rungeom = [" << geom.muStart/ctn::d2r << ", " << geom.muEnd/ctn::d2r << "]\n";
		Radians orig_mu0 = geom.muStart;
		Radians orig_t0 = geom.tstart;

		geom.muStart = mu0;
		geom.muEnd = mu1;
		geom.tstart = orig_t0 + (geom.muStart - orig_mu0)/ctn::pi2;
		geom.tend = orig_t0 + (geom.muEnd - orig_mu0)/ctn::pi2;

		std::cout << "***  New [mu,nu]_measured, [mu,nu]_rungeom = [" << mu0/ctn::d2r << ", " << mu1/ctn::d2r << "] ["
			<< geom.muStart/ctn::d2r << ", " << geom.muEnd/ctn::d2r << "]\n";
	}
        coordinates::normalize(geom.muEnd);
        coordinates::normalize(geom.muStart);
//	std::cerr << "Final [mu,nu]_measured, [mu,nu]_rungeom = [" << mu0/ctn::d2r << ", " << mu1/ctn::d2r << "] ["
//		<< geom.muStart/ctn::d2r << ", " << geom.muEnd/ctn::d2r << "]\n";

//        exit(0);
}

int loadObservations(std::vector<SloanObservation> &out, Radians node, Radians inc, ifstream &in);

int main(int argc, char* argv[])
{
	if(argc != 7) {
		cout << "Description: Identify objects from \"object\" catalog with objects in \"base\" catalog."
				" Which catalog is base, and which is object is controled by <base>=sloan|cat input parameter.\n";
//		cout << "Usage: " << argv[0] << " <output_file> <radius> <observations> <catalog> <cache> <base>\n";
		cout << "Usage: " << argv[0] << " <output_file> <radius> <observations> <catalog> <cache> <base>\n";
		cout << "Catalog must be in NATIVE format\n";
		return -1;
	}

	const char *ws = System::workspace();
	
	enum {SLOAN, CAT} base;
	if(!stricmp(argv[6], "sloan")) base = SLOAN;
	else if(!stricmp(argv[6], "cat")) base = CAT;
	else {
		cout << "<base> input parameter must be 'sloan' or 'cat'\n";
		return -1;
	}

	// construct paths
	char sout[1001], scat[1000], ssky[1000], sin[1000];
	sprintf(sout, "%s/output/%s.out", ws, argv[1]);
	sprintf(sin,  "%s/input/%s.data", ws, argv[3]);
	sprintf(ssky, "%s/tmp/skies/%s.cache", ws, argv[5]);
	sprintf(scat, "%s/tmp/propagated/%s.obj", ws, argv[4]);

	out.open(sout);
	const double matchRadius = atof(argv[2])*ctn::s2r;

	ifstream f(sin);
	Catalog *cat = Catalog::open(scat, "NATIVE");
	ObservationCache cache(ssky, "r");
	RunGeometry geom;
	RunGeometryDB geomDB;
	geomDB.getGeometry(cache.getHeader().geom.run, geom);

	cout << "Radius  : " << matchRadius/ctn::s2r << "\"\n";
	cout << "Run     : " << geom.run << "\n";
	cout << "Catalog : " << scat << "\n";
	cout << "Cache   : " << ssky << "\n";
	cout << "Input   : " << sin << "\n";
	cout << "Output  : " << sout << "\n";

	ObservationCalculator oc;
	vector<Observation> obsv;
	int i;

	// unidentified observation dummy object
	Observation unmached;
	unmached.name[0] = 0; unmached.ra = unmached.dec = unmached.ddec = unmached.dra = unmached.mag = 0;
	Asteroid unmachedAst; strcpy(unmachedAst.name, "Undef");
	SloanObservation unmatchedSloan;

	out << setiosflags( ios::fixed );

	int plus = 0, minus = 0;

	// load all SLOAN observations
	vector<SloanObservation> sloan;
	cout << "Loading observations..."; cout.flush();
	loadObservations(sloan, geom.node, geom.inc, f);
	deduceAndFixBounds(sloan, geom);

	// get cached objects and copy them to a list (to keep iterators valid when erasing)
	cout << "Loading cache..."; cout.flush();
	cache.getObservations(obsv, 0, cache.getObservationCount());
	cout << "... done\n"; cout.flush();
	cout << "Applying mask... "; cout.flush();
	applyRunMask(obsv, geom, matchRadius);
	cout << "... " << obsv.size() << " objects in field\n"; cout.flush();

	// run must be valid even for unidentified asteroids
	unmatchedSloan.run = sloan[0].run;

	// create cat_entry vectors
	std::vector<cat_entry> cat_sloan, cat_astorb;
	cat_entry e;
	for(int i = 0; i != obsv.size(); i++) { e.id = i; e.p = SkyPoint(obsv[i].ra, obsv[i].dec); cat_astorb.push_back(e); }
	for(int i = 0; i != sloan.size(); i++) { e.id = i; e.p = sloan[i].p; cat_sloan.push_back(e); }

	OrbfitPropagLibrary::initialize();

	// start
	IdentRecord::header(out);
	cout << "Starting matching procedure...\n"; cout.flush();

	switch(base) {
	case CAT:
		match(cat_sloan, cat_astorb);

		// print matches
		for(int i = 0; i != cat_sloan.size(); i++) {
			cat_entry &m = cat_sloan[i];

			if(!m.used || m.dist >= matchRadius) {
				printMatch(geom, sloan[i], unmached, unmachedAst, m);
				minus++;
			} else {
				Asteroid ast; cat->read(ast, obsv[m.match->id].id);
				printMatch(geom, sloan[i], obsv[m.match->id], ast, m);
				plus++;
			}
		}
		break;

	case SLOAN:
		match(cat_astorb, cat_sloan);

		// print matches
		for(int i = 0; i != cat_astorb.size(); i++) {
			cat_entry &m = cat_astorb[i];
			Asteroid ast; cat->read(ast, obsv[i].id);

			if(!m.used || m.dist >= matchRadius) {
				printMatch(geom, unmatchedSloan, obsv[i], ast, m);
				minus++;
			} else {
				printMatch(geom, sloan[m.match->id], obsv[i], ast, m);
				plus++;
			}
		}
		break;
	}

	cout << "Totals CAT   (+/-/total) : " << plus << "/" << obsv.size() - plus << "/" << obsv.size() << "    " << setprecision(2) << 100.0 * float(plus) / float(obsv.size()) << "%\n";
	cout << "Totals SLOAN (+/-/total) : " << plus << "/" << sloan.size() - plus << "/" << sloan.size() << "   " << setprecision(2) << 100.0 * float(plus) / float(sloan.size()) <<  "%\n";

	delete cat;

	return 0;
}
