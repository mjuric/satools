#include <astro/system/preferences.h>
#include <astro/system/log.h>
#include <astro/coordinates.h>
#include <astro/util.h>
#include <iostream>
#include <iomanip>
#include <astro/skypoint.h>

#include "orbfitlib.h"

#include "observationcalculator.h"

using namespace peyton;
using namespace peyton::system;
using namespace peyton::asteroids;
using namespace peyton::coordinates;
using namespace peyton::sdss;

using namespace std;

extern Preferences pref;

ObservationCalculator::ObservationCalculator()
{
	OrbfitPropagLibrary::initialize();
}

inline Radians angle_sub(Radians x2, Radians x1)
{
	return x1 > x2 ? x1 - x2 : x1 - x2 + 2.*ctn::pi;
}

int ObservationCalculator::calculateObservationsTDI(std::vector<Observation> &obsv, const std::vector<Asteroid> &obj, const RunGeometry &geom, const int flags, const int calcFlags)
{
	obsv.resize(obj.size());
	FOR(0, obsv.size())
	{
		calculateObservationTDI(obsv[i], obj[i], geom, flags, calcFlags);
	}
	return 0;
}

int ObservationCalculator::calculateObservationTDI(Observation &o, const Asteroid &obj, const RunGeometry &geom, const int flags, const int calcFlags)
{
	const int maxiter = 10;
	MJD d0 = geom.tstart - 4./24.;

	// calculate position at the time of TDI scan start (0th approximation)
	calculateObservation(o, geom.tstart, obj, ObsFlags::pos, calcFlags);
	coordinates::equgcs(geom.node, geom.inc, o.ra, o.dec, o.ra, o.dec);
	o.t0 = geom.tstart;

	// iterative refinement for TDI
	int j;
	SkyPoint old;
	for(j = 0; j != maxiter; j++)
	{
		old = SkyPoint(o.ra, o.dec);

		// deal with compact (ra, dec) coordinate system
		Radians len = o.ra > geom.muStart ? o.ra - geom.muStart : o.ra + ctn::pi2 - geom.muStart;
		o.t0 = geom.tstart + len/(ctn::pi2*1.00273791);

		while(o.t0 > d0 + 1) o.t0 -= 1;
		while(o.t0 < d0 ) o.t0 += 1;

		// new approximation
		calculateObservation(o, o.t0, obj, ObsFlags::pos, calcFlags);
		coordinates::equgcs(geom.node, geom.inc, o.ra, o.dec, o.ra, o.dec);

		// check convergence
		double dist = old.distance(SkyPoint(o.ra, o.dec));
		if(dist < 0.01*ctn::s2r) break;
	}

	// final calculation, with requested flags
	calculateObservation(o, o.t0, obj, flags, calcFlags);

	if(j == maxiter && (fabs(o.t0 - d0) > 0.04 && fabs(o.t0 - d0) < 0.96)) {
		// didn't converge - sound a warning
		fprintf(stderr, "A TDI position calculation didn't converge [id=%d, name=%s, raOld=%f, ra=%f, dec=%f]\n", o.id, o.name, old.ra/ctn::d2r, o.ra/ctn::d2r, o.dec/ctn::d2r);
		fprintf(stderr, "t0,t1=%f,%f,%f iter=%d, t=%f, name=%s, muOld=%f, mu=%f, nu=%f]\n", geom.tstart, geom.tend, o.t0-d0, j, o.t0, o.name, old.ra/ctn::d2r, o.ra/ctn::d2r, o.dec/ctn::d2r);
		return -1;
	}

	return 0;
}

int ObservationCalculator::calculateObservation(Observation &obs, MJD time, const Asteroid &obj, const int flags, const int calcFlags)
{
	// prepare the output observation record
	strcpy(obs.name, obj.name);
	obs.id = obj.id;
	obs.t0 = time;

	int iobs;
	int code = pref["observatory.code"];

//	std::cout << "[" << obj.name << "] " << obj.numeration << " " << obj.elements[0] << "\n";
	// (in)sanity checks - ASTORB sometimes has weird stuff in it...
	if(obj.elements[0] > 150 || obj.elements[0] == 0) { 
		obs.ra = 0; obs.dec = 88; obs.mag = 99; obs.R = 99; obs.phase = 0; return -1;
	}

	if(flags & ObsFlags::pos) {
		iobs = 1001; // get positions
		if(calcFlags & CalcFlags::twoBody) {
			preobs2_("KEP", obj.t0, code, time, obj.elements, iobs, obs.ra, obs.dec, obj.h, obj.g, obs.mag, 3);
		} else {
			preobs_("KEP", obj.t0, code, time, obj.elements, iobs, obs.ra, obs.dec, obj.h, obj.g, obs.mag, 3);
		}

		obs.flags &= ObsFlags::pos;
	}

	if(flags & ObsFlags::vel) {
#if 1
		iobs = 4001; // get velocities (angular)
		double dummy;
		if(calcFlags & CalcFlags::twoBody) {
			preobs2_("KEP", obj.t0, code, time, obj.elements, iobs, obs.dra, obs.ddec, obj.h, obj.g, dummy, 3);
		} else {
			preobs_("KEP", obj.t0, code, time, obj.elements, iobs, obs.dra, obs.ddec, obj.h, obj.g, dummy, 3);
		}
#else
		// direct computation of velocity as \delta(ra)/\delta(t)
		iobs = 1001; // get positions
		const double dt = 60./(3600.*24.);
		double mag2;
		Radians ra2, dec2;
		MJD t2 = time+dt;
		if(calcFlags & CalcFlags::twoBody) {
			preobs2_("KEP", obj.t0, code, t2, obj.elements, iobs, ra2, dec2, obj.h, obj.g, mag2, 3);
		} else {
			preobs_("KEP", obj.t0, code, t2, obj.elements, iobs, ra2, dec2, obj.h, obj.g, mag2, 3);
		}
		obs.dra = ra2 - obs.ra;
		obs.ddec = dec2 - obs.dec;
		if(fabs(obs.dra) > ctn::pi)  { obs.dra = 2*ctn::pi + (obs.dra < 0) * obs.dra; }
		if(fabs(obs.ddec) > ctn::pi) { obs.ddec = 2*ctn::pi + (obs.ddec < 0) * obs.ddec; }
		obs.dra /= dt;
		obs.ddec /= dt;
#endif
		obs.flags &= ObsFlags::vel;
	}

	// get R and phase angle
	phvars_(obs.R, obs.phase, obs.umag, obs.dist);
	// cout << obs.dist << "\n";
	ERRCHECK(obs.R < 0 || obs.R > 150 || obs.phase < 0 || obs.phase > ctn::pi) {
		Log::debug(Log::terminate, "Phase or R out of range [id=%d, desig=%s, R=%f AU, phase=%f rad]", obs.id, obs.name, obs.R, obs.phase);
		exit(-1);
	}

	return 0;
}

int ObservationCalculator::calculateObservations(vector<Observation> &obsv, MJD time, vector<Asteroid> &o, const int flags, const int calcFlags)
{
	obsv.clear();
	obsv.resize(o.size());

	for(int i = 0; i != o.size(); i++) {
		Asteroid &obj = o[i];
		Observation &obs = obsv[i];

		calculateObservation(obs, time, obj, flags, calcFlags);
	}

	return o.size();
}

