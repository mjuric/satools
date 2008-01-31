#include <astro/system/preferences.h>
#include <astro/system/log.h>
#include <iostream>
#include <iomanip>

#include "orbfitlib.h"

#include "observationcalculator.h"

using namespace peyton;
using namespace peyton::system;
using namespace peyton::asteroids;

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

int ObservationCalculator::calculateObservation(Observation &obs, MJD time, Asteroid &obj, const int flags, const int calcFlags)
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

