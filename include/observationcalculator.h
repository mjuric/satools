#ifndef _observation_calculator_h
#define _observation_calculator_h

#include <astro/asteroids/asteroid.h>
#include <astro/asteroids/observation.h>
#include <astro/constants.h>
#include <astro/types.h>

#include <vector>

namespace CalcFlags {
	const int twoBody = 0x00000001;
};

namespace ObsFlags {
	const int vel = 0x00000001;
	const int pos = 0x00000004;
};

class ObservationCalculator 
{
public:
	ObservationCalculator();

	int calculateObservations(std::vector<peyton::asteroids::Observation> &obsv, peyton::MJD time, std::vector<peyton::asteroids::Asteroid> &obj, const int flags = ObsFlags::pos, const int calcFlags = 0);
	int calculateObservation(peyton::asteroids::Observation &obs, peyton::MJD time, peyton::asteroids::Asteroid &obj, const int flags = ObsFlags::pos, const int calcFlags = 0);
};

#endif
