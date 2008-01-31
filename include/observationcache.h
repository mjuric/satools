#ifndef __observation_cache_h
#define __observation_cache_h

#include <astro/asteroids/observation.h>
#include <astro/asteroids/catalog.h>
#include <astro/skypoint.h>
#include <astro/sdss/rungeometry.h>

#include <vector>
#include <map>

#include <stdio.h>

class ObservationCalculator;

class ObservationCache {
public:
	struct Header {
		char catalog[100];
		int len;
		peyton::sdss::RunGeometry geom;
	};

protected:
	Header h;
	std::vector<peyton::asteroids::Observation> obsv;
	FILE *f;

protected:
	void loadCache(const char *fname, bool memory);
	int newCache(const char *fname, bool memory);
	int closeCache();

public:
	ObservationCache(const char *cachefile, const char *mode);
	~ObservationCache();

	int create(peyton::sdss::RunGeometry &geom, peyton::asteroids::Catalog *cat, ObservationCalculator *calc);
	int append(peyton::asteroids::Observation *o, int count);
	
	int getObservationCount();
	int getObservations(std::vector<peyton::asteroids::Observation> &obs, int begin, int end);

	int setHeader(const Header h, bool commit = false);
	Header getHeader();
};

#endif
