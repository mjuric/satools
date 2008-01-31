#ifndef __sloanobservation_h
#define __sloanobservation_h

#include <astro/constants.h>
#include <astro/skypoint.h>

#include <memory.h>
#include <stdio.h>

#include <iostream>
#include <iomanip>

class SloanObservation {
public:
	int run, camCol, field, id;
	double rowc, colc; int type;
	peyton::coordinates::SkyPoint p;
	double vmu, vnu, vmuErr, vnuErr;
	double gl, gb;
	double reddening;

	double u, g, r, i, z;
	double uErr, gErr, rErr, iErr, zErr;
	
	double time;
public:
	bool read(const char *str);
};

#endif

