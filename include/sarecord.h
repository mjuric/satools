#ifndef __sarecord_h
#define __sarecord_h

#include <astro/types.h>
#include <astro/skypoint.h>

#include "identrecord.h"

namespace SAFlags {
	const int MPCSubmitted = 1;
};

class SARecord {
public:
	static char FORMAT_VERSION[20];
public:
	// regular
	peyton::MJD time;
	peyton::coordinates::SkyPoint p;
	double rowc, colc;

	// sdss
	int run, col, field, id;
	ObjectID oid;
	char detName[21]; // our identification
	int detNum;

	// flags
	int flags; // SDSS flags - defined according to the needs of SDSSMOC catalog/project
	int isIdentified; // flag if this asteroid has been matched
	
	int apparences; // number of apparences of this asteroid
	int iapp; // index of this apparence

	double u, g, r, i, z, b;
	double uErr, gErr, rErr, iErr, zErr, bErr;
	double V, B;

	double vmu, vnu, vmuErr, vnuErr;
	double vra, vdec;
//	double vraErr, vdecErr;

	// identification information
	peyton::coordinates::SkyPoint calculated;
	double magc;
	double R, dist, phase; // heliocentric & geocentric distance, phase angle
	double lambda, beta, phi; // ecliptic coordinates, distance from the opposition

	// orbital elements
	char oe_cat[21]; // catalog info - includes catalog ID and date - eg. ASTORB_20011011 for astorb
	double H, G, arc, epoch, a, e, inc, lan, aop, M;

	// proper elemets
	char pe_cat[21]; // catalog info
	double ap, ep, sinip;
public:
	SARecord();
	SARecord(IdentRecord &r);

	char *toString(char *s);
	char *toMPECString(char *s, char whichMag);
	int parse(char *s);

	void prettyPrint(std::ostream &o);
};

#endif
