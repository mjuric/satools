#ifndef __rungeometry_h
#define __rungeometry_h

#include <astro/types.h>

#include <map>

class Mask {
protected:
	static Radians nuCam[6][2];
public:
	Radians nu[6][2], start, end;

	Mask(Radians s, Radians e, Radians nu0) { setAt(s, e, nu0); }
	Mask() { setAt(0, 0, 0); }

	void setAt(Radians s, Radians e, Radians nu0);
	void expand(Radians x, Radians y);

	inline Radians lo(int col) { return nu[col][0]; }
	inline Radians hi(int col) { return nu[col][1]; }
};

struct RunGeometry {
	int run;
	Radians ra, dec, node, inc, muStart, muEnd, nu;
	MJD tstart, tend;
};

class RunGeometryDB {
public:
	std::map<int, RunGeometry> db;
public:
	RunGeometryDB();
	bool getGeometry(int run, RunGeometry &geom);
};

#endif
