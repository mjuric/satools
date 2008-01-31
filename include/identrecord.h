#ifndef __identrecord_h
#define __identrecord_h

#include <fstream>

#include <astro/types.h>
#include <astro/skypoint.h>
#include <astro/asteroids/asteroid.h>

// a unique object identifier for SDSS asteroid identification
struct ObjectID {
protected:
	char id[7];

public:
	bool operator==(const char *c) const { return strcmp(id, c) == 0; }
	bool operator==(const ObjectID &o) const { return memcmp(id, o.id, 7) == 0; }
	operator bool() const { return strcmp(id, "000000") != 0; }
	operator const char*() const { return id; }
	bool operator<(const ObjectID &o) const { return memcmp(id, o.id, 7) < 0; }
	bool operator>(const ObjectID &o) const { return memcmp(id, o.id, 7) > 0; }
	
	ObjectID() { memset(id, '0', 6); id[6] = 0;  }
	ObjectID(const char *c) { strncpy(id, c, 6); id[6] = 0; }
	ObjectID(const int n);

	static ObjectID generate(int run, double ra, double dec);

	friend inline std::istream& operator >> (std::istream& is, ObjectID& s) { char buf[100]; is >> buf; strncpy(s.id, buf, 6); s.id[6] = 0; return is; }
};
inline std::ostream& operator << (std::ostream& os, const ObjectID& s) { os << (const char *)s; return os; }

class IdentRecord {
public:
	ObjectID oid;

	int run, camCol, field, id;
	int ast, numeration; char name[peyton::asteroids::Asteroid::maxNameLen + 1];
	int type; // type = 0 - asteroid, 1 - comet
	
	peyton::MJD time;

	double rowc, colc; // pixel coordinates
	peyton::coordinates::SkyPoint observed; // sky coordinates
	double u, g, r, i, z, uErr, gErr, rErr, iErr, zErr;

	double mu, nu;
	double vra, vdec;
	double vmu, vnu, vmuErr, vnuErr;

	peyton::MJD calculated_time;
	peyton::coordinates::SkyPoint calculatedTDI;	// calculated at calculated_time
	peyton::coordinates::SkyPoint calculatedT;	// calculated at time
	double magc, umagc;
	
	double vrac, vdecc;				// WARNING: these are actually vlambda, vbeta...
	
	double raErr, decErr;
	
	double H, G, arc, epoch, a, e, inc, lan, aop, M;
	
	double R, dist, phase;
	
	double lambda, beta, phi;
	
public:
	bool read(std::istream &f);
	bool write(std::ostream &f, const char sep = ' ') const;
	
	static bool header(std::ostream &f, const char sep = ' ');
};
inline std::ostream& operator << (std::ostream& os, const IdentRecord& r) { r.write(os); return os; }
inline std::istream& operator >> (std::istream& is, IdentRecord& r) { r.read(is); return is; }
#endif
