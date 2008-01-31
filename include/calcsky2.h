#ifndef calcsky2__h__
#define calcsky2__h__

#include <astro/util.h>
#include <astro/math.h>
#include <iomanip>

struct ephemrec {
	int id;
	char name[18];
	double t0;
	peyton::Radians ra, dec;
	float mag;
	float err; // in radians!!
	peyton::Radians bearing, v;
	peyton::Radians R, dist;
};

std::ostream& operator <<(std::ostream &out, const ephemrec &o)
{
	out << std::setprecision(10);
	out
		<< o.id << "\t"
		<< o.name << "\t"
		<< o.t0 << "\t"
		<< o.ra / peyton::ctn::d2r << "\t"
		<< o.dec / peyton::ctn::d2r << "\t"
		<< std::setprecision(4) << o.mag << "\t"
		<< std::setprecision(2) << peyton::math::deg(o.err) << std::setprecision(10) << "\t"
		<< peyton::math::deg(o.bearing) << "\t"
		<< std::setprecision(5) << peyton::math::deg(o.v) << std::setprecision(10) << "\t"
		<< o.R << "\t" << o.dist;
	return out;
}

#endif
