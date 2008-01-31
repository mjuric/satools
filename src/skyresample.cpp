#include <astro/system/preferences.h>
#include <astro/system/error.h>
#include <astro/system.h>
#include <astro/exceptions.h>
#include <astro/util.h>
#include <astro/math.h>
#include <astro/coordinates.h>

#include "observationcache.h"
#include "observationcalculator.h"

#include "orbfitlib.h"
#include "calcsky2.h"

#include <iomanip>
#include <fstream>
#include <iostream>
#include <algorithm>

using namespace std;
using namespace peyton;
using namespace peyton::exceptions;
using namespace peyton::asteroids;
using namespace peyton::sdss;
using namespace peyton::math;

peyton::system::Preferences pref;

struct cmp
{
	bool operator()(const std::pair<double, int> &a, const double &b)
	{
		return a.first < b;
	}
};

void load_rec(ephemrec &r, istream &in, int idx)
{
	in.seekg(idx*sizeof(r));
	if(!in.read((char *)&r, sizeof(r)))
	{
		std::cerr << "Error while reading binary record. Aborting\n";
		abort();
	}
}

std::pair<double, double>
gc_move_by(const Radians &lon1, const Radians &lat1, const Radians &brng, const Radians &dist)
{
	Radians lat2 = asin(sin(lat1)*cos(dist) + cos(lat1)*sin(dist)*cos(brng)),
		lon2 = lon1 + atan2(sin(brng)*sin(dist)*cos(lat1), cos(dist)-sin(lat1)*sin(lat2));

	return make_pair(lon2, lat2);
}


void interpolate(ephemrec &r, ephemrec &r1, ephemrec &r2, double t)
{
	double dt = (t - r1.t0);
	double frac = dt / (r2.t0 - r1.t0);
	//std::cerr << setprecision(10) << "(t, t0, t1) = " << t << " " << r1.t0 << " " << r2.t0 << " Frac = " << frac << "\n";

	Radians dist = r1.v * dt;
	//std::cerr << "v, bearing, dt, dist = " << deg(r1.v) << " " << deg(r1.bearing) << " " << dt << " " << deg(dist) << "\n";
	pair<double, double> p = gc_move_by(r1.ra, r1.dec, r1.bearing, dist);

	r = r1;
	r.t0 = t;

	r.mag += (r2.mag - r1.mag)*frac;
	r.R += (r2.R - r1.R)*frac;
	r.dist += (r2.dist - r1.dist)*frac;
	r.bearing += (r2.bearing - r1.bearing)*frac;
	r.v += (r2.v - r1.v)*frac;
	
	r.ra = p.first;
	r.dec = p.second;
}

main(int argc, char **argv)
{
	if(argc != 6) {
		cout << "Usage: " << argv[0] << " <t0> <t1> <dt> <ephemeris.bin> <ephemeris.idx>\n";
		cout << "Resamples the emphemeris file to new dt grid\n";
		return -1;
	}

	double t0 = atof(argv[1]);
	double t1 = atof(argv[2]);
	double dt = atof(argv[3]);
	ifstream  ephem(argv[4]);
	ifstream iephem(argv[5]);

	std::cerr << "Input         : " << argv[4] << " " << argv[5] << "\n";

	typedef std::pair<double, int> pdi;
	std::vector<pdi> index;
	pdi idx;

	// load index
	iephem.seekg(0, ios::end);
	int fsize = iephem.tellg();
	if(fsize % sizeof(idx) != 0)
	{
		std::cerr << "Index file not a multiple of sizeof(std::pair<double, int>). Aborting.\n";
		abort();
	}
	int nidx = fsize / sizeof(idx);
	index.resize(nidx);
	iephem.seekg(0);
	iephem.read((char *)&index[0], fsize);


	// automatic time limits
	if(t0 == t1)
	{
		t0 = index.front().first;
		t1 = index.back().first;
	}

	std::cerr << "Time interval : " << t0 << " " << t1 << "\n";
	std::cerr << "Timestep      : " << dt << "\n";

	// find the interpolation points, load them, interpolate
	std::vector<pdi>::iterator it = index.begin();
	int n = 0;
	for(double t = t0; t <= t1; t += dt)
	{
		it = std::lower_bound(it, index.end(), t, cmp());
		if(it == index.end()) {
			std::cerr << "Time index " << t << " is out of range. Aborting.\n";
			abort();
		}
		
		if((*it).first == t)
		{
			ephemrec r;
			load_rec(r, ephem, (*it).second);
			std::cout << r << "\n";
			continue;
		}
		else if(it == index.begin())
		{
			std::cerr << "Time index " << t << " is out of range. Aborting.\n";
			abort();
		}

		// load & interpolate
		ephemrec r, r1, r2;
		load_rec(r2, ephem, (*it).second); --it;
		load_rec(r1, ephem, (*it).second);

		interpolate(r, r1, r2, t);

		std::cout << r << "\n";
		n++;
	}
	std::cerr << n << " records written.\n";
}
