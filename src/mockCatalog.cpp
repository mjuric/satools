/***************************************************************************
                          mockCatalog.cpp  -  create
	a mock asteroid catalog based on an existing orbital elements
	distribution from ASTORB and a given magnitude distribution.
                             -------------------
    begin                : Wed Dec 11 2002
    copyright            : (C) 2002 by Mario Juric
    email                : mjuric@astro.princeton.edu
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "version.h"

#include <astro/asteroids/catalog.h>
#include <astro/system.h>
#include <astro/exceptions.h>
#include <astro/system/error.h>
#include <astro/util.h>
 
#include <memory>
#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include <valarray>

#include <stdio.h>
#include <iostream>

using namespace std;
using namespace peyton;
using namespace peyton::asteroids;
using namespace peyton::exceptions;

// a simple hash-based sparse array

//const double bM = 1., baop = 1., blan = 1., bi = .1, be = .02, ba = .02;
//const double box[] = { .002, .002, .1, 1, 1, 1 };
const double box[] = { .01, .02, .5, 1, 1, 1 };
/*
typedef vector<Asteroid> *pV;

class LUT : public vector<pV> {
public:
	int h, w;
public:
	LUT(int w_, int h_) : h(h_), w(w_), vector<pV>(h_*w_) { FOREACH(*this) { *i = NULL; } }
	pV &operator()(int i, int j) { return (*this)(i + w*j); }
};
*/

double rnd() { return double(rand())/double(RAND_MAX); }

float gaussdev()
	// Returns a normally distributed deviate with zero mean and unit variance, using ran1(idum)
	// as the source of uniform deviates.
{
	static int iset=0;
	static float gset;
	float fac,rsq,v1,v2;
	if (iset == 0) { // We don.t have an extra deviate handy, so
		do {
			v1=2.0*rnd()-1.0;	// pick two uniform numbers in the square extending
						// from -1 to +1 in each direction,
			v2=2.0*rnd()-1.0;
			rsq=v1*v1+v2*v2;
		} while (rsq >= 1.0 || rsq == 0.0); // and if they are not, try again.

		fac=sqrt(-2.0*log(rsq)/rsq);
		gset=v1*fac;
		iset=1; // Set flag.
		return v2*fac;
	} else {
		iset=0; // so unset the flag,
		return gset; // and return it.
	}
}

int main(int argc, char **argv)
{
	PRINT_VERSION_IF_ASKED(argc, argv);

	srand(time(NULL));
	try {
		const char *ws = System::workspace();

		// create convol. kernel
		const char *astorb = "20020912";
		char buf[1000];
		sprintf(buf, "%s/catalogs/astorb.dat.%s", ws, astorb);

		// read asteroids
		std::vector<Asteroid> ast;
		auto_ptr<Catalog> cat(Catalog::open(buf, "ASTORB2"));
		cat->read(ast, 0, cat->recordCount());

		int nfactor = 10; double ffactor = .33;

		vector<Asteroid> mock;

		cerr << "Catalog  : " << ast.size() << " objects\n";
		fprintf(stderr, "Expected : %10d objects\n", int(ast.size()*(nfactor+ffactor)));
		FOREACH(ast) {
			FORj(j, 2, 6) { (*i).elements[j] /= ctn::d2r; }
			int nf = nfactor + ((rnd() <= ffactor) ? 1 : 0);
			FORj(f, 0, nf) {
				Asteroid a = *i;
				FOR(0, 6) {
					// a.elements[i] += box[i] * (rnd() - .5);
					a.elements[i] += gaussdev()*box[i];
				}
				mock.push_back(a);
			}
		}
		fprintf(stderr, "Generated : %10d objects\n", mock.size());

		// 20021122 232.067049  74.043505  80.482660 10.583568 0.07928004   2.76596026
		FOREACH(mock) {
			Asteroid &a = *i;
			printf("20021122 %12.8f %10.6f %10.6f %10.6f %10.8f %12.8f\n",
				a.elements[5], a.elements[4], a.elements[3],
				a.elements[2], a.elements[1], a.elements[0]);
		}

/*		// create LUT
		LUT lut(7./ba, .5/be);
		FOREACH(ast) {
			pV &p = lut((*i).elements[0]/ba, (*i).elements[1]/be);
			if(p == NULL) { (*p).push_back(i); }
		}

		double m = 10.;
		// go through LUT and multiply through
		FORj(a, 0, lut.w) FORj(e, 0, lut.h) {
			vector<Asteroid> &v = *lut(a, e);
			if(&v == NULL) continue;

			// do further windowing
			sort(v.begin(), v.end(), sort_i);
		}
*/
	} catch(EAny &e) {
		e.print();
	}
}
