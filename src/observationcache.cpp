#include "observationcalculator.h"
#include "observationcache.h"

#include <astro/system/error.h>
#include <astro/system/log.h>
#include <astro/constants.h>
#include <astro/coordinates.h>
#include <astro/exceptions.h>
#include <astro/skypoint.h>

#include <unistd.h>
#include <dirent.h>	
#include <stdio.h>
#include <iomanip>

#include <iostream>

using namespace peyton;
using namespace peyton::asteroids;
using namespace peyton::exceptions;
using namespace peyton::sdss;
using namespace peyton::system;
using namespace peyton::coordinates;

using namespace std;

void ObservationCache::loadCache(const char *fname, bool memory)
{
	if(h.geom.run != -1) return;

	// open and load the cache
	FILE *f;
	if(!memory) {
		f = fopen(fname, "rb");
	} else {
		pair<char *, size_t> &data = *(pair<char *, size_t> *)fname;
		f = fmemopen(data.first, data.second, "rb");
	}
	
	if(f == NULL) {
		THROW(EAny, "Error loading cache file");
	}

	fread(&h, sizeof(Header), 1, f);
	obsv.resize(h.len);
	fread(reinterpret_cast<void *>(&*obsv.begin()), sizeof(Observation), h.len, f);
	fclose(f);

	Log::write("Loaded cache : %s [%f MJD/%d objects]", fname, h.geom.tstart, obsv.size());
}

int ObservationCache::newCache(const char *fname, bool memory)
{
	// open cache file
	if(!memory) {
		f = fopen(fname, "wb");
	} else {
		pair<char *, size_t> *data = (pair<char *, size_t> *)fname;
		f = open_memstream(&data->first, &data->second);
	}
	if(f == NULL) { THROW(EAny, "Error creating cache file"); }

	// write empty header
	Header h;
	h.len = 0; h.geom.run = -1; h.catalog[0] = 0;
	fwrite(&h, sizeof(Header), 1, f);
}

int ObservationCache::closeCache()
{
	// commit header
	setHeader(getHeader(), true);

	// close file
	fclose(f); f = NULL;
}

int ObservationCache::setHeader(const Header h, bool commit)
{
	this->h = h;
	if(f != NULL && commit) {
		int at = ftell(f);
		fseek(f, 0, SEEK_SET);
		fwrite(&h, sizeof(h), 1, f);
		fseek(f, at, SEEK_SET);
	}
}

ObservationCache::Header ObservationCache::getHeader()
{
	return h;
}

int ObservationCache::append(Observation *o, int count)
{
	// append observations
	fseek(f, 0, SEEK_END);
	fwrite(o, sizeof(Observation), count, f);

	// update header
	Header h = getHeader();
	h.len += count;
	setHeader(h);

	return count;
}

int ObservationCache::create(RunGeometry &geom, Catalog *cat, ObservationCalculator *calc)
{
	const int blockSize = 10000;

	int recordCount = cat->recordCount();
	cout << setprecision(6);

	MJD d0 = geom.tstart - 4./24.;
//	cout << d0 << "\n";
	cout << "\n";

	const int maxiter = 10;
	int hist[maxiter+1] = {0};

	// start progress bar
	cout << "Progress [ "; cout.flush();

	// calculate cache
	vector<Asteroid> obj;
	vector<Observation> obsv;
	int i = 0, prog = 0;
	double tref;
	while(i < recordCount) {
		int read = cat->read(obj, i, i+blockSize);

		// calculate positions for the time of TDI scan start (0th approximation)
		calc->calculateObservations(obsv, geom.tstart, obj, ObsFlags::pos | ObsFlags::vel, CalcFlags::twoBody);

		// start iterative approximation for TDI display
		for(int k = 0; k != obsv.size(); k++) {
			Observation &o = obsv[k];
			coordinates::equgcs(geom.node, geom.inc, o.ra, o.dec, o.ra, o.dec);
			o.t0 = geom.tstart;

			int j;
			SkyPoint old;
			for(j = 0; j != maxiter; j++) {
//				if(i == 1023) { cout << setprecision(10) << j << " " << o.ra/ctn::d2r << " " << o.dec/ctn::d2r << " " << o.t0 << "\n"; cout.flush();}
			
				old = SkyPoint(o.ra, o.dec);

				// deal with compact (ra, dec) coordinate system
#if 0
				// this piece contained a _very_ subtle bug if the slew rate was != 1/360deg...
				o.t0 = geom.tstart + (o.ra - geom.muStart)/(ctn::pi2*361./360.);
//				o.t0 = geom.tstart + (o.ra - geom.muStart)/(ctn::pi2);
#else
				Radians len = o.ra > geom.muStart ? o.ra - geom.muStart : o.ra + ctn::pi2 - geom.muStart;
				o.t0 = geom.tstart + len/(ctn::pi2*1.00273791);
//				o.t0 = geom.tstart + len/(ctn::pi2);
#endif
				while(o.t0 > d0 + 1) o.t0 -= 1;
				while(o.t0 < d0 ) o.t0 += 1;

				// new approximation
				calc->calculateObservation(o, o.t0, obj[k], ObsFlags::pos | ObsFlags::vel, CalcFlags::twoBody);
				coordinates::equgcs(geom.node, geom.inc, o.ra, o.dec, o.ra, o.dec);


				// check convergence
				double dist = old.distance(SkyPoint(o.ra, o.dec));
/*				if(!strcmp(o.name, "2005 SV285"))
				{
					fprintf(stderr, "t0,t1=%f,%f, iter=%d, dist=%f, t=%f, name=%s, muOld=%f, mu=%f, nu=%f]\n", geom.tstart, geom.tend, j, dist/ctn::s2r, o.t0, o.name, old.ra/ctn::d2r, o.ra/ctn::d2r, o.dec/ctn::d2r);
				}
*/				if(dist < 0.01*ctn::s2r) break;
			}

			if(j == maxiter && (fabs(o.t0 - d0) > 0.04 && fabs(o.t0 - d0) < 0.96)) {
				// didn't converge - sound a warning
				fprintf(stderr, "A TDI position calculation didn't converge [id=%d, name=%s, raOld=%f, ra=%f, dec=%f]\n", o.id, o.name, old.ra/ctn::d2r, o.ra/ctn::d2r, o.dec/ctn::d2r);
				fprintf(stderr, "t0,t1=%f,%f,%f iter=%d, t=%f, name=%s, muOld=%f, mu=%f, nu=%f]\n", geom.tstart, geom.tend, o.t0-d0, j, o.t0, o.name, old.ra/ctn::d2r, o.ra/ctn::d2r, o.dec/ctn::d2r);
			}
			hist[j]++;
		}

		fwrite(reinterpret_cast<void *>(&*obsv.begin()), sizeof(Observation), obsv.size(), f);

		i += read;

		// progress bar
		while(int(50*double(i)/double(recordCount)) > prog) {
			cout << "#"; cout.flush();
			prog++;
		}
	}

	// finish progress bar
	cout << " ]\n"; cout.flush();

	// construct and write the true header
	Header h;
	memset(&h, 0, sizeof(h));
	h.geom = geom;
	h.len = i;
	strcpy(h.catalog, "NATIVE");
//	cat->identify(h.catalog);
	setHeader(h);

	closeCache();

	for(int i = 0; i != maxiter; i++) {
		cout << i+1 << ":" << hist[i] << (i+1 != maxiter ? " | " : "\n");
	}

	return 0;
}

ObservationCache::ObservationCache(const char *cf, const char *mode)
{
	// memory file?
	bool memory = strchr(mode, 'm') != NULL;
	
	// see if the path checks out
	if(!memory && mode[0] == 'r' && access(cf, 04)) { THROW(EAny, "Cannot access cache file"); }

	h.geom.run = -1;
	f = NULL;

	if(mode[0] == 'r') { loadCache(cf, memory); }
	if(mode[0] == 'w') { newCache(cf, memory); }
}

int ObservationCache::getObservationCount()
{
	return h.len;
}

int ObservationCache::getObservations(std::vector<Observation> &obs, int begin, int end)
{
	if(end > obsv.size()) end = obsv.size();
	if(begin < 0) begin = 0;
	if(begin > obsv.size()) begin = obsv.size();

	obs.clear();
	obs.insert(obs.begin(), obsv.begin() + begin, obsv.begin() + end);

	return end - begin;
}

ObservationCache::~ObservationCache()
{
	if(f != NULL) { closeCache(); }
}
