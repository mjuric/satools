#include <astro/system/preferences.h>
#include <astro/asteroids/asteroid.h>
#include <astro/asteroids/observation.h>
#include <astro/catalogs/catalog.h>
#include <astro/system/error.h>
#include <astro/coordinates.h>
#include <astro/util.h>
#include <astro/constants.h>

#include <fstream>
#include <math.h>
#include <stdio.h>

#include <list>
#include <iomanip>

#include "mpecrecord.h"
#include "mpecfile.h"
#include "observationcalculator.h"

#include "analysis.h"

using namespace std;

Preferences pref;

class A : public MPECRecord {
public:
	Asteroid a;
	Observation o;

	double raErr, decErr;
	double err;
	double lambda, beta, phi;
	double vel;
public:
	A() : a(), o() {
		raErr = decErr = 0;
		lambda = beta = 0;
		phi = 0;
	}
	A(const MPECRecord &r) {
		MPECRecord *self = (MPECRecord *)this;
		memcpy(self, &r, sizeof(MPECRecord));
		raErr = decErr = 0;
		lambda = beta = 0;
		phi = 0;
	}
	void print(ostream &f);
};

void A::print(ostream &f)
{
/*	f << setprecision(18)
		// identification
		<< run << sep << camCol << sep << field << sep << id << sep 
		// catalog identification
		<< ast << sep << name << sep << numeration << sep << type << sep
		
		// SDSS measured
		<< setprecision(5)		// 1 second precision
		<< time << sep
		<< setprecision(6)		// 0.004 arcsec precision
		<< observed.ra/ctn::d2r << sep << observed.dec/ctn::d2r << sep

		<< setprecision(3)
		<< u << sep << uErr << sep 
		<< g << sep << gErr << sep 
		<< r << sep << rErr << sep 
		<< i << sep << iErr << sep 
		<< z << sep << zErr << sep 
		<< setprecision(4)		// 0.4 arcsec precision
		<< vrao << sep << vraoErr << sep
		<< vdeco << sep << vdecoErr << sep

		// calculated
		<< setprecision(6)		// 0.004 arcsec precision
		<< calculated.ra/ctn::d2r << sep << calculated.dec/ctn::d2r << sep << magc << sep	// positions
		<< setprecision(4)		// 0.4 arcsec precision
		<< vrac/ctn::d2r << sep << vdecc/ctn::d2r << sep					// velocities

		// identification errors
		<< setprecision(2)		// 0.01 arcsec precision
		<< raErr/ctn::s2r << sep << decErr/ctn::s2r << sep

		// catalog information
		<< setprecision(3)		// 3 decimal places
		<< H << sep << G << sep
		<< arc << sep 
		<< setprecision(5)		// 1 second precision
		<< epoch << sep
		<< setprecision(6)		// 6 decimal places
		<< a << sep << e << sep << inc/ctn::d2r << sep
		<< lan/ctn::d2r << sep << aop/ctn::d2r << sep << M/ctn::d2r << sep

		// heliocentric distance and phase
		<< setprecision(3)		// 3 decimal places
		<< R << sep << phase/ctn::d2r << sep

		// position in ecliptic coordinates, distance from opposition
		<< setprecision(6)		// 0.004 arcsec precision
		<< lambda/ctn::d2r << sep << beta/ctn::d2r << sep
		<< phi/ctn::d2r;
*/
}

typedef list<A> L;
typedef L::iterator ii;

typedef map<int, Catalog *> Catalogs;
Catalogs cats;

#include <algorithm>

/*struct less_err : public std::binary_function<A, A, bool> {
        bool operator()(const A a, const A b) const {
		return a.err < b.err;
        }
};*/

bool less_err(const A &a, const A &b) { return a.err < b.err; }
#define SORT(all, pred) { vector<A> tmp; tmp.insert(tmp.end(), all.begin(), all.end()); sort(tmp.begin(), tmp.end(), pred); all.clear(); all.insert(all.end(), tmp.begin(), tmp.end()); }

void serge(L all)
{
	CONSTRAIN_TO(all, 'V', band, 'V');
	CONSTRAIN_TO(all, 'y', identified, 'y');
	SORT(all, less_err);

	cout << "#\n"
		"# objectId, designation, time, rao, deco, magV,\n"
		"# rac, decc, magc,\n"
		"# raErr, decErr, err,\n"
		"# vrao, vraoErr, vdeco, vdecoErr,\n"
		"# vrac, vdecc,\n"
		"# R, phi,\n"
		"# epoch, a, e, i, lan, aop, M\n"
		"#\n";

	for(ii i = all.begin(); i != all.end(); i++) {
		A &a = (*i);
		char buf[1000];
		sprintf(buf, "%6s %-20s %11.5f %11.6f %+10.5f %5.2f "
			" %11.6f %+10.5f %5.2f "
			" %+6.3f %+6.3f %6.3f "
			" %+6.4f %6.4f %+6.4f %6.4f"
			" %+6.4f %+6.4f "
			" %7.3f %+6.3f "
			" %11.5f %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f",
			(const char *)a.oid, a.detName, a.time,
			a.p.ra/ctn::d2r, a.p.dec/ctn::d2r, a.mag,
			a.o.ra/ctn::d2r, a.o.dec/ctn::d2r, a.o.mag,
			a.raErr/ctn::s2r, a.decErr/ctn::s2r, a.err/ctn::s2r,
			a.vra, a.vraErr,
			a.vdec, a.vdecErr,
			a.o.dra/ctn::d2r, a.o.ddec/ctn::d2r,
			a.o.R, a.phi/ctn::d2r,
			a.a.t0, a.a.elements[0], a.a.elements[1], 
			a.a.elements[2]/ctn::d2r, a.a.elements[3]/ctn::d2r, a.a.elements[4]/ctn::d2r, a.a.elements[5]/ctn::d2r);
		cout << buf << "\n";
/*		cout << setprecision(10) << setiosflags(ios::fixed)
			<< a.oid << " " << a.detName << " " << a.time << " "
			<< a.p.ra/ctn::d2r << " " << a.p.dec/ctn::d2r << " " << a.mag << " "
			<< a.o.ra/ctn::d2r << " " << a.o.dec/ctn::d2r << " " << a.o.mag << " "
			<< a.raErr/ctn::s2r << " " << a.decErr/ctn::s2r << " " << a.err/ctn::s2r << " "
			<< a.vra << " " << a.vraErr << " "
			<< a.vdec << " " << a.vdecErr << " "
			<< a.o.dra/ctn::d2r << " " << a.o.ddec/ctn::d2r << " "
			<< a.o.R << " " << a.phi/ctn::s2r << " "
			<< a.a.t0 << " " << a.a.elements[0] << " " << a.a.elements[1];
			for(int j = 2; j != 6; j++) {
				cout << " " << a.a.elements[j]/ctn::d2r;
			}
			cout << "\n";*/
	}
}

main(int argc, char *argv[])
{
//	if(argc != 2) { cout << "Usage : " << argv[0] << " <file.sdss>\n"; return -1; }

	// load
	MPECFile mpec("all.mpec.sdss"/*argv[1]*/);
	if(mpec.problem) { Error::error(-1, "Problem loading %s file. Bailing out.", argv[1]); return -1; }
	cout << "# " << mpec.size() << " records loaded\n";

	// aux objects
	ObservationCalculator oc;

	// transform and get extended data
	A a; L all; Catalog *cat;
	int ndet = 0, nundet = 0;
	for(int i = 0; i != mpec.size(); i++) {
		a = mpec[i];

		// calculate opposition values
		Coordinates::equecl(a.p.ra, a.p.dec, a.lambda, a.beta);
		double antisun = Util::approxSunLongitude(a.time) - ctn::pi;
		if(antisun < 0) antisun += ctn::pi2;
		a.phi = a.lambda - antisun;
		if(a.phi > ctn::pi) a.phi -= ctn::pi2;

		// velocity
		a.vel = sqrt(a.vra*a.vra + a.vdec*a.vdec);

		// if this is an unidentified observation, that's it...
		if(a.identified != 'y') {
			nundet++;
			all.push_back(a);
			continue;
		}

		// load apropriate catalog
		Catalogs::iterator c = cats.find(a.run);
		if(c == cats.end()) {
			char buf[100];
			sprintf(buf, "%d.obj", a.run);
			cat = Catalog::open(buf, "NATIVE");
			cats[a.run] = cat;
		} else {
			cat = (*c).second;
		}

		// find and copy the asteroid
		const char *desig;
		if(a.oid == a.name) { desig = a.detName; }
		else { desig = a.name; }
		if(cat->read(a.a, desig) == -1) {
			Error::error(-1, "Cannot find asteroid %s in catalog file!", a.name);
			return -1;
		}

		// calculate position
		oc.calculateObservation(a.o, a.time, a.a, ObsFlags::pos | ObsFlags::vel, CalcFlags::twoBody);

		// residuals
		a.raErr = a.p.ra - a.o.ra;
		a.decErr = a.p.dec - a.o.dec;
		a.err = a.p.distance(SkyPoint(a.o.ra, a.o.dec)); //sqrt(sqr(a.raErr) + sqr(a.decErr));
		if(a.err/ctn::s2r > 30) cout << a.raErr/ctn::s2r << " " << a.err/ctn::s2r << "\n";

		// store it into the list
		ndet++;
		all.push_back(a);
	}

	cout << "#\n";

	cout << "# Loaded and gathered information on " << all.size() << " objects.\n";
	cout << "# " << ndet << " are identified\n";
	cout << "# " << nundet << " are unidentified\n";
	
	cout << "#\n";

#if 0
	//   Produce files for Serge's orbit algorithms
	serge(all);
	return 0;
#endif


#if 1
	//   Merge files of orbital elements of matched asteroids with those
	//   derived through Serge's algorithm
	{
	
	CONSTRAIN_TO(all, -15, phi/ctn::d2r, 15);
	CONSTRAIN_TO(all, 'V', band, 'V');
	CONSTRAIN_TO(all, 'y', identified, 'y');

	ifstream f("Rinc_goodvel2.dat");
	int nmatched = 0, ndet = 0;
	cout << "name rao deco r Ro a e i lan aop M "
		<< "ra  dec r b lambda beta phi vlambda vbeta R0 inc0 ac incc omegac deltadotc errorc\n";
	while(!f.eof()) {
		char buf[1000];
		double ra, dec, r, b, lambda, beta, phi, vlambda, vbeta, R0, inc0, a, inc, omega, deltadot, error;
		f.getline(buf, 1000);
		if(sscanf(buf, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
			&ra, &dec, &r, &a, &lambda, &beta, &phi, &vlambda, &vbeta, &R0, &inc0, &a, &inc, &omega, &deltadot, &error)
			!= 16) { continue; }
		// try to find a positional match
		SkyPoint p(ra, dec); p.toRad();
		const double radius = 2*ctn::s2r;
		ii i;
		for(i = all.begin(); i != all.end(); i++) {
			if((*i).p.distance(p) < radius) {
				break;
			}
		}
		if(i == all.end()) continue;
		A &ast = *i;
		cout << setiosflags(ios::fixed) << setprecision(8)
			<< ast.name << " " << ast.p << " " << ast.r << " " << ast.o.R << " "
			<< ast.a.elements[0] << " " << ast.a.elements[1] << " " << ast.a.elements[2]/ctn::d2r << " " 
			<< ast.a.elements[3]/ctn::d2r << " " << ast.a.elements[4]/ctn::d2r << " " << ast.a.elements[4]/ctn::d2r << " "
			<< buf << "\n";
			/*
			<< ast.name << " " << ast.o.R << " " << ast.a.elements[0] << " " << a
			<< "\n"*/;
	}

	return 0;	
	}
#endif

	// dump everything in an apropriate format
//	for(ii i = all.begin(); i != all.end(); i++) {
//	}
//	cout << 
//	cout << setprecision(10)

	//
	//   First standard constraints
	//	14 <= mag <= 21.5 (notice that this is our *observed* V-band magnitude)
	//	-15 <= phi/ctn::d2r <= 15

	CONSTRAIN_TO(all, 14, mag, 21.5);
	CONSTRAIN_TO(all, -15, phi/ctn::d2r, 15);

	//
	//   Extract only identified
	//

	L ident = all;
	CONSTRAIN_TO(ident, 'y', identified, 'y');

	//
	//   H distribution of detected asteroids
	//
	Bins bins;
	HISTOGRAM(ident, a.h, bins, 10, 20, 15);
	CUMULATIVE_HISTOGRAM(ident, a.h, bins, 10, 20, 15);

	WRITE_HIST(bins, "h_cum_histo.s.txt");

	//
	//   Magnitude distribution
	//

	HISTOGRAM(ident, mag, bins, 12, 25, 65);
	WRITE_HIST(bins, "mag_histo.s.txt");

	ofstream f("mags.txt");
	for(ii i = ident.begin(); i != ident.end(); i++) {
		A &a = *i;
		f << a.mag << " " << a.o.mag << "\n";
	}
}
