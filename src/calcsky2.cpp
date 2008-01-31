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

#include <iomanip>
#include <fstream>
#include <iostream>

using namespace std;
using namespace peyton;
using namespace peyton::exceptions;
using namespace peyton::asteroids;
using namespace peyton::sdss;
using namespace peyton::math;

peyton::system::Preferences pref;

inline Radians dist(Radians a, Radians b) {
	Radians d = a - b;
	Radians d2 = d > 1. ? d - ctn::pi2 : ( d < -1. ? d + ctn::pi2 : d);
	//if (d != d2) { cerr << d << " " << d2 << "\n"; }
	return d2;
}

inline void propagateAsteroid(Asteroid &obj, MJD t1)
{
        double ele[6];
        proele_("KEP", obj.t0, obj.elements, t1, ele, 3);

        memcpy(obj.elements, ele, sizeof(double)*6);
        obj.t0 = t1;
}

template <typename T>
struct autotrig
{
	double a;
	double c, s;

	autotrig() : c(-10), s(-10) {}
	autotrig(const T &v) { *this = v; }
	autotrig(const autotrig &v) { *this = v; }

	T cos() { return c != -10 ? c : (c = cos(a)); }
	T sin() { return s != -10 ? s : (s = sin(a)); }

	operator T() { return a; }
	autotrig &operator =(const T &v) { a = v; c = s = -1; return *this; }
	autotrig &operator =(const autotrig &v) { memcpy(this, &v, sizeof(v)); return *this; }
};
//typedef autotrig<Radians> atRadians;
typedef Radians atRadians;

namespace peyton {
namespace coordinates {

double bearing(const atRadians &lon1, const atRadians &lat1, const atRadians &lon2, const atRadians &lat2)
{
	atRadians dlon = lon2 - lon1;
	Radians theta = atan2(sin(dlon)*cos(lat2), cos(lat1)*sin(lat2) - sin(lat1)*cos(lat2)*cos(dlon));
	return theta;
}

std::pair<double, double>
gc_move_by(const atRadians &lon1, const atRadians &lat1, const atRadians &brng, const atRadians &dist)
{
	Radians lat2 = asin(sin(lat1)*cos(dist) + cos(lat1)*sin(dist)*cos(brng)),
		lon2 = lon1 + atan2(sin(brng)*sin(dist)*cos(lat1), cos(dist)-sin(lat1)*sin(lat2));

	return make_pair(lon2, lat2);
}

} // coordinates
} // peyton

#define VARSTEP 1
#define LOGSTEPCTL 1
#define BEARING 1

main(int argc, char **argv)
{
	try {
		if(argc < 8) {
			cout << "Usage: " << argv[0] << " <output_file.txt> <mjd_star> <mjd_end> <dt> <tol> <catalog> <ASTORB2|NATIVE> [from, to)\n";
			cout << "Specify tolerance in arcsec, dt in days.\n";
			cout << "[from, to) can specify interval of catalog entries to process.\n";
			return -1;
		}

		const char *ws = System::workspace();
		char scat[1000];
		std::string prefix = argv[1];
		MJD d0 = atof(argv[2]); MJD dstart = d0;
		MJD d1 = atof(argv[3]);
		double dt = atof(argv[4]);
		const double vdt = 600./(3600.*24.);
		const Radians smax = rad(atof(argv[5])/3600.);

		string format = Util::toupper(argv[7]);
		if(format == "ASTORB2") {
			sprintf(scat, "%s/catalogs/astorb.dat.%s", ws, argv[6]);
		} else if(format == "NATIVE") {
			sprintf(scat, "%s/tmp/propagated/%s.obj", ws, argv[6]);
		} else {
			THROW(EAny, "Unknown catalog format [" + format);
		}

		const int from = argc > 8 ? atoi(argv[8]) : 0;
		const int to = argc > 9 ? atoi(argv[9]) : 200000000;

		OrbfitPropagLibrary::initialize();

		cout << "Time      : " << d0 << " to " << d1 << " (" << (d1 - d0) / 365.25 << " years)\n";
		cout << "Max tstep : " << dt << " days\n";
		cout << "Tolerance : " << 3600*deg(smax) << " arcsec\n";
		cout << "\n";
		cout << "Catalog   : " << scat << "\n";

		Catalog *cat = Catalog::open(scat, format.c_str());
		cout << "Objects   : " << cat->recordCount() << "\n";
		cout << "\n";
		
		cout << "Output file prefix : " << prefix << "\n";
		cout << "\n";
		cout << "Calculating:\n";

		cout.flush();
		
		ObservationCalculator oc;
		const int blockSize = 1000;

		int recordCount = std::min(cat->recordCount(), to);

		// calculate orbits
		vector<Asteroid> obj;
		vector<Observation> obsv, obsv1;
		int i = from, prog = 0;
		while(i < recordCount) {

			#if 0
			int read = cat->read(obj, i, i+blockSize);
			#else
			int read = 1;
			obj.resize(1);
			cat->read(obj[0], i);
			#endif

			for(int k = 0; obj[0].name[k] != 0; k++) { if(obj[0].name[k] == ' ') { obj[0].name[k] = '_'; } }

			std::string sout(prefix + "." + obj[0].name + ".txt");
			std::string soeout(prefix + "." + obj[0].name + ".oe.txt");
			cout << setw(4) << i << setw(18) << obj[0].name << " ==> " << sout << ", " << soeout << " ... ";
			ofstream out(sout.c_str());	out << setprecision(10);
			ofstream oeout(soeout.c_str());	oeout << setprecision(10);
			cout.flush();


			d0 = dstart;
			#if VARSTEP
			int nsucc = 0, nrestart = 0; time_t rt0 = time(NULL);
			double oldd0 = dstart, oldra, olddec, curdt = dt, epserr = 0;
			#if BEARING
			double bearing = 0, v = 0;
			#else
			double vlon, vlat;
			#endif
			#endif
			while(d0 <= d1)
			{
				// calculate positions for the time of TDI scan start (0th approximation)
				#if 0
				oc.calculateObservations(obsv, d0, obj, ObsFlags::pos | ObsFlags::vel, CalcFlags::twoBody);
				oc.calculateObservations(obsv1, d0+vdt, obj, ObsFlags::pos, CalcFlags::twoBody);
				#else
				oc.calculateObservations(obsv, d0, obj, ObsFlags::pos | ObsFlags::vel);
				oc.calculateObservations(obsv1, d0+vdt, obj, ObsFlags::pos);
				#endif

//	std::cerr << "t, RA, Dec = (" << setprecision(10) << d0 << "     : " << deg(obsv[0].ra) << ", " << deg(obsv[0].dec) << ")\n";
//	std::cerr << "\tcurdt = " << setprecision(10) << curdt << "\n";

				#if VARSTEP
					if(d0 != dstart)
					{
						#if BEARING
						std::pair<Radians, Radians> ppred = peyton::coordinates::gc_move_by(oldra, olddec, bearing, v*curdt);
						#else
						//peyton::coordinates::equecl(oldra, olddec, l
						double lon2 = fmod(oldra + curdt*vlon + ctn::twopi, ctn::twopi);
						double lat2 = fmod(olddec + curdt*vlat, ctn::twopi);
						std::pair<Radians, Radians> ppred(lon2, lat2);
						#endif
//	std::cerr << "\tpredicted RA, Dec = (" << deg(ppred.first) << ", " << deg(ppred.second) << ")\n";
						Radians ds = peyton::coordinates::distance(obsv[0].ra, obsv[0].dec, ppred.first, ppred.second);
						epserr = ds / smax;

//	std::cerr << "\tError: " << deg(ds) << " (fraction: " << epserr << ")\n";

						if(epserr > 1.) {
							double fact;
							#if !LOGSTEPCTL
							// adjust curdt (assume first order (linear) error growth with dt)
							static const double SAFETY = 1.1;
							curdt /= fact = SAFETY * epserr;
							#else
							curdt *= fact = 0.7;
							#endif
							d0 = oldd0 + curdt;

//	std::cerr << "\tAdjusting timestep to " << curdt << " (factor " << 1. / fact << ")\n";
							nrestart++;

							continue;
						}
					}
					nsucc++;
				#endif

				// permanently propagate the orbital elements
				bool propagated = d0 == dstart;
				for(int k = 0; k != obj.size(); k++)
				{
					if(fabs(obj[k].t0 - d0) >= dt)
					{
						propagateAsteroid(obj[k], d0);
						propagated = true;
					}
				}
				if(propagated)
				{
					Observation &o = obsv[0];
					Asteroid &a = obj[0];
					
					oeout << a.h << "\t";
					oeout << a.arc << "\t" << a.t0;
					for(int i = 0; i != 2; i++) oeout << "\t" << a.elements[i];
					for(int i = 2; i != 6; i++) oeout << "\t" << a.elements[i]/ctn::d2r;
					oeout << "\n";
				}

				// output the result
				for(int j = 0; j != obsv.size(); j++) {
					Observation &o = obsv[j];
					Asteroid &a = obj[j];
					char *c = o.name;
				//	cout << j << " " << c << "\n";
				//	continue;
					while(*c) {
						if(*c == ' ') *c = '_';
						c++;
					}
					out
						<< o.id << "\t"
						<< o.name << "\t"
						<< o.t0 << "\t"
						<< o.ra / ctn::d2r << "\t"
						<< o.dec / ctn::d2r << "\t"
						<< o.mag << "\t"
					//	<< o.dra / ctn::d2r << "\t"
					//	<< o.ddec / ctn::d2r << "\t"
					#if !BEARING
						<< dist(o.ra, obsv1[j].ra) / vdt / ctn::d2r << "\t"
						<< dist(o.dec, obsv1[j].dec) / vdt / ctn::d2r << "\t"
					#else
						<< setprecision(2) << deg(epserr * smax) * 3600 << setprecision(10) << "\t"
						<< deg(bearing) << "\t"
						<< setprecision(5) << v / ctn::d2r << setprecision(10) << "\t"
					#endif
						<< o.R << "\t" << o.dist;
//					out	<< "\t"
//						<< (o.ra - obsv1[j].ra) / vdt / ctn::d2r << "\t"
//						<< (o.dec - obsv1[j].dec) / vdt / ctn::d2r;

					out
						<< "\n";
				}
				
				// variable dt
				#if VARSTEP
					// store coordinates, calculate new bearing and speed
					oldra = obsv[0].ra;
					olddec = obsv[0].dec;
					bearing = peyton::coordinates::bearing(oldra, olddec, obsv1[0].ra, obsv1[0].dec);
					Radians ds = peyton::coordinates::distance(oldra, olddec, obsv1[0].ra, obsv1[0].dec);
					v = ds / vdt;

//	std::cerr << "\tbearing = " << deg(bearing) << " deg\n";
//	std::cerr << "\tspeed = " << v / ctn::d2r << " deg/day\n";
//	std::cerr << "\tcurdt = " << curdt << " days\n";

					// if we have an error estimate, attempt to increase timestep as much as possible while remaining
					// within error tolerance, but don't be to agressive
					double curdt2 = curdt;
//	std::cerr << "\tepserr: " << epserr << "\n";
					if(d0 != dstart && epserr < 0.8)
					{
						#if !LOGSTEPCTL
						static const double SAFETY2 = 1.1;
						curdt2 = curdt / (SAFETY2 * epserr);
						#else
						curdt2 = curdt * 1.1;
						#endif
					}
					#if LOGSTEPCTL
					else if(epserr > 0.9)
					{
//	std::cerr << "\tpreventively decreasing timestep\n";
						curdt2 = curdt * 0.9;
					}
					#endif

//	std::cerr << "\tnewdt1 = " << curdt2 << " days (factor " << curdt2 / curdt << " increase)\n";

					// move forward but...
					oldd0 = d0;
					int frame0 = (int)((d0 - dstart)/dt + 0.00001);
					d0 += min(curdt2, dt);

					// ... always produce a frame at dt boundary
					int frame1 = (int)((d0 - dstart)/dt - 0.00001);
					if(frame0 != frame1)
					{
						d0 = dstart + frame1*dt;
					}

//	std::cerr << "\tnewdt = " << (d0 - oldd0) << " days (factor " << (d0 - oldd0) / curdt << " increase)\n";

					curdt = d0 - oldd0;
				#else
				d0 += dt;
				#endif

			}

			#if VARSTEP
			int ncalc = nsucc + nrestart;
			double pctsucc = 100. * nsucc / ncalc;
			int rdt = time(NULL) - rt0;
	
			out << "# " << ncalc << " position calculations, ";
			out << setprecision(4) << pctsucc << setprecision(10) << "% successfull [" << rdt << " sec].\n";

			cout << setprecision(4);
			cout << ncalc << " calc, " << pctsucc << "% succ, " << rdt << " sec.";
			cout << setprecision(6);
			#endif

			cout << "  [  OK  ]\n";
			cout.flush();

			i += read;
		}
	} catch(EAny &e) {
		e.print();
		exit(-1);
	}

	return 0;
}
