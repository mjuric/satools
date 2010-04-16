#include "version.h"

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

#include <algorithm>
#include <iomanip>
#include <fstream>
#include <iostream>
#include <set>

using namespace std;
using namespace peyton;
using namespace peyton::exceptions;
using namespace peyton::asteroids;
using namespace peyton::sdss;
using namespace peyton::math;

peyton::system::Preferences pref;
Catalog *cat = NULL;
bool in_run_only = false;

inline Radians dist(Radians a, Radians b) {
	Radians d = a - b;
	Radians d2 = d > 1. ? d - ctn::pi2 : ( d < -1. ? d + ctn::pi2 : d);
	//if (d != d2) { cerr << d << " " << d2 << "\n"; }
	return d2;
}

inline void propagateAsteroid(Asteroid &obj, MJD t1)
{
	double ele[6];
//	if(0 == strcmp(obj.name, "2006 F1")) {
//		std::cerr << obj.name << " " << t1 << " e=" << obj.elements[1] << " a=" << obj.elements[0] << " i=" << obj.elements[2]/ctn::d2r << "...\n";
//	}
	proele_("KEP", obj.t0, obj.elements, t1, ele, 3);

	memcpy(obj.elements, ele, sizeof(double)*6);
	obj.t0 = t1;
//	if(0 == strcmp(obj.name, "2006 F1")) {
//		std::cerr << obj.name << " " << t1 << " e=" << obj.elements[1] << " a=" << obj.elements[0] << " i=" << obj.elements[2]/ctn::d2r << "...\n";
//	}
}

char repl_wspc(const char c) { return c == ' ' ? '_' : c; }

void print_observation(const Observation &o, int numeration = -1)
{
	Radians vlambda, vbeta;
	Coordinates::rot_vel(0, -23.439291*ctn::d2r, o.ra, o.dec, o.dra, o.ddec, vlambda, vbeta);
	
	std::string name(o.name);
	transform(name.begin(), name.end(), name.begin(), repl_wspc);

	std::cout << setiosflags(ios::fixed)
		<< name << "\t"
		<< numeration << "\t"
		<< std::setprecision(6) << o.t0 << "\t"
		<< setprecision(6)
		<< o.ra / ctn::d2r << "\t"
		<< o.dec / ctn::d2r << "\t"
		<< setprecision(3)
		<< o.mag << "\t"
		<< setprecision(4)		// 0.4 arcsec precision
		<< o.dra / ctn::d2r << "\t"
		<< o.ddec / ctn::d2r << "\t"
		<< vlambda / ctn::d2r << "\t"
		<< vbeta / ctn::d2r << "\t"
		<< setprecision(2)		// 0.4 arcsec precision
		<< o.R << "\t" << o.dist;
}

bool geom_epoch_sort(const RunGeometry &a, const RunGeometry &b)
{
	return a.tstart > b.tstart;
}

void calculate_position_in_runs(std::vector<Observation> &obsv, const std::vector<RunGeometry> &geoms, const Asteroid &obj)
{
	// propagate orbits & calculate positions at each epoch
	static ObservationCalculator oc;

	Asteroid ast(obj);
	obsv.resize(geoms.size());

	FOR(0, geoms.size())
	{
		const RunGeometry &geom = geoms[i];
		//std::cerr << "AST: " << ast.t0 << " " << ast.elements[0] << "\n";
		//std::cerr << "GEOM: " << geom.run << " " << geom.tstart << "\n";
		//continue;
		Asteroid prev(ast);
		propagateAsteroid(ast, geom.tstart);
		if(ast.elements[1] >= 1.)
		{
			std::cerr << "Object " << ast.name << " became parabolic/hyperbolic in run " << geom.run << " (MJD ~ " << geom.tstart << "). Skipping.\n";
			ast = prev;
		}
		else
		{
//			oc.calculateObservationTDI(obsv[i], ast, geom, ObsFlags::pos | ObsFlags::vel);
			oc.calculateObservationTDI(obsv[i], ast, geom, ObsFlags::pos | ObsFlags::vel, CalcFlags::twoBody);
		}
	}
}

bool is_in_run_aux(const Observation &o, const Mask &mask, const Radians matchRadius)
{
	for(int j = 0; j != 6; j++)
	{
		if(Coordinates::inBox(o.ra, o.dec, mask.start, mask.lo(j), mask.end, mask.hi(j)))
		{
			return true;
		}
	}
	return false;
}

void is_in_run(bool &within_bounds, bool &within_tolerance, const Observation &obj, const RunGeometry &geom, const Radians matchRadius)
{
	// convert to mu/nu
	Observation o(obj);
	Coordinates::equgcs(geom.node, geom.inc, obj.ra, obj.dec, o.ra, o.dec);

	Mask mask(geom.muStart, geom.muEnd, geom.nu);
	within_bounds = is_in_run_aux(o, geom, matchRadius);

	mask.expand(matchRadius, matchRadius);
	within_tolerance = is_in_run_aux(o, geom, matchRadius);
}

void calculate_positions_in_runs(const std::set<int> &runs, std::vector<std::string> &desigs)
{
	// load all runs, sort according to time
	RunGeometryDB db;
	std::vector<RunGeometry> geoms;
	RunGeometry geom;
	FOREACHj(run, runs)
	{
		db.getGeometry(*run, geom);
		geoms.push_back(geom);
	}
	std::sort(geoms.begin(), geoms.end(), geom_epoch_sort);

	// load requested asteroids
	std::vector<Asteroid> asts;
	if(desigs.empty())
	{
		cat->read(asts, 0, cat->recordCount());
	}
	else
	{
		asts.resize(desigs.size());
		for(int i=0; i != desigs.size(); i++)
		{
			if(cat->read(asts[i], desigs[i].c_str()) != 0)
			{
				std::cerr << "Error: Object " << desigs[i] << " does not exist in the catalog.\n";
				exit(-1);
			}
		}
	}

	// propagate orbits & calculate positions at each epoch
	std::vector<Observation> obsv;
	FOREACH(asts)
	{
		Asteroid &ast = *i;
		calculate_position_in_runs(obsv, geoms, ast);
		FOR(0, obsv.size())
		{
			bool within_bounds, within_tolerance;
			is_in_run(within_bounds, within_tolerance, obsv[i], geoms[i], 30*ctn::s2r);		
			
			if(in_run_only && !within_tolerance) { continue; }

			std::cout << geoms[i].run << "\t";
			print_observation(obsv[i], ast.numeration);
			std::cout << "\t" << within_bounds << "\t" << within_tolerance;
			std::cout << "\n";
		}
	}
}

void calculate_position(double mjd, const std::string &desig)
{
	static ObservationCalculator oc;
	static const double vdt = 600./(3600.*24.);
	Asteroid ast;
	Observation o, o1;

	// locate asteroid
	if(cat->read(ast, desig.c_str()) == -1)
	{
		//std::cerr << "[" << desig << "]: No asteroid of such name in the catalog\n";
		o.ra = o.dec = o1.ra = o1.dec = 0;
	}
	else
	{
		// calculate position and velocity vectors
		oc.calculateObservation(o, mjd, ast, ObsFlags::pos | ObsFlags::vel);
		oc.calculateObservation(o1, mjd+vdt, ast, ObsFlags::pos);
	}

#if 0
	// print out
	std::cout
		<< setprecision(12) << mjd << "\t"
		<< desig << "\t"
		<< setprecision(10) << o.ra / ctn::d2r << "\t"
		<< setprecision(10) << o.dec / ctn::d2r << "\t"
		<< setprecision(7) << dist(o1.ra, o.ra) / vdt / ctn::d2r << "\t"
		<< setprecision(7) << dist(o1.dec, o.dec) / vdt / ctn::d2r << "\n"
		;
#else
	print_observation(o, ast.numeration);
	std::cout << "\n";
#endif
}

main(int argc, char **argv)
{
	PRINT_VERSION_IF_ASKED(argc, argv);

	try {
		if(argc != 3 && argc < 5) {
			cout << "Usage 1: " << argv[0] << " <mjd> <desig> <catalog> <ASTORB2|NATIVE|COMET>\n";
			cout << "Usage 2: " << argv[0] << " <catalog> <ASTORB2|NATIVE|COMET>\n";
			cout << "Usage 3: " << argv[0] << " -runs [-desig=<desig|->] [-in_run_only] <catalog> <ASTORB2|NATIVE|COMET> <run1> [run2 [...]]";
			cout << "  Calculates the (ra, dec) of an asteroid, given time, designation and orbit catalog.\n";
			cout << "  If used in second form, desig and mjd are read from stdin.\n";
			cout << "  Third form calculates the positions of objects in a given SDSS run (takes scanning into account).\n";
			return -1;
		}

		const char *ws = System::workspace();
		char scat[1000];
		
		std::string catname, cattype, desig; double mjd = 0;
		std::set<int> runs;
		std::vector<std::string> desigs;

		if(argc >= 5 && strcmp(argv[1], "-runs") == 0)
		{
			if(strncmp(argv[2], "-desig=", 7) == 0)
			{
				// calculate for just one object
				desig = argv[2] + 7;
				argv++; argc--;
				if(desig == "-")
				{
					// slurp from STDIN
					while(cin >> desig)
					{
						desigs.push_back(desig);
					}
				}
				else
				{
					desigs.push_back(desig);
				}
			}
			if(strcmp(argv[2], "-in_run_only") == 0)
			{
				in_run_only = true;
				argv++; argc--;
			}
			catname = argv[2];
			cattype = argv[3];
			for(int k=4; k != argc; k++)
			{
				int run = atoi(argv[k]);
				if(run == 0) {
					// assume this is a file containing run numbers
					ifstream in(argv[k]);
					if(!in) { std::cerr << "Cannot open " << argv[k] << " file. Aborting."; exit(-1); }
					while(in >> run) runs.insert(run);
					if(!in.eof()) { std::cerr << "Reading of " << argv[k] << " stopped before end of file. Aborting."; exit(-1); }
				}
				else
				{
					runs.insert(run);
				}
//				std::cerr << "run: " << run << "\n";
			}
//			std::cerr << catname << " " << cattype << "\n";
		}
		else if(argc == 3)
		{
			catname = argv[1];
			cattype = argv[2];
		}
		else
		{
			mjd = atof(argv[1]);
			desig = argv[2];
			catname = argv[3];
			cattype = argv[4];
			std::cerr << "desig=" << desig << "\n";
		}

		string format = Util::toupper(cattype);
		if(format == "ASTORB2") {
			sprintf(scat, "%s/catalogs/astorb.dat.%s", ws, catname.c_str());
		} else if(format == "NATIVE") {
			sprintf(scat, "%s/tmp/propagated/%s.obj", ws, catname.c_str());
		} else if(format == "COMET") {
			sprintf(scat, "%s/catalogs/%s", ws, catname.c_str());
		} else {
			THROW(EAny, "Unknown catalog format [" + format + "]");
		}

		OrbfitPropagLibrary::initialize();

		cerr << "Catalog   : " << scat << "\n";

		cat = Catalog::open(scat, format.c_str());
		cerr << "Objects   : " << cat->recordCount() << "\n";
		cerr << "\n";

		if(!runs.empty())
		{
			calculate_positions_in_runs(runs, desigs);
		}
		else if(!desig.empty())
		{
			calculate_position(mjd, desig);
		}
		else
		{
			cin >> mjd;
			while(getline(cin, desig))
			{
				desig = Util::trim(desig);
				calculate_position(mjd, desig);
				cin >> mjd;
			}
		}

	} catch(EAny &e) {
		e.print();
		exit(-1);
	}

	return 0;
}
