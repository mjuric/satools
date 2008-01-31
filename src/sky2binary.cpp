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

using namespace std;
using namespace peyton;
using namespace peyton::exceptions;
using namespace peyton::asteroids;
using namespace peyton::sdss;
using namespace peyton::math;

peyton::system::Preferences pref;

main(int argc, char **argv)
{
	if(argc != 4) {
		cout << "Usage: " << argv[0] << " <input.txt> <output.bin> <output.idx>\n";
		cout << "Converts calcsky2.x text format to binary files with indices\n";
		return -1;
	}

	std::string inf(argv[1]);
	std::string outf(argv[2]);
	std::string idxf(argv[3]);

	std::map<double, int> index;
	ifstream in(inf.c_str());
	ofstream out(outf.c_str());

	// convert to binary
	int at = 0;
	while(in.good())
	{
		ephemrec r;
		in >> r.id >> r.name >> r.t0 >> r.ra >> r.dec >> r.mag >> r.err >> r.bearing >> r.v >> r.R >> r.dist;
		RAD(r.ra); RAD(r.dec); RAD(r.bearing); RAD(r.v); RAD(r.err);
		if(in.eof()) break;

		out.write((const char *)&r, sizeof(r));
		index[r.t0] = at++;
	}

	// dump index
	ofstream idx(idxf.c_str());
	typedef std::map<double, int>::iterator it;
	for(it i = index.begin(); i != index.end(); i++)
	{
		idx.write((const char *)&*i, sizeof(*i));
	}

	std::cerr << "Wrote " << at << " records for " << inf << " to " << outf << " and " << idxf << ".\n";
}
