#include "version.h"

#include <astro/time.h>
#include <astro/types.h>
#include <astro/util.h>
#include <astro/system/preferences.h>
#include <astro/asteroids/catalog.h>
#include <astro/system/log.h>
#include <astro/constants.h>
#include <astro/system.h>

#include <algorithm>

#include <string.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <unistd.h>

#include <vector>

#include "identrecord.h"
#include "sarecord.h"

#include <iostream>

#define stricmp strcasecmp

using namespace std;
using namespace peyton;
using namespace peyton::asteroids;
using namespace peyton::system;

peyton::system::Preferences pref;

struct less_mpec : public std::binary_function<char *, char *, bool> {
	bool operator()(const char *x, const char *y) const {
		if(isdigit(*x) && *y == ' ') return true;
		if(isdigit(*y) && *x == ' ') return false;

		return strcmp(x, y) < 0;
	}
};

void prepHeader(ostream &db)
{
	db << "# IJL VER " << SARecord::FORMAT_VERSION << "\n";

	const char *hf = pref["mpec.headers"];
	ifstream f(hf);
	char buf[1000];
	while(!f.eof()) {
		f.getline(buf, 1000);
		Util::trim(buf);
		if(buf[0] == 0) continue;

		cout << buf << "\n";
		db << "# " << buf << "\n";
	}

	// ACK header
	time_t t = ::time(NULL);
	strcpy(buf, ctime(&t));

	cout << "ACK Prepared using SDSS MPECReport on " << buf << "\n";
	db << "# " << "ACK Prepared using SDSS MPECReport on " << buf << "\n";
}

const double vCut = 0.03;

int main(int argc, char *argv[])
{
	PRINT_VERSION_IF_ASKED(argc, argv);

	const char *ws = System::workspace();

	if(argc != 3) {
		cout << "Description: Generate SDSS asteroid database files from .out files\n";
		cout << "Usage: " << argv[0] << " <out_file> <in_file>\n";
		return -1;
	}

	char infile[500], outfile[500];
	sprintf(infile,  "%s/output/%s.out", ws, argv[2]);
	sprintf(outfile, "%s/output/%s.sdss", ws, argv[1]);

	if(access(infile, R_OK) != 0) {
		Log::write("Cannot access input file %s", infile);
		return -1;
	}
	ifstream in(infile);

	// db file
	ofstream db(outfile);

	cout << "Input  : " << infile << "\n";
	cout << "Output : " << outfile << "\n";
	cout.flush();

	char buf[1000];

	IdentRecord r;
	Asteroid *a, catAst, undef;
	undef.numeration = 0; undef.type = 0; strcpy(undef.name, "Undef");

	// prepend header
//	prepHeader(db);

	std::vector<const char *> mpec;

	// counters
	int numerated = 0, det = 0, undet = 0, rejects = 0;
	double maxDate = -1E10, minDate = 1E10;

	while(!in.eof()) {
		if(!r.read(in)) continue;
		if(!r.oid) { r.oid = ObjectID::generate(r.run, r.observed.ra, r.observed.dec); }

		// load MPEC record
		SARecord m(r);

#if 0 // -- now relegated to input/output filters
		// reject anything perverse
		if(14.5 > m.r || m.r > 21.5) { rejects++; continue; }
		if(m.vmu*m.vmu + m.vnu*m.vnu < vCut*vCut) { rejects++; continue; }
#endif

		// statistics gathering
		if(r.numeration > 0) numerated++;
		if(!stricmp(r.name, "Undef")) undet++;
		else det++;

		if(m.time < minDate) minDate = r.time;
		if(m.time > maxDate) maxDate = r.time;
		if(fabs(m.vmu) > 2 || fabs(m.vnu) > 2) { cout << "ERROR !!!!: Velocities suck - " << r.oid << "\n"; }

		mpec.push_back(strdup(m.toString(buf)));
	}

	// sort MPEC records
//	std::sort(mpec.begin(), mpec.end(), less_mpec());

	// final sanity checks
	//   -- presume the data is all from the same run
	//       => (maxDate-minDate) < 1
	if(maxDate - minDate > .9) { cout << "AAAAAAAAAAAARGHHHHH !!!!\n"; }

	char m[81]; m[80] = 0;
	for(int i = 0; i != mpec.size(); i++) {
		db << mpec[i] << "\n";
		free((void *)mpec[i]);
	}
	
	cout << mpec.size() << " records written\n";
	cout << det << " identified\n";
	cout << undet << " unidentified\n";
	cout << numerated << " numerated asteroids\n";
	cout << rejects << " rejected because of weird parameter values\n";
}
