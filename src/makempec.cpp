#include <astro/time.h>
#include <astro/types.h>
#include <astro/util.h>
#include <astro/system/preferences.h>
#include <astro/asteroids/catalog.h>
#include <astro/system/error.h>
#include <astro/constants.h>

#include <algorithm>

#include <string.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include "sarecord.h"

#include <iostream>

#define stricmp strcasecmp

using namespace std;
using namespace peyton;
using namespace peyton::asteroids;

peyton::system::Preferences pref;

struct less_mpec : public std::binary_function<char *, char *, bool> {
	bool operator()(const char *x, const char *y) const {
		if(isdigit(*x) && *y == ' ') return true;
		if(isdigit(*y) && *x == ' ') return false;

		return strcmp(x, y) < 0;
	}
};

void prepHeader(ostream &db, const char *mpecFile)
{
	const char *hf = pref["mpec.headers"];
	ifstream f(hf);
	char buf[1000];
	while(!f.eof()) {
		f.getline(buf, 1000);
		Util::trim(buf);
		if(buf[0] == 0) continue;

		cout << buf << "\n";
		db << buf << "\n";
	}

	// ACK header
	time_t t = ::time(NULL);
	strcpy(buf, ctime(&t));
	while(buf[strlen(buf)-1] == '\n' || buf[strlen(buf)-1] == '\r') buf[strlen(buf)-1] = 0;

	cout << "ACK Prepared using SDSS makeMPEC on " << buf << ", " << mpecFile << "\n";
	db << "ACK Prepared using SDSS makeMPEC on " << buf << ", " << mpecFile << "\n";
	db << "\n";
}

int main(int argc, char *argv[])
{
	if(argc != 3) {
		cout << "Description: Generate MPC reports from SDSS asteroid database files\n";
		cout << "Usage: " << argv[0] << " <out_file.mpec> <in_file.sdss>\n";
		return -1;
	}

	// input file
	ifstream in(argv[2]);

	// MPEC report file
	ofstream db(argv[1]);
	prepHeader(db, argv[1]);

	char buf[1000];

	SARecord r;
	Asteroid *a, catAst, undef;
	undef.numeration = 0; undef.type = 0; strcpy(undef.name, "Undef");

	std::vector<const char *> mpec;

	// counters
	int numerated = 0, det = 0, undet = 0, rejects = 0;

	while(!in.eof()) {
		in.getline(buf, 1000);
		if(buf[0] == '#' || !r.parse(buf)) { continue; }

		if(!(r.flags & SAFlags::MPCSubmitted)) {
			mpec.push_back(strdup(r.toMPECString(buf, 'V')));
			mpec.push_back(strdup(r.toMPECString(buf, 'B')));
		}
	}

	// sort MPEC records
//	std::sort(mpec.begin(), mpec.end(), less_mpec());

	char m[81]; m[80] = 0;
	for(int i = 0; i != mpec.size(); i++) {
		db << mpec[i] << "\n";
		free((void *)mpec[i]);
	}
}
