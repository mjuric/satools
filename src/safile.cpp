#include "safile.h"
#include <astro/system/error.h>
#include <astro/system/preferences.h>
#include <astro/constants.h>
#include <astro/util.h>

#include <stdio.h>

using namespace std;
using namespace peyton;
using namespace peyton::coordinates;
using namespace peyton::system;

SAFile::SAFile(const char *name, int level)
{
	strcpy(filename, name);

	problem = false;
	if(level == 0) return;

	SARecord r;
	ifstream f(name);

	char buf[1000];
	bool hdrsRead = false;
	int line = 0;
	while(!f.eof()) {
		f.getline(buf, 1000); line++;

		// first empty line designates header end
		if(!hdrsRead) {
			if(!isalpha(buf[0])) { hdrsRead = true; continue; }
			headers.push_back(buf);
			continue;
		}

		if(buf[0] == 0) continue; // skip over empty lines

		if(r.parse(buf) >= level) { push_back(r); continue; }
		if(f.eof()) break;

		if(hdrsRead) {
			problem = true;
			Log::debug(Log::terminate, "Problem reading MPEC.SDSS file %s at line %d", filename, line);
			break;
		}
	}
}

bool SAFile::commit()
{
	if(problem) return false;

	// backup
	char buf[1000];
	sprintf(buf, "%s~", filename);
	rename(filename, buf);

	// write
	ofstream f(filename);

	for(int i = 0; i != headers.size(); i++) {
		f << headers[i] << "\n";
	}

	f << "\n";

	for(int i = 0; i != size(); i++) {
		f << (*this)[i].toString(buf) << "\n";
	}
	
	return true;
}

SARecord *SAFile::at(SkyPoint p, double r)
{
	const double radius = r*ctn::s2r;

	// find objects withing 1 arcsec
	int cnt = 0;
	for(ii i = begin(); i != end(); i++) {
		if((*i).p.distance(p) < radius) {
			return &(*i);
		}
	}
	
	return NULL;
}
