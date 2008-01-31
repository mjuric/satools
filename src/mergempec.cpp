#include "safile.h"

#include <astro/system/error.h>
#include <astro/system/preferences.h>
#include <astro/constants.h>
#include <astro/util.h>

#include <stdio.h>
#include <iomanip>

Preferences pref;

#define stricmp strcasecmp

using namespace std;
int main(int argc, char *argv[])
{
	if(argc < 3) {
		cout << "Description : Merges a number of .mpec.sdss files into one big file, discarding the headers\n";
		cout << "Usage       : " << argv[0] << " <dest.sdss> <source1.mpec> [ <source2.mpec> [...]]\n";
		return -1;
	}

	char *dest = argv[1];
	
	MPECFile out(argv[2]);
	if(out.problem) { Error::error(-1, "Problem loading MPEC.SDSS file. Bailing out."); return -1; }
	cout << out.size() << " records loaded\n";
	
	// redirect to new file
	strcpy(out.filename, dest);

	// concatenate	
	for(int i = 3; i != argc; i++) {
		if(!stricmp(argv[i], dest)) continue;
		
		MPECFile m1(argv[i]);
		if(m1.problem) { Error::warning(-1, "Problem loading %s file. Ignoring.", argv[i]); continue; }
		cout << m1.size() << " records loaded\n";
		
		out.insert(out.end(), m1.begin(), m1.end());
	}
	
	// write
	out.commit();
	cout << out.size() << " records written\n";
}
