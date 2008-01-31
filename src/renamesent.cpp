#include "mpecfile.h"

#include <astro/system/error.h>
#include <astro/system/preferences.h>
#include <astro/constants.h>
#include <astro/util.h>

#include <stdio.h>
#include <iomanip>

#include <algorithm>

Preferences pref;

#define stricmp strcasecmp

struct less_mpec : public std::binary_function<MPECRecord &, MPECRecord &, bool> {
	bool operator()(const MPECRecord &x, const MPECRecord &y) const {
		if(x.oid == y.oid) return x.band > y.band;
		return strcmp(x.name, y.name) < 0;
	}
};

using namespace std;
int main(int argc, char *argv[])
{
	
	MPECFile out("94.mpec.sdss");
	MPECFile unsent("94_unsent.mpec.sdss", 0);
	MPECFile sent("sent.mpec.sdss", 0);
	unsent.headers = out.headers;
	sent.headers = out.headers;
	MPECFile in("../mpecs/r.mpec.sdss", 1);

	int ren = 0;
	for(MPECFile::ii i = out.begin(); i != out.end(); i++) {
		MPECRecord &r = (*i);
		MPECRecord *o = in.at(r.p);
		if(o != NULL) {
			// sanity check - this must be the 'V' record
			if(r.band != 'V' && r.band != ' ') { cout << "PANIX : " << r.name << ":" << r.band << r.mag << "\n"; continue; }
			strcpy(r.name, o->name);
			r.oid = ObjectID(o->name);
			ren++;
			sent.push_back(r);
			// rename the following record, too
			i++;
			MPECRecord &rn = (*i);
			if(rn.band != 'B' && rn.band != ' ') { cout << "PANIX : " << rn.name << ":" << rn.band << rn.mag << "\n"; continue; }
			strcpy(rn.name, o->name);
			rn.oid = ObjectID(o->name);
			unsent.push_back(rn);
		} else {
			// new asteroid
			unsent.push_back(r);
		}
	}

	// sanity check 2 - B,V,B,V sequence ckeck
	int err;
/*	for(int i = 0; i < out.size(); i+=2) {
		cout << i << "\n";
		if(out[i].band == out[i+i].band && (out[i].band == 'V' || out[i].band == 'B' || out[i].band == ' ')) continue;
		cout << "PANIXXX: " << out[i].name << "  " << out[i+1].name << "\n";
	}
*/
	std::stable_sort(out.begin(), out.end(), less_mpec());
	std::stable_sort(sent.begin(), sent.end(), less_mpec());
	std::stable_sort(unsent.begin(), unsent.end(), less_mpec());

	cout << in.size() << " records in sent file.\n";
	cout << ren << " records changed.\n";

	// write
	sent.commit();
	cout << sent.size() << " records written (sent)\n";

	out.commit();
	cout << out.size() << " records written (complete)\n";

	unsent.commit();
	cout << unsent.size() << " records written (unsent)\n";
}
