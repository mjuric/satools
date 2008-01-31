#include <astro/asteroids/catalog.h>
#include <astro/system/preferences.h>

#include <time.h>

#include "observationcalculator.h"
#include "orbfitlib.h"

#include <iostream>

const int blockSize = 5000;

using namespace peyton::asteroids;
using namespace std;

peyton::system::Preferences pref;

int cnt = 0;

void filterCatalog(Catalog *cat, Catalog *out)
{
	time_t t0 = time(NULL);
	vector<Asteroid> o;
	int recordCount = cat->recordCount();
	int i = 0;
	double ele[6];
	while(i < recordCount) {
		int read = cat->read(o, i, i+blockSize);

		for(int j = 0; j != read; j++) {
			Asteroid &obj = o[j];

			if(obj.arc > 300) {
				obj.id = cnt;
				out->write(obj, -1);
				cnt++;
			}

			if((i+j) % 50 == 0) {
				double rps = double(i+j)/double(time(NULL) - t0);
				cout << "\n [" << i+j << "/" 
					<< double(recordCount - (i+j)) / rps
					<< "s] : ";
			}
			cout << "#";
		}

		i += read;
	}
}

main(int argc, char **argv)
{
	OrbfitPropagLibrary::initialize();

	if(argc != 3) {
		cout << "Description: Filters objects from the catalog according to the compiled in criteria\n";
		cout << "Usage: " << argv[0] << " <output_catalog> <input_catalog>\n";
		cout << "Input catalog must be in NATIVE format, output catalog will be in NATIVE format\n";
		return -1;
	}

	Catalog *cat = Catalog::open(argv[2], "NATIVE");
	Catalog *out = Catalog::open(argv[1], "NATIVE", "w");

	filterCatalog(cat, out);

	cout << "Filtered " << cnt << " out of " << cat->recordCount() << " objects\n";

	delete cat;
	delete out;
}
