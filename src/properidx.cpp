#include <stdio.h>

#include <iomanip>
#include <iostream>
#include <fstream>

#include <astro/system/preferences.h>
#include <astro/coordinates.h>

#include <astro/asteroids/catalog.h>

using namespace peyton::asteroids;
using namespace std;

peyton::system::Preferences pref;

main(int argc, char **argv)
{
	// finds the ASTORB indices of proper elements given in .pro file
	// used for easy merging of proper elements data later in SM

	Catalog *cat = Catalog::open("94.obj", "NATIVE");
	
	char buf[100];
	char n1[5], n2[5], name[50];
	n1[4] = n2[4] = 0;

	{
		ifstream f("allnum.pro");

		// header skipping
		for(int i = 0; i != 3; i++) { f.getline(buf, 100); }

		Asteroid a;
		while(!f.eof()) {
			f.getline(buf, 100);
			int num;
			if(sscanf(buf, "%d", &num) == 1) {
				if(cat->read(a, num-1) != -1) {
					cout << a.id << " " << buf << "\n";
				}
			}
		}
	}
	

	{
		ifstream f("ufitobs.pro");

		// header skipping
		for(int i = 0; i != 3; i++) { f.getline(buf, 100); }

		Asteroid a;
		while(!f.eof()) {
			f.getline(buf, 100);
			if(sscanf(buf, "%4c%4c", n1, n2) == 2) {
				sprintf(name, "%s_%s", n1, n2);
				while(name[strlen(name)-1] == ' ') name[strlen(name)-1] = 0;
				//cout << name << "\n";
				if(cat->read(a, name) != -1) {
					cout << a.id << " " << buf << "\n";
				}
			}
		}
	}
	
}
