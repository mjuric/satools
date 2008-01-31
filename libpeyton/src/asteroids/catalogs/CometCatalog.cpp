#include <astro/constants.h>
#include <astro/time.h>
#include <astro/exceptions.h>
#include <astro/system/log.h>

#include <stdio.h>
#include <iostream>
#include <sstream>
#include <string>
#include <fstream>

#include "CometCatalog.h"

using namespace peyton;
using namespace peyton::exceptions;
using namespace peyton::asteroids;

bool CometCatalog::openCatalog(const char *filename, const char *mode)
{
	std::ifstream f(filename);
	if(!f) { DEBUG(basic, "Error opening catalog"); return false; }

	Asteroid obj;
	std::string line;

	// load the entire catalog to memory
	while(std::getline(f, line))
	{
		obj.numeration = atoi(line.substr(0, 4).c_str());
		std::istringstream ss(line.substr(14));
		// perihelion passage time
		int y, m; double d;
		ss >> y >> m >> d;
		MJD epochTime = 0., periTime = peyton::time::calToMJD(y, m, int(d), d - int(d));

		//         q                  e                   aop               lan                 i
		ss >> obj.elements[0] >> obj.elements[1] >> obj.elements[4] >> obj.elements[3] >> obj.elements[2];
		if(obj.elements[1] == 1.) { continue; } // we cannot handle parabolic orbits (OrbFit doesn't support them)
		obj.elements[0] /= 1 - obj.elements[1]; // convert q -> a

		if(line.size() > 100)
		{
			// epoch of oscultation
			if(line[81] != ' ')
			{
				int d;
				sscanf(line.c_str() + 81, "%04d%02d%02d", y, m, d);
				epochTime = peyton::time::calToMJD(y, m, d, 0.);
			}
			else
			{
				continue;
			}
			// H, G
			sscanf(line.c_str() + 91, "%f %f", obj.h, obj.g);
		}
		else
		{
			continue;
		}
		
		// calculate mean anomaly
		obj.elements[5] = (peyton::ctn::gk * std::pow(obj.elements[0], -3.)) * (epochTime - periTime);
		obj.elements[5] = remainder(obj.elements[5], peyton::ctn::pi2);

		// store the object
		comets.push_back(obj);
	}

	return true;
}

int CometCatalog::write(Asteroid &obj, int at)
{
	THROW(ENotImplemented, "Not implemented yet !");
	return -1;
}

char *CometCatalog::identify(char *name) {
	strcpy(name, "COMET");
	return name;
}


int CometCatalog::read(Asteroid &obj, const char *name)
{
	THROW(ENotImplemented, "Not implemented yet !");
	return -1;
}

int CometCatalog::read(Asteroid &obj, const int id)
{
	if(id < 0 || id >= comets.size())
	{
		return -1;
	}

	obj = comets[id];

	return 0;
}

int CometCatalog::read(std::vector<Asteroid> &aobj, const int from_in, const int to_in)
{
	int from = from_in;
	int to = to_in <= comets.size() ? to_in : comets.size();

	aobj.clear();
	aobj.insert(aobj.begin(), comets.begin() + from, comets.begin() + to);

	return aobj.size();
}

int CometCatalog::recordCount() { return comets.size(); }

CometCatalog::~CometCatalog() { }
