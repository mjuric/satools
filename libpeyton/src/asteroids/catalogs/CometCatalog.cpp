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
		MJD periTime = peyton::time::calToMJD(y, m, int(d), 24.*(d - int(d)));
//		periTime += d - int(d);
//		std::cerr << periTime << " " << d - int(d) << "\n";

		//         q                  e                   aop               lan                 i
		ss >> obj.elements[0] >> obj.elements[1] >> obj.elements[4] >> obj.elements[3] >> obj.elements[2];
		for(int i=2; i <= 4; ++i) { obj.elements[i] *= ctn::d2r; }
		if(obj.elements[1] >= .98) { continue; } // we cannot handle parabolic orbits (OrbFit doesn't support them)
		obj.elements[0] /= 1 - obj.elements[1]; // convert q -> a

		if(line.size() > 100)
		{
			// epoch of oscultation
			if(line[81] != ' ')
			{
				int d;
				sscanf(line.c_str() + 81, "%04d%02d%02d", &y, &m, &d);
				obj.t0 = peyton::time::calToMJD(y, m, d, 0.);
			}
			else
			{
				continue;
			}
			// H, G
			sscanf(line.c_str() + 91, "%lf %lf", &obj.h, &obj.g);
			
			// Comet name
			std::string desig = line.c_str() + 102;
			size_t k = desig.find('/');
			if(k != std::string::npos && k != 1) // e.g., 7P/Pons-Winnecke
			{
				// remember only 7P
				strncpy(obj.name, desig.substr(0, k).c_str(), Asteroid::maxNameLen);
			}
			else if(k == 1) // e.g., P/1994 J3 (Shoemaker)
			{
				// remember only 1994 J3
				obj.name[Asteroid::maxNameLen]=0;
				bool second_wspc = false;
				for(int i=0; i != Asteroid::maxNameLen; ++i)
				{
					char c = desig[i+2];
					if(c == ' ')
					{
						if(second_wspc)
						{
							obj.name[i]=0;
							break;
						}
						else
						{
							second_wspc = true;
						}
					}
					obj.name[i] = c;
				}
			}
			else	// unrecognized format -- copy as much as we can take
			{
				strncpy(obj.name, line.c_str() + 102, Asteroid::maxNameLen);
				obj.name[Asteroid::maxNameLen]=0;
			}
		}
		else
		{
			continue;
		}

		// calculate mean anomaly using GM=n^2*r^3  M=n*(T-T_0) (in radians)
		obj.elements[5] = (peyton::ctn::gk * std::pow(obj.elements[0], -3./2.)) * (obj.t0 - periTime);
		obj.elements[5] = remainder(obj.elements[5], peyton::ctn::pi2);

#if 0
		std::cerr << "[" << obj.numeration << "] [" << obj.name << "] [" << obj.h << "] [" << obj.g << "]" << "\n";
#endif

		// store the object
		obj.id = comets.size();
		cometNameMap[obj.name] = comets.size();
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
	typeof(cometNameMap.begin()) it = cometNameMap.find(name);
	if(it == cometNameMap.end()) { return -1; }

	obj = comets[it->second];
	return 0;
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
