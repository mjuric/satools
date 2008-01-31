#include "orbfitlib.h"

#include <astro/system/log.h>

using namespace peyton::system;

int FortranUnits::rep, FortranUnits::ele, FortranUnits::dif, FortranUnits::eph;

void initializeOrbfit()
{
	int iunit;
	char defFile[80] = "propag.def";

	FortranUnits::initialize();

	libini_();
	namini_();

	filopl_(iunit, defFile, strlen(defFile));
	rdnam_(iunit);
	filclo_(iunit," ", 1);

	// read keylists
	char keylist[80] = "fitobs.key";
	rdklst_(keylist, strlen(keylist));
	chkkey_();

	char run[80];
	padd(run, 80, "BLA 22");
	rmodel_(run, strlen(run));
}

bool OrbfitPropagLibrary::libraryInitialized = false;
void OrbfitPropagLibrary::initialize()
{
	if(!libraryInitialized) {
		Log::write("Initializing propag library");
		initializeOrbfit();
		libraryInitialized = true;
	}
}
