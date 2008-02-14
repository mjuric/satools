#ifndef __orbfitlib_h
#define __orbfitlib_h

#include <memory.h>

inline void padd(char *dest, int len, const char *src) {
	memset(dest, 32, len);
	strcpy(dest, src);
}

class FortranUnits {
public:
	static int rep, ele, dif, eph;
	static void initialize() {
		rep = ele = dif = eph = 3;
	}
};

class OrbfitPropagLibrary {
protected:
	static bool libraryInitialized;
public:
	static void initialize();
};

// libsuit
extern "C" double tjm1_(int &iday, int &month, int &iyear, double &h);
extern "C" void libini_();
extern "C" void namini_();
extern "C" void filopl_(int &iun, char *name, int name_len);
extern "C" void filclo_(int &iun, char *status, int status_len);
extern "C" void rdnam_(int &iun);
extern "C" void chkkey_();
extern "C" void rdklst_(char *file, int file_len);

// libprop
extern "C" void rmodel_(char *run,
						int run_size
						);
extern "C" void preobs_(char *coo,
						const double &t0,
						const int &code,
						const double &t1,
						const double east0[],
						int const &iobs,

						double &x,
						double &y,

						const double &h,
						const double &g,
						
						double &hmagn,
						int coo_size
						);
extern "C" void proele_(char *coo, double &t0, double *east0, double &t1, double *east1, int coo_size);

// helper FORTRAN routines
extern "C" void preobs2_(char *coo,
						const double &t0,
						const int &code,
						const double &t1,
						const double east0[],
						const int &iobs,

						double &x,
						double &y,

						const double &h,
						const double &g,
						
						double &hmagn,
						int coo_size
						);

/* 
	Description: Returns heliocentric distance and phase of the asteroid from /phase/ COMMON block.
		Valid only after a call to one of the preobs routines (or alfdel)
*/
extern "C" void phvars_(double &R, 
			double &phse,
			double &umg,
			double &dist
						);

#endif
