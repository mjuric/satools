#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#ifdef __cplusplus
}
#endif

#include "astro/util.h"
#include "astro/time.h"
#include "astro/coordinates.h"
#include "astro/asteroids.h"

MODULE = LibPeyton::Util		PACKAGE = LibPeyton::Util		

void
format_coords(ra, dec, fmt)
	double ra
	double dec
	int fmt
PPCODE:
	if(fmt != 1 && fmt != 2) { croak("fmt=%d argument must be 1 (MPC_FORMAT) or 2 (NO_FRAC_SECONDS)", fmt); }

	char ras[100], decs[100];
	peyton::coordinates::formatCoord(ras, decs, ra, dec, fmt);

	EXTEND(SP, 2);
	PUSHs(sv_2mortal(newSVpv(ras, 0)));
	PUSHs(sv_2mortal(newSVpv(decs, 0)));

char *
format_coord(fmt, v)
	char *fmt
	double v
CODE:
	char vs[100];
	peyton::coordinates::formatCoord(fmt, vs, v);
	RETVAL = vs;
OUTPUT:
	RETVAL

double
approx_sun_longitude(t)
	double t
CODE:
	RETVAL=peyton::util::approxSunLongitude(t);
OUTPUT:
	RETVAL

char *
mpec_time_format(t)
	double t
CODE:
	char buf[100];
	peyton::time::toMPECTimeFormat(buf, t);
	RETVAL = buf;
OUTPUT:
	RETVAL

char *
packed_form(prov, orbitType=NULL)
	char *prov
	char *orbitType
CODE:
	char buf[100];
	peyton::asteroids::prov2packed(buf, prov, orbitType);
	RETVAL = buf;
OUTPUT:
	RETVAL



void
ymd(J)
	double J
PPCODE:
	int y, m; double d;
	peyton::time::MJD2cal(J, y, m, d);

	EXTEND(SP, 3);
	PUSHs(sv_2mortal(newSViv(y)));
	PUSHs(sv_2mortal(newSViv(m)));
	PUSHs(sv_2mortal(newSVnv(d)));

double
dayfrac(h, m, s)
	int h
	int m
	int s
CODE:
	RETVAL = peyton::time::dayfrac(h, m, s);
OUTPUT:
	RETVAL

double
mjd(y, m, d, h=0, min=0, s=0)
	int y
	int m
	double d
	int h
	int min
	int s
CODE:
	int dd = int(d);
	double dfrac = d - dd;
	RETVAL = peyton::time::calToMJD(y, m, dd, 24.*(dfrac + peyton::time::dayfrac(h, min, s)));
OUTPUT:
	RETVAL
