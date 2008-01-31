#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#ifdef __cplusplus
}
#endif

#include "astro/coordinates.h"

static int
not_here(char *s)
{
    croak("%s not implemented on this architecture", s);
    return -1;
}

static double
constant(char *name, int len, int arg)
{
    errno = EINVAL;
    return 0;
}

MODULE = LibPeyton::Coordinates		PACKAGE = LibPeyton::Coordinates		


double
constant(sv,arg)
    PREINIT:
	STRLEN		len;
    INPUT:
	SV *		sv
	char *		s = SvPV(sv, len);
	int		arg
    CODE:
	RETVAL = constant(s,len,arg);
    OUTPUT:
	RETVAL


void
transform_pos(node, inc, ra, dec)
	double node
	double inc
	double ra
	double dec
PPCODE:
	peyton::Radians mu, nu;
	peyton::coordinates::equgcs(node*peyton::ctn::d2r, inc*peyton::ctn::d2r, ra*peyton::ctn::d2r, dec*peyton::ctn::d2r, mu, nu);
	mu /=peyton::ctn::d2r;
	nu /=peyton::ctn::d2r;
	EXTEND(SP, 2);
	PUSHs(sv_2mortal(newSVnv(mu)));
	PUSHs(sv_2mortal(newSVnv(nu)));

void
equ_gal(ra, dec)
	double ra
	double dec
PPCODE:
	peyton::Radians l, b;
	peyton::coordinates::equgal(ra*peyton::ctn::d2r, dec*peyton::ctn::d2r, l, b);
	l /=peyton::ctn::d2r;
	b /=peyton::ctn::d2r;
	if(l > 360.) l -= int(l/360.)*360.;
	if(l < 0.) l += (int(l/360.)+1)*360.;
	EXTEND(SP, 2);
	PUSHs(sv_2mortal(newSVnv(l)));
	PUSHs(sv_2mortal(newSVnv(b)));

void
gal_equ(l, b)
	double l
	double b
PPCODE:
	peyton::Radians ra, dec;
	peyton::coordinates::galequ(l*peyton::ctn::d2r, b*peyton::ctn::d2r, ra, dec);
	ra /=peyton::ctn::d2r;
	dec /=peyton::ctn::d2r;
	if(ra > 360.) ra -= int(ra/360.)*360.;
	if(ra < 0.) ra += (int(ra/360.)+1)*360.;
	EXTEND(SP, 2);
	PUSHs(sv_2mortal(newSVnv(ra)));
	PUSHs(sv_2mortal(newSVnv(dec)));

double
angular_dist(ra1, dec1, ra2, dec2)
	double ra1
	double dec1
	double ra2
	double dec2
CODE:
	RETVAL = peyton::Coordinates::distance(ra1*peyton::ctn::d2r, dec1*peyton::ctn::d2r, ra2*peyton::ctn::d2r, dec2*peyton::ctn::d2r)/peyton::ctn::d2r;
OUTPUT:
	RETVAL

void
transform_vel(node, inc, mu, nu, vmu, vnu)
	double node
	double inc
	double mu
	double nu
	double vmu
	double vnu
PPCODE:
	peyton::Radians vra, vdec;
	peyton::coordinates::rot_vel(node*peyton::ctn::d2r, inc*peyton::ctn::d2r, mu*peyton::ctn::d2r, nu*peyton::ctn::d2r, vmu*peyton::ctn::d2r, vnu*peyton::ctn::d2r, vra, vdec);
	vra /=peyton::ctn::d2r;
	vdec /=peyton::ctn::d2r;
	EXTEND(SP, 2);
	PUSHs(sv_2mortal(newSVnv(vra)));
	PUSHs(sv_2mortal(newSVnv(vdec)));

int
is_bounded_by(ra, dec, ra_lo, dec_lo, ra_hi, dec_hi)
	double ra
	double dec
	double ra_lo
	double dec_lo
	double ra_hi
	double dec_hi
CODE:
	RETVAL = peyton::Coordinates::inBox(ra*peyton::ctn::d2r, dec*peyton::ctn::d2r, ra_lo*peyton::ctn::d2r, dec_lo*peyton::ctn::d2r, ra_hi*peyton::ctn::d2r, dec_hi*peyton::ctn::d2r);
OUTPUT:
	RETVAL
