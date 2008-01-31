#include "sarecord.h"

#include <astro/sdss/photometry.h>
#include <astro/system/preferences.h>
#include <astro/util.h>
#include <astro/constants.h>
#include <astro/time.h>
#include <astro/system/error.h>
#include <astro/compat/compat.h>
#include <astro/coordinates.h>

#include <astro/sdss/rungeometry.h>

#include <ctype.h>
#include <stdio.h>
#include <math.h>

#include <iomanip>

using namespace peyton;
using namespace peyton::sdss;
using namespace peyton::coordinates;

extern peyton::system::Preferences pref;

char SARecord::FORMAT_VERSION[20] = "20011112";

SARecord::SARecord()
{
	memset(this, 0, sizeof(*this));
}

int SARecord::parse(char *s)
{
	if(strlen(s) < 80) return 0;

//	cout << "Parsing : " << s << "\n";

//	char detNum_s[6]; detNum_s[5] = 0;
	char objid[7];
	oe_cat[0] = 0;
	int n = sscanf(s, "%s %d %d %d %d %lf %lf "
			"%lf %lf %lf %lf %lf %lf "
			"%d %d %s %d %d %x "
			"%lf %lf %lf %lf %lf %lf "
			"%lf %lf "
			"%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf "
			"%lf %lf %lf "
			"%lf %lf %lf "
			"%s %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf "
			"%s %lf %lf %lf",
		&run, &col, &field, &id, &rowc, &colc, objid, 
		&time, &p.ra, &p.dec, &lambda, &beta, &phi,
		&isIdentified, &detNum, detName, &iapp, &apparences, &flags, 
		&vmu, &vmuErr, &vnu, &vnuErr, &vra, &vdec,
		&V, &B,
		&u, &uErr, &g, &gErr, &r, &rErr, &i, &iErr, &z, &zErr, &b, &bErr,
		&calculated.ra, &calculated.dec, &magc,
		&R, &dist, &phase,
		oe_cat, &H, &G, &arc,
		&epoch, &a, &e, &inc, &lan, &aop, &M,
		pe_cat, &ap, &ep, &sinip);
//	detName[20] = 0;

//	cout << "N: " << oe_cat << "|" << eqName << "|" << n << "\n"; cout.flush();

	if(n != 58) return 0;

//	objid[6] = 0; eqName[6] =0; oe_cat[20] = 0; pe_cat[20] = 0;
	p.toRad(); calculated.toRad(); RAD(inc); RAD(lan); RAD(aop); RAD(M);
	RAD(lambda); RAD(beta); RAD(phi); RAD(phase);

	oid = objid;
	util::trim(detName);
//	detNum = atoi(detNum_s);

	return 2;
}

inline bool magLimits(double &m, const double lo = 0, const double hi = 40) { if(!(lo < m && m < hi)) { m = 99.99; return true; } return false; }
inline bool magErrLimits(double &m) { if(!(0 < m && m < 10)) { m = 9.99; return true; } return false; }

SARecord::SARecord(IdentRecord &r)
{
	memset(this, 0, sizeof(*this));
	
	// astrometry
	time = r.time;
	p = r.observed;

	// identification
	detNum = r.numeration < 1 ? 0 : r.numeration;
	if(!stricmp(r.name, "Undef")) {
		strcpy(detName, "-");
		isIdentified = 0;
	} else {
		strcpy(detName, r.name);
		isIdentified = 1;
	}
//	strcpy(eqName, "-");
	oid = r.oid;
//	strcpy(name, oid);
//	numeration = 0;
//	asterix = '*';

	// photometry
//	if((0 < r.r && r.r < 23) && (0 < r.g && r.g < 23)) {
		sdss::johnson(V, B, r.r, r.g);
//		mag = V;
//		band = 'V'; // Johnson-Morgan-Cousins V band
//	} else {
//		mag = 99.9; 
//		V = 99.9; B = 99.9;
//		band = ' ';
//	}

//	method = 'C';
//	note = ' ';
//	orbitType = ' ';
//	obsCode = pref["observatory.code"];

	// sdss
	run = r.run; col = r.camCol; field = r.field; id = r.id;

	u = r.u; g = r.g; this->r = r.r; i = r.i; z = r.z;
	uErr = r.uErr; gErr = r.gErr; rErr = r.rErr; iErr = r.iErr; zErr = r.zErr;

	// a* color
	b = 0.89*(r.g-r.r) + 0.45*(r.r-r.i) - 0.57;
	bErr = sqrt(sqr(0.89*gErr) + sqr(0.44*rErr) + sqr(0.45*iErr));

	// check magnitude limits
	int i;
	double *mags = &u; for(i = 0; i != 5; i++) { magLimits(mags[i]); }
	mags = &uErr; for(i = 0; i != 5; i++) { magErrLimits(mags[i]); }
	magLimits(b, -3, 3); magErrLimits(bErr);
	magLimits(V);
	magLimits(B);

	vmu = r.vmu; vnu = r.vnu; vmuErr = r.vmuErr; vnuErr = r.vnuErr;
	vra = r.vra; vdec = r.vdec;
	rowc = r.rowc; colc = r.colc;

	// identification data
	if(isIdentified) {
		calculated = r.calculatedT;
		magc = r.magc;
		R = r.R; dist = r.dist; phase = r.phase;
	}
	lambda = r.lambda; beta = r.beta; phi = r.phi;
//	cout << lambda << " " << beta << " " << phi << "\n";

	apparences = 1;
	iapp = 0;
	flags = 0;
//	matcher = 's';

	// leave the orbital and proper elements to be filled in by subsequent scripts
	oe_cat[0] = pe_cat[0] = '-';
	oe_cat[1] = pe_cat[1] = 0;
}


//     J98W09N  C1998 12  6.90676 03 25 01.60  18 20 24.4          17.1        120
char *SARecord::toMPECString(char *s, char band)
{
	char time_s[100], ra_s[100], dec_s[100], mag_s[10], number_s[20];
	s[0] = 0;

	peyton::time::toMPECTimeFormat(time_s, time);
	coordinates::formatCoord(ra_s, dec_s, p.ra/ctn::d2r, p.dec/ctn::d2r, FC_MPEC_FORMAT);

	double mag;
	switch(band) {
	case 'V': mag = V; break;
	case 'B': mag = B; break;
	case 'u': mag = u; break;
	case 'g': mag = g; break;
	case 'r': mag = r; break;
	case 'i': mag = i; break;
	case 'z': mag = z; break;
	}

	if(mag != 99.9) {
		sprintf(mag_s, "%04.1f", V);
	} else {
		mag_s[0] = 0;
	}

	number_s[0] = 0;

	char desig[20];
	strcpy(desig, detName[0] == '-' ? (const char *)oid : detName);

	char asterix = (detName[0] == '-') ? '*' : ' ';
	char note = ' ',  method = 'C'; int obsCode = pref["observatory.code"];

	sprintf(s, "%5s%-7s%c%c%c%s %s %s          %4s %c      %d",
		number_s, desig,
		asterix, note, method, time_s, ra_s, dec_s, mag_s, band, obsCode);

	return s;
}

char *SARecord::toString(char *s)
{
	s[0] = 0;

	// moving object record
	char buf[200];
	sprintf(buf, "%-6s %5d %1d %4d %5d %8.3f %8.3f  ", (const char *)oid, run, col, field, id, rowc, colc);
	strcat(s, buf);

	// time in MJD and position in deg (equatorial and ecliptic)
	sprintf(buf, " %12.5f %10.6f %10.6f %10.6f %10.6f %11.6f ", time, p.ra/ctn::d2r, p.dec/ctn::d2r, lambda/ctn::d2r, beta/ctn::d2r, phi/ctn::d2r);
	strcat(s, buf);

      // check for insane velocities, and undef them if they don't fit the format
	if(	fabs(vmu) >= 10 || fabs(vnu) >= 10 ||
		fabs(vra) >= 10 || fabs(vdec) >= 10 ||
		fabs(vmuErr) >= 10 || fabs(vnuErr) >= 10
	) { vmu = vnu = vmuErr = vnuErr = vra = vdec = 9.9999; }

	// velocities
	sprintf(buf, " %+6.4f %6.4f %+6.4f %6.4f %+6.4f %+6.4f ", vmu, vmuErr, vnu, vnuErr, vra, vdec);
	strcat(s, buf);

	// SDSS magnitudes
	sprintf(buf, " %5.2f %4.2f %5.2f %4.2f %5.2f %4.2f %5.2f %4.2f %5.2f %4.2f %5.2f %4.2f ",
		u, uErr, g, gErr, r, rErr, i, iErr, z, zErr, b, bErr);
	strcat(s, buf);

	// Johnson magnitudes
	sprintf(buf, " %5.2f %5.2f ", V, B);
	strcat(s, buf);
	

	
	// identification information
	sprintf(buf, "%1d %7d %-20s %2d %2d %08x ", isIdentified, detNum, detName, iapp, apparences, flags);
	strcat(s, buf);

	// identification data
	sprintf(buf, " %10.6f %10.6f %5.2f ", calculated.ra/ctn::d2r, calculated.dec/ctn::d2r, magc);
	strcat(s, buf);
	sprintf(buf, " %7.3f %7.3f %5.2f ", R, dist, phase/ctn::d2r);
	strcat(s, buf);



	// orbital elements
	sprintf(buf, " %-20s %5.2f %4.2f %5.0f ", oe_cat, H, G, arc);
	strcat(s, buf);
	sprintf(buf, " %12.6f %12.8f %10.8f %10.6f %10.6f %10.6f %10.6f ", epoch, a, e, inc/ctn::d2r, lan/ctn::d2r, aop/ctn::d2r, M/ctn::d2r);
	strcat(s, buf);

	// proper elements
	sprintf(buf, " %-20s %12.8f %10.8f %10.6f", pe_cat, ap, ep, sinip);
	strcat(s, buf);
	
	return s;
}

void SARecord::prettyPrint(std::ostream &o)
{
	o << std::setiosflags(std::ios::fixed);

	o << "Name : " << oid;
	if(detNum > 0) o << " (" << detNum << ")";
	o << "\n";
	o << "SDSS : " << oid << " [" << run << " " << col << " " << field << " " << id << "]\n";
	o << "Det  : " << detName;
	if(detNum > 0) o << " (" << detNum << ")";
	o << "\n";
	int y, mon, day, h, m, s; double dd;
	Time::MJD2cal(time, y, mon, dd);
	day = int(dd); dd -= day; dd *= 24; h = int(dd); dd -= h; dd *= 60.0; m = int(dd); dd -= m; dd *=60.0; s = (int)rint(dd);
	o << "Time : " << std::setprecision(5) << time << " [" << y << "/" << mon << "/" << day << " " << h << ":" << m << ":" << s << "]\n";
	o << "Pos  : " << std::setprecision(6) << p.ra/ctn::d2r << " " << p.dec/ctn::d2r << "\n";
	o << "Vel  : " << std::setprecision(4) << "      vmu = " << vmu << " +-" << vmuErr << "\n";
	o << "     : " << "     vnu = " << vnu << " +-" << vnuErr << "\n";
	o << "Mag  : " << "        V = " << V << "\n";
	o << "     : " << "u g r i z = " << std::setprecision(2) << u << " " << g << " " << r << " " << i << " " << z << "\n";
	o << "     : " << "   errors = " << std::setprecision(2) << uErr << " " << gErr << " " << rErr << " " << iErr << " " << zErr << "\n";
//	o << "Flgs : " << "[" << asterix << "|" << note << "]\n";
}
