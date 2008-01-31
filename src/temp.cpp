#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <iostream>

bool main()
{
	char *s = "     s00e9a * C1998 09 19.27223 23 21 55.69 -00 55 57.1          19.8 V      645    94 1  104 s00e9a     0 -                    -0.0542 +0.0016 -0.0235 +0.0016  21.76 0.18 20.12 0.02 19.46 0.02 19.22 0.02 19.15 0.07";

	if(strlen(s) < 80) return false;

	char orbitType;

	// parse MPEC string/extended SDSS records
	// check for typeoforbit flag
	orbitType = isdigit(s[4]) ? ' ' : s[4];

	char num_s[100], sign;
	int y, mon, h, rm, d, dm;
	double day, rs, ds, mag;
	int n;

	char asterix, note, method, band;
	int code;
	char designation[20];
	double u, uErr, g, gErr, r, rErr, i, iErr, z, zErr, vra, vraErr, vdec, vdecErr;
	int run, col, field;
	char oid[7]; char detName[20]; int detNum;

	designation[7] = 0;
	n = sscanf(s, "%5c%7c%c%c%c%4d %2d %8lf %2d %2d %5lf %c%2d %2d %4lf%*10c%4lf %c%*6c%3d",
		num_s, designation, &asterix, &note, &method,
		&y, &mon, &day, &h, &rm, &rs, &sign, &d, &dm, &ds,
		&mag, &band, &code);
	// 18 for MPEC
	n = sscanf(s+80, " %5d %1d %4d %6c %5d %20c %lf %lf %lf %lf  %5lf %4lf %5lf %4lf %5lf %4lf %5lf %4lf %5lf %4lf",
		&run, &col, &field, oid, &detNum, detName,
		&vra, &vraErr, &vdec, &vdecErr,
		&u, &uErr, &g, &gErr, &r, &rErr, &i, &iErr, &z, &zErr);
	oid[6] = 0; detName[20] = 0; // 18 + 20 for SDSS

	switch(orbitType) {
	case ' ': num_s[5] = 0; break;
	default : num_s[4] = 0; break;
	}

	cout <<
		num_s << "|" <<  designation << "|" <<  asterix << "|" <<  note << "|" <<  method << "|" << 
		y << "|" <<  mon << "|" <<  day << "|" <<  h << "|" <<  rm << "|" <<  rs << "|" <<  sign << "|" << d << "|" <<  dm << "|" <<  ds << "|" << 
		mag << "|" <<  band << "|" <<  code << "\n";
	cout <<
			run << "|" << col << "|" << field << "|" << oid << "|" << detNum << "|" << detName << "|" <<
			vra << "|" << vraErr << "|" << vdec << "|" << vdecErr << "|" <<
			u << "|" << uErr << "|" << g << "|" << gErr << "|" << r << "|" << rErr << "|" << i << "|" << iErr << "|" << z << "|" << zErr
			<< "\n";
	cout << n << "\n";
}
