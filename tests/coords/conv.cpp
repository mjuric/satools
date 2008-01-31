#include <astro/coordinates.h>
#include <astro/constants.h>

#include <iostream>
#include <iomanip>

#include <stdio.h>

#include <math.h>

using namespace std;

void main()
{
	Radians ra, dec, nu, mu, node, inc;
	int run;

	char *line = "    756  117.406351  -1.056300   51259.121345   51259.449765 287.957300   0.005886 117.406458  -1.055333";
	sscanf(line, " %d %lf %lf %*lf %*lf %lf %lf %*lf %*lf", &run, &ra, &dec, &node, &inc);
	cout << "[" << run << " " << ra << " " << dec << " " << node << " " << inc << "]\n";

	ra = 214.717581; dec = -0.440989;

	ra *= ctn::d2r;
	dec *= ctn::d2r;
	node *= ctn::d2r;
	inc *= ctn::d2r;
	Coordinates::equgcs(node, inc, ra, dec, mu, nu);
	if(mu < 0) mu +=ctn::pi2;
	mu /= ctn::d2r;
	nu /= ctn::d2r;

//	cout << setprecision(10) << "mu: " << mu << "\n";
//	cout << setprecision(10) << "nu: " << nu << "\n";

	// Reverse
	mu *= ctn::d2r;
	nu *= ctn::d2r;
	node *= ctn::d2r;
	inc *= ctn::d2r;
	Coordinates::gcsequ(node, inc, ra, dec, mu, nu);
	if(ra < 0) mu +=ctn::pi2;
	ra /= ctn::d2r;
	dec /= ctn::d2r;

//	cout << setprecision(10) << "ra: " << ra << "\n";
//	cout << setprecision(10) << "dec: " << dec << "\n";



	// Velocity conversion
	node = 0 * ctn::d2r; inc = 23.439291 * ctn::d2r;
//	node = 20 * ctn::d2r; inc = 20 * ctn::d2r;
	mu = 10 * ctn::d2r; nu = 20 * ctn::d2r;
	Radians vmu = .5 * ctn::d2r, vnu = 0 * ctn::d2r;
	Coordinates::gcsequ(node, inc, mu, nu, ra, dec);

	cout << "\n";
//	return -1;

	Radians vra, vdec;
	cout << setprecision(10) << "v: " << sqrt(vmu*vmu + vnu*vnu) / ctn::d2r << "\n\n";

	printf("%f %f : mu=%f nu=%f : ra=%f dec=%f : vmu=%f vnu=%f\n", node / ctn::d2r, inc / ctn::d2r, mu / ctn::d2r, nu / ctn::d2r, ra / ctn::d2r, dec / ctn::d2r, vmu / ctn::d2r, vnu / ctn::d2r);
	printf("eqvel2ecvel2 %f %f %f %f %f %f vra vdec\n", mu / ctn::d2r, nu / ctn::d2r, vmu / ctn::d2r, vnu / ctn::d2r, ra / ctn::d2r, dec / ctn::d2r);

	Coordinates::rot_vel(node, inc, mu, nu, vmu, vnu, vra, vdec);
	cout << setprecision(10) << "vra: " << vra / ctn::d2r << "\n";
	cout << setprecision(10) << "vdec: " << vdec / ctn::d2r << "\n";
	cout << setprecision(10) << "v: " << sqrt(vdec*vdec + vra*vra) / ctn::d2r << "\n";

	cout << "\nReverse:\n";

	Coordinates::rot_vel(node, -inc, ra, dec, vra, vdec, vmu, vnu);
	cout << setprecision(10) << "vra: " << vmu / ctn::d2r << "\n";
	cout << setprecision(10) << "vdec: " << vnu / ctn::d2r << "\n";
	cout << setprecision(10) << "v: " << sqrt(vmu*vmu + vnu*vnu) / ctn::d2r << "\n";

}
