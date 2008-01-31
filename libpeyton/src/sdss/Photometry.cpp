#include <astro/sdss/photometry.h>

using namespace peyton;

void sdss::johnson(double &V, double &B, double r, double g) throw()
{
#if 0
	// Fukugita
	r -= .11; g += .12;

	V = .4666667*g + .533333*r;
	B = 1.419048*g - .419048*r;
#endif

#if 0
	// Krisciunas
	r -= .10; g += .08;

	V = .5625*r + .4375*g;
	B = -.479166666*r + 1.47916666*g;
#endif

#if 1
	V = r + 0.44*(g-r) - 0.02;
	B = 1.04*(g-r) + 0.19 + V;
#endif

//	cout << r+.11 << " " << g-.12 << " " << V << " " << B << "\n";
}
