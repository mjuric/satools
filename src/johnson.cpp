#include <iostream>
#include <stdlib.h>
#include <astro/photometry.h>

int main(int argc, char *argv[])
{
	if(argc < 3) {
		cout << "Usage: " << argv[0] << " <g> <r>\n";
		return -1;
	}
	
	double g = atof(argv[1]);
	double r = atof(argv[2]);

//	g += r;

	double B, V;
	Photometry::johnson(V, B, r, g);
	
	cout << V << "  " << B << "\n";
}
