#include <ifstream>

using namespace std;

class IdentRecord {
public:
	int run, camCol, field, sloanId;
	int ast; char name[20];
	
	MJD time
	
	Radians rao, deco;
	double u, g, r, i, z, uErr, gErr, rErr, iErr, zErr;
	
	double vrao, vdeco, vraoErr, vdecoErr;
	
	Radians rac, decc;
	double magc;
	
	double vrac, vdecc;
	
	double raErr, decErr;
	
	double h, g, arc, epoch, a, e, i, lan, aop, M;
	
	double R, phase;
	
	double lambda, beta, phi;
};
