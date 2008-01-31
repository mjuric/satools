
//--- libpeyton includes. Add any libpeyton includes _before_ including
//--- astro/useall.h

#include <astro/system/options.h>
#include <astro/io/printf.h>
#include <astro/math/vector.h>
#include <astro/math.h>
#include <astro/constants.h>
#include <astro/useall.h>

#include <iostream>

using namespace std;

namespace peyton {
namespace io {

template<>
void format_type(Formatter &f, const V3 &v)
{
	string fmt = f.front();

	     if(fmt == "%(abs)") { f.out << abs(v); }
	else if(fmt == "%(polar())") {
		f.out << io::format("[r=%f theta=%f phi=%f]") << abs(v) << deg(v.theta()) << deg(v.phi());
	}
	else if(fmt == "%(x)") { f.out << v.x; }
	else if(fmt == "%(y)") { f.out << v.y; }
	else if(fmt == "%(z)") { f.out << v.z; }
	else f.out << v;
}
/*
template<>
void format_type(Formatter &f, const Radians &v)
{
	string fmt = f.front();

	if(fmt == "%(deg)") { f.out << deg(v); }
	else { f.out << v; }
}
*/
}
}

int main(int argc, char **argv)
{
try
{
	VERSION_DATETIME(version);

	Options opts(
		"This program has not been described",
		version,
		Authorship::majuric
	);

	//# add any arguments your program needs. eg:
	//# opts.argument("run", "For which run should I generate the volume map.");

	// add any options your program might need. eg:
	// opts.option("meshFactor", "meshFactor", 0, "--", Option::required, "4", "Resolution decrease between radial steps");

	try {
		opts.parse(argc, argv);
	} catch(EOptions &e) {
		cout << opts.usage(argv);
		e.print();
		exit(-1);
	}

	/////// Start your application code here
	double r = ctn::pi;

	cout << io::format("abs(v) = %(abs) %f %2.1f\n") << V3(1, 2, 3) << r << r;

	io::Formatter fs(cout, "abs(v) = %2.1f %(polar()) %3.2f %f %@\n", true);
	fs << r << V3(1, 2, 3) << r << r << V3(1, 1, 1);
	fs << r << V3(1, 2, 3) << r << r << V3(1, 1, 1);

	std::string s = io::format("abs(v) = %(abs) %f %2.1f\n") << V3(1, 2, 3) << r << r;
	cout << s;
}
catch(EAny &e)
{
	e.print();
}
}
