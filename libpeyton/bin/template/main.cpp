
//--- libpeyton includes. Add any libpeyton includes _before_ including
//--- astro/useall.h

#include <astro/system/options.h>
#include <astro/useall.h>

#include <iostream>

using namespace std;

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
	
}
catch(EAny &e)
{
	e.print();
}
}
