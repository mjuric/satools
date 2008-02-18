#include "version.h"

#include "identrecord.h"
#include "sarecord.h"

#include <string>
#include <iostream>

#include <astro/system/preferences.h>

using namespace std;

void convertdb(int from, int to);

peyton::system::Preferences pref;

int main(int argc, char **argv)
{
	PRINT_VERSION_IF_ASKED(argc, argv);

	string b("binary"), a("ascii");

	if(argc != 3 ||
		argv[1] != b && argv[1] != a ||
		argv[2] != b && argv[2] != a
	) {
		cout << "Usage: oiddbconvert <binary|ascii> <ascii|binary>\n";
		return -1;
	}

	convertdb(argv[1] == b, argv[2] == b);

	return 0;
}
