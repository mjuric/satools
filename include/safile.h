#ifndef __safile_h
#define __safile_h

#include <vector>
#include <string>

#include "sarecord.h"

class SAFile : public std::vector<SARecord> {
public:
	std::vector<std::string> headers;
public:
	typedef SAFile::iterator ii;
	char filename[1000];
	bool problem;
public:
	SAFile(const char *name, int level = 2);

	SARecord *at(peyton::coordinates::SkyPoint p, double radius = 1);

	bool commit();
};

#endif
