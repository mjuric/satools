#include <astro/constants.h>
#include <astro/time.h>
#include <astro/exceptions.h>
#include <astro/system/log.h>

#include <stdio.h>
#include <iostream>
#include <cstdlib>

#include "BowellCatalog.h"

using namespace peyton;
using namespace peyton::exceptions;
using namespace peyton::asteroids;

bool BowellCatalog::openCatalog(const char *filename, const char *mode) {
	fp = fopen(filename, "rb");
	if(fp == NULL) { DEBUG(basic, "Error opening catalog"); return false; }

	fseek(fp, 0, SEEK_END);
	int size = ftell(fp);
	if(size % BowellCatalog::recordByteLen != 0) {
		fclose(fp); fp = NULL;
		DEBUG(basic, "Size of catalog not a multiple of record length");
		return false;
	}

	recordCnt = size / BowellCatalog::recordByteLen;
	
	return true;
}

int BowellCatalog::write(Asteroid &obj, int at)
{
	THROW(ENotImplemented, "Not implemented yet !");
	return -1;
}

char *BowellCatalog::identify(char *name) {
	strcpy(name, "ASTORB2");
	return name;
}


int BowellCatalog::read(Asteroid &obj, const char *name)
{
	// on-demand creation of name-id mapping
	if(name2id.empty())
	{
		Asteroid obj;
		for(int i=0; i != recordCount(); i++)
		{
			read(obj, i);
			name2id[obj.name] = i;
		}
	}

	typeof(name2id.begin()) it = name2id.find(name);
	if(it == name2id.end()) { return -1; }

	return read(obj, it->second);
}

int BowellCatalog::read(Asteroid &obj, const int id_)
{
	int id = id_;
	if(id != -1) {
		fseek(fp, id*recordByteLen, SEEK_SET);
	} else {
		id = ftell(fp) / recordByteLen;
	}

	int y, m, d;
	char buffers[14][20];
	int blimits[14] = {6, 18, 5, 5, 5, 4, 2, 2, 10, 10, 10, 10, 10, 12};

	// load record
	char buf[recordByteLen];
	fread(buf, recordByteLen, 1, fp);

	// parse record
	int ret = sscanf(buf, "%6c%*c%18c%*17c%5c%*c%5c%*42c%5c%*6c%4c%2c%2c%*c%10c%*c%10c%*c%10c%10c%*c%10c%*c%12c",
		buffers[0], buffers[1],
		buffers[2], buffers[3],
		buffers[4],
		buffers[5], buffers[6],	buffers[7],
		buffers[8], buffers[9],	buffers[10], buffers[11], buffers[12], buffers[13]
		);

	if(ret != 14) {
		char c[20]; sprintf(c, "%d", id+1);
		DEBUG(basic, "Error reading asteroid at line #" + std::string(c));
		return -1;
	}

	// fixups
	int i;
	for(i = 0; i != 14; i++) buffers[i][blimits[i]] = 0;

	obj.numeration = atoi(buffers[0]);
	obj.type = 0; // ASTORB contains only asteroids

	for(i = Asteroid::maxNameLen-1; i != -1 && buffers[1][i] == ' '; i--);
	buffers[1][i+1] = 0;
	strcpy(obj.name, buffers[1]);
	obj.id = id == -1 ? ftell(fp)/recordByteLen - 1 : id;

	obj.h = atof(buffers[2]); obj.g = atof(buffers[3]);
	obj.arc = atoi(buffers[4]);
	y = atoi(buffers[5]);
	m = atoi(buffers[6]);
	d = atoi(buffers[7]);

	for(i = 0; i != 6; i++) obj.elements[i] = atof(buffers[13-i]);

	// conversions
	obj.t0 = peyton::time::calToMJD(y, m, d, 0);
	// TDT->TAI conversion
	//obj.t0 -= 32.184 / (24.*3600.);

	obj.elements[5] *= ctn::d2r;
	obj.elements[4] *= ctn::d2r;
	obj.elements[3] *= ctn::d2r;
	obj.elements[2] *= ctn::d2r;

	return 0;
}

int BowellCatalog::read(std::vector<Asteroid> &aobj, const int from_in, const int to_in)
{
	int from = from_in, to = to_in;
	if(to > recordCnt) {
		to = recordCnt;
	}

	aobj.erase(aobj.begin(), aobj.end());
	aobj.resize(to - from);
	int cnt = 0;

	// locate first record
	fseek(fp, from*recordByteLen, SEEK_SET);

	while(cnt != to - from) {
		Asteroid &obj = aobj[cnt];

		read(obj, -1);

		cnt++;
	}

	return cnt;
}

int BowellCatalog::recordCount() { return recordCnt; }

BowellCatalog::~BowellCatalog() {
	if(fp != NULL) fclose(fp);
}
