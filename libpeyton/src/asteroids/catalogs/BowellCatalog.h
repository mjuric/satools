#ifndef _astro_bowellcatalog_h
#define _astro_bowellcatalog_h

#include <astro/asteroids/catalog.h>
#include <map>
#include <string>

namespace peyton {
namespace asteroids {

class BowellCatalog : public Catalog {
protected:
	enum {recordByteLen = 267 + 1 };
	int recordCnt;
	virtual bool openCatalog(const char *filename, const char *mode);
	FILE *fp;

	std::map<std::string, int> name2id;

	friend class Catalog;
public:
	BowellCatalog() { fp = NULL; recordCnt = -1; }

	virtual int read(Asteroid &obj, const char *name);
	virtual int read(std::vector<Asteroid> &obj, const int from, const int to);
	virtual int read(Asteroid &obj, const int id);
	virtual int recordCount();

	virtual int write(Asteroid &obj, int at);

	virtual char *identify(char *);

	virtual ~BowellCatalog();
};

}
}

#endif
