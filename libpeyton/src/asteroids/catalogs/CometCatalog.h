#ifndef _astro_cometcatalog_h
#define _astro_cometcatalog_h

#include <astro/asteroids/catalog.h>
#include <map>
#include <string>

namespace peyton {
namespace asteroids {

class CometCatalog : public Catalog {
protected:
	virtual bool openCatalog(const char *filename, const char *mode);
	std::vector<Asteroid> comets;
	std::map<std::string, int> cometNameMap;

	friend class Catalog;
public:
	CometCatalog() : cometNameMap() { }

	virtual int read(Asteroid &obj, const char *name);
	virtual int read(std::vector<Asteroid> &obj, const int from, const int to);
	virtual int read(Asteroid &obj, const int id);
	virtual int recordCount();

	virtual int write(Asteroid &obj, int at);

	virtual char *identify(char *);

	virtual ~CometCatalog();
};

}
}

#endif
