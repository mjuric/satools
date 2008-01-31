#ifndef __astro_system_memorymap_h
#define __astro_system_memorymap_h

#include <sys/mman.h>
#include <string>

namespace peyton {
namespace system {

class MemoryMap
{
protected:
	std::string filename;
	int fd;

	int length;
	void *map;

public:	
	enum {
		ro = PROT_READ,
		wo = PROT_WRITE,
		rw = PROT_READ | PROT_WRITE,
		none = PROT_NONE,
		exec = PROT_EXEC
	};
	
	enum {
		shared = MAP_SHARED,
		priv = MAP_PRIVATE
	};
public:
	MemoryMap();
	MemoryMap(const std::string &filename, int length, int offset = 0, int mode = ro, int map = shared);

	void open(const std::string &filename, int length, int offset = 0, int mode = ro, int map = shared);
	void close();

	~MemoryMap();

	operator void *() { return map; }
};

template<typename T>
class MemoryMapVector : public MemoryMap
{
public:
	typedef T* iterator;
public:
	MemoryMapVector() : MemoryMap() {}
	void open(const std::string &filename, int size, int offset = 0, int mode = ro, int mapstyle = shared)
	{
		MemoryMap::open(filename, sizeof(T)*size, offset, mode, mapstyle);
	}
	
	const T &operator[](int i) const { return ((const T *)map)[i]; }
	T &operator[](int i) { return ((T *)map)[i]; }

	iterator begin() { return (T *)map; }
	iterator end() { return ((T *)map) + length; }
	size_t size() { return length; }
};

} // namespace system
} // namespace peyton

#define __peyton_system peyton::system

#endif

