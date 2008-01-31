#include <astro/system/memorymap.h>
#include <astro/exceptions.h>
#include <astro/util.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <astro/useall.h>
using namespace std;

void MemoryMap::open(const std::string &fn, int length, int offset, int mode, int mapstyle)
{
	close();

	this->length = length;
	this->filename = fn;

	int flags = 0;

	     if(mode & rw) { flags |= O_RDWR; }
	else if(mode & ro) { flags |= O_RDONLY; }
	else if(mode & wo) { flags |= O_WRONLY; }
	else THROW(EIOException, "Invalid mode parameter - mode needs to include ro, wo or rw");

	fd = ::open(filename.c_str(), flags);
	if(fd == -1)
	{
		fd = 0;
		THROW(EIOException, string("Error opening file [") + fn + "]");
	}

	map = mmap(0, length, mode, mapstyle, fd, offset);
	if(map == MAP_FAILED)
	{
		map = NULL;
		close();
		THROW(EIOException, string("Memory mapping of file [") + fn + "] falied. Parameters: length=" + str(length) + ", offset=" + str(offset));
	}
}

MemoryMap::MemoryMap()
: filename(""), fd(0), map(NULL), length(0)
{
}

MemoryMap::MemoryMap(const std::string &fn, int length_, int offset, int mode, int mapstyle)
: filename(fn), fd(0), map(NULL), length(length_)
{
	open(fn, length, offset, mode, mapstyle);
}

void MemoryMap::close()
{
	if(map != NULL)
	{
		if(munmap(map, length) == -1) { THROW(EIOException, string("Error unmapping file [") + filename + "]"); }
	}

	if(fd != 0)
	{
		if(::close(fd) == -1) { THROW(EIOException, string("Error closing file [") + filename + "]"); }
	}
}

MemoryMap::~MemoryMap()
{
	close();
}
