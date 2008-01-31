#ifndef _astro_log_h
#define _astro_log_h

#include <sstream>

namespace peyton {
namespace system {

/**
	\brief Class for event logging and debugging.
	
	\warning Do not use this class directly. Instead, use DEBUG(), ERRCHECK() and ASSERT() macros instead.
*/
class Log {
protected:
	static char buf[1000];
	static int debugLevel;
public:
	static void write(const char *text, ...);
	static void debug(int level, const char *text, ...);

	static std::ostream &stream_begin();
	static void stream_end();

	enum { terminate=-2, error, exception, basic, verbose, all };

public:
	class linestream : public std::ostringstream {
	public:
		linestream(int level);
		~linestream();
		std::ostringstream &stream();
	};
	static int level(int newlevel = -1);
};

}
}

#define DEBUG(lev, args...) { \
	if(peyton::system::Log::level() >= peyton::system::Log::lev) { peyton::system::Log::linestream((int)peyton::system::Log::lev).stream() << args; } \
}

#define ERRCHECK(condition) if(condition)
#define ASSERT(cond) if(!(cond)) { DEBUG(terminate, "Assertion [" #cond "] failed at " << __PRETTY_FUNCTION__ << ", " << __FILE__ << ":" << __LINE__); abort(); }

#define __peyton_system peyton::system

#endif
