#include <astro/system/fs.h>
#include <astro/exceptions.h>
#include <astro/util.h>

#include <iostream>
#include <cstdlib>

using namespace peyton::system;
using namespace peyton::exceptions;

EnvVar::operator std::string() const
{
	const char *c = getenv(nm.c_str());
	if(c == NULL)
	{
		THROW(EEnvVarNotSet, "Environment variable '" + nm + "' is not set");
	}
	return c;
}

void EnvVar::set(const std::string &v, bool overwrite)
{
	if(setenv(nm.c_str(), v.c_str(), overwrite) != 0)
	{
		THROW(EEnvVar, "Failed to set [" + nm + "] environment variable");
	}
}

void EnvVar::unset() throw()
{
	unsetenv(nm.c_str());
}

EnvVar::operator bool() const throw()
{
	return (getenv(nm.c_str()) == NULL);
}

namespace peyton {
namespace system {
	OSTREAM(const EnvVar &v)
	{
		return out << "$" << v.name() << " = '" << (std::string &)v << "'";
	}

}
}
