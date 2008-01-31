#ifndef __astro_io_printf
#define __astro_io_printf

#include <astro/exceptions.h>

// cout << io::format("%2.3f %d %o") << a << b << c;
//
// format f("%2.3f %d %o");

#include <iostream>
#include <cstdio>
#include <memory>

namespace peyton {

namespace exceptions {
	SIMPLE_EXCEPTION(EIOFormat);
}

namespace io {

struct trad_format
{
	char flag;
	int width;
	int prec;
	char length[2];
	char conversion;
};

class format : public std::string
{
public:
	format(const char *c) : std::string(c) {}
	format(const std::string &s) : std::string(s) {}
};

class Formatter
{
public:
	std::string formatt;
	std::ostream *out;
	bool sstrm;
public:
	int at, to;
	trad_format tf;
public:
	explicit Formatter(const std::string &fmt);
	
	Formatter(std::ostream &out_, const std::string &fmt)
	: formatt(fmt), out(&out_), sstrm(false), at(0), to(0)
	{
		pop();
	}

	~Formatter();

	operator std::string() const;

	void pop();
	std::string front() { return formatt.substr(at, to-at); }
	bool empty() { return at == to; }
};

typedef std::auto_ptr<Formatter> PFormatter;

//
// This operator creates the transient parser object
//
template<typename T>
inline PFormatter operator <<(const format &fmt, const T &x)
{
	PFormatter f(new Formatter(fmt));
	return f, x;
}

inline PFormatter operator <<(std::ostream &out, const format &fmt)
{
	return PFormatter(new Formatter(out, fmt));
}

inline std::ostream &operator <<(std::ostream &out, const PFormatter f)
{
	return out << (std::string)(*f);
}

//
// Default output template for formatted output
// Calls format_type(ostream out, string fmt, const T &x) which is expected to be
// specialized for a type being streamed.
// Note that PFormatter is the type of f, not PFormatter &
//
template<typename T>
PFormatter operator <<(PFormatter f, const T &x)
{
	if(f->empty()) { (*f->out) << x; return f; }
	format_type((*f->out), f->front(), x);
	f->pop();
	return f;
}

/*
template<typename T>
Formatter &operator <<(Formatter &f, const T &x)
{
	if(f.empty()) { (*f.out) << x; return f; }
	format_type((*f.out), f.front(), x);
	f.pop();
	return f;
}
*/

//
// Default, unspecialized format_type template
// TODO: Should this throw an exception, or even not be here at all?
//
template<typename T>
void format_type(std::ostream &f, const std::string &fmt, const T &v)
{
	f << "###UNKNOWN_TYPE###";
}

//
// specializations for built in types
// - we directly specialize operator << in order to use information already obtained
// from the parser in Formatter::tf structure
//
PFormatter operator <<(PFormatter f, const int &x);
PFormatter operator <<(PFormatter f, const unsigned &x);
PFormatter operator <<(PFormatter f, const double &x);
PFormatter operator <<(PFormatter f, const char &x);
PFormatter operator <<(PFormatter f, const bool &x);
PFormatter operator <<(PFormatter f, const char *x);

inline PFormatter operator <<(PFormatter f, const std::string &x) { if(f->empty()) { (*f->out) << x; return f; }; return (f << x.c_str()); }
inline PFormatter operator <<(PFormatter f, const float &x) { if(f->empty()) { (*f->out) << x; return f; }; return (f << (double)x); }
inline PFormatter operator <<(PFormatter f, const char &x) { if(f->empty()) { (*f->out) << x; return f; }; return (f << (int)x); }

} // namespace io
} // namespace peyton

#define __peyton peyton
#define __peyton_io peyton::io
#define __peyton_exceptions peyton::exceptions

#endif
