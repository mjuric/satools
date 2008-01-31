#ifndef __analysis_h
#define __analysis_h

#include <vector>
#include <iomanip>
#include <fstream>

typedef pair<double, int> Bin;
typedef vector<Bin> Bins;
inline ostream& operator << (ostream& os, const Bin& s) { os << setprecision(10) << s.first << " " << s.second; return os; }

#define CONSTRAIN_TO(lst, low, elem, hi) \
	for(ii i = lst.begin(); i != lst.end(); ) { ii j = i++; if(!(low <= (*j).elem && (*j).elem <= hi)) lst.erase(j); } \
	cout << "# " << #lst << ": " << lst.size() << " remained after " #low " <= " #elem " <= " #hi " cut\n";
#define WRITE_HIST(lst, file) { ofstream f(file); copy(lst.begin(), lst.end(), ostream_iterator<Bin>(f, "\n")); }

#define HISTOGRAM(all, elem, bins, low, hi, nbins) \
	{ \
	bins.clear(); \
	double delta = (double(hi) - double(low)) / double(nbins); \
	for(int i = 0; i != nbins; i++) { bins.push_back(Bin(low + delta * (i + 0.5), 0)); } \
	for(ii i = all.begin(); i != all.end(); i++) { \
		double val = (*i).elem; \
		int bin = int((val - low) / delta); \
		if(0 < bin && bin < nbins) { \
			bins[bin].second++; \
		} \
	} \
	}

#define CUMULATIVE_HISTOGRAM(all, elem, bins, low, hi, nbins) \
	HISTOGRAM(all, elem, bins, low, hi, nbins) \
	{ int sum = 0; for(int i = 0; i != nbins; i++) { int v = bins[i].second; bins[i].second += sum; sum += v; } }

#endif

