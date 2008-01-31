#include <stack>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <fstream>
#include <iostream>

using namespace std;

struct sss {
	int rep;
	bool single;
	const char *fmtstart;
};

int nextNum(const char *&fmt)
{
	int len = 0;
	while(*fmt && isdigit(*fmt)) { len *= 10; len += *fmt - '0'; fmt++; }
	if(*fmt == '.') {
		fmt++;
		while(isdigit(*fmt)) fmt++;
	}
	if(len == 0) len = 1;
	return len;
}

int sfscanf(const char *text, const char *format, ...)
{
	va_list marker;
	va_start(marker, format);

	stack<sss> stk;
	sss s;

	const char *fmt = format;
	const char *t = text;

	char *dest;
	char tmp[100];
	int *intdest;
	double *fltdest;
	int len;

	while(*fmt) {
		char c = toupper(*fmt);
		if(isdigit(c) || c == '(') {
			s.rep = nextNum(fmt);
			while(isspace(*fmt) || *fmt == ',') fmt++;
			if(s.rep == 1 && *fmt != '(') { continue; }
			if(*fmt == '(') {
				s.single = false;
				fmt++;
			} else {
				s.single = true;
			}
			s.fmtstart = fmt;
			stk.push(s);
			continue;
		}

		if(c != ')') fmt++;
		len = nextNum(fmt);

		switch(c) {
		case 'A':
			dest = va_arg(marker, char *);
			strncpy(dest, t, len);
			dest[len] = 0;
			t+=len;
			break;
		case 'I':
			intdest = va_arg(marker, int *);
			strncpy(tmp, t, len);
			tmp[len] = 0;
			*intdest = atoi(tmp);
			t+=len;
			break;
		case 'F':
			fltdest = va_arg(marker, double *);
			strncpy(tmp, t, len);
			tmp[len] = 0;
			*fltdest = atof(tmp);
			t+=len;
			break;
		case 'X':
			t+=len;
			break;
		}

		while(isspace(*fmt) || *fmt == ',') fmt++;

		if(!stk.empty()) {
			sss &state = stk.top();
			if(*fmt == ')' || state.single) {
				state.rep--;
				if(state.rep) {
					fmt = state.fmtstart;
				} else {
					if(*fmt==')' && !state.single) fmt++;
					stk.pop();
				}
			}

			while(isspace(*fmt) || *fmt == ',') fmt++;
		}
	}

	va_end(marker);

	return 0;
}

void main()
{
	const char *fmt = "A5,1X,A18,1X,A15,1X,A5,1X,F5.2,1X,A4,1X,A5,1X,A4,1X,6I4,1X,2I5,1X,I4,2I2.2,3(1X,F10.6),F10.6,1X,F10.8, 1X,F12.8,1X,I4,2I2.2,1X,F7.2,1X,F8.2,1X,I4,2I2,3(1X,F7.2,1X,I4,2I2)";
//	const char *txt = "    1 Ceres              E. Bowell        3.34  0.12 0.72 913.0 G?      0   0   0   0   0   0 56959 4750 19960427  80.477333  71.802404  80.659857 10.600303 0.07604100   2.76788714 19960414 2.3E-02  1.4E-04 19960416 2.7E-02 19960530 3.1E-02 20040111 3.1E-02 20040111";
	const char *txt = "    1 Ceres              E. Bowell        3.34  0.12 0.72 848.4 G?      0   0   0   0   0   0 62125 5596 20010710 125.049915  73.973398  80.489708 10.583794 0.07879873   2.76716809 20010411 2.3E-02 -1.5E-05 20010710 1.9E-02 20021003 2.6E-02 20090225 2.6E-02 20090225";

	char num[6];
	char desig[19];
	char computer[16];
	char H[6];
	double G;
	char BV[5];
	char irasDiam[6];
	char irasTaxonomy[5];
	int codes[6];
	int arc, obsnum;
	int y, m, d;
	double elem[6];
	int y2, m2, d2; // date of orbit computation
	double ceu, ceu_change;
	int y3, m3, d3; // date of CEU
	double nextPEU; int y4, m4, d4; // nextPEU
	double greatestPEU; int y5, m5, d5; // greatestPEU
	double greatestPEUo; int y6, m6, d6; // greatestPEU

	ifstream f("astorb.dat");
	char buf[500];

	f.getline(buf, 500);
	while(!f.eof()) {
		sfscanf(buf, fmt,
			num, desig, computer, H, &G, BV, irasDiam, irasTaxonomy,
			&codes[0], &codes[1], &codes[2], &codes[3], &codes[4], &codes[5],
			&arc, &obsnum,
			&y, &m, &d,
			&elem[0], &elem[1], &elem[2], &elem[3], &elem[4], &elem[5],
			&y2, &m2, &d2,
			&ceu, &ceu_change,
			&y3, &m3, &d3,
			&nextPEU, &y4, &m4, &d4,
			&greatestPEU, &y5, &m5, &d5,
			&greatestPEUo, &y6, &m6, &d6
			);

		cout <<
			num << "|" << desig << "|" << computer << "|" << H << "|" << G << "|" << BV << "|" << irasDiam << "|" << irasTaxonomy << "|" << 
			codes[0] << "|" << codes[1] << "|" << codes[2] << "|" << codes[3] << "|" << codes[4] << "|" << codes[5] << "|" << 
			arc << "|" << obsnum << "|" << 
			y << "-" << m << "-" << d << "|" << 
			elem[0] << "|" << elem[1] << "|" << elem[2] << "|" << elem[3] << "|" << elem[4] << "|" << elem[5] << "|" << 
			y2 << "-" << m2 << "-" << d2 << "|" << 
			ceu << "|" << ceu_change << "|" << 
			y3 << "-" << m3 << "-" << d3 << "|" << 
			nextPEU << "|" << y4 << "-" << m4 << "-" << d4 << "|" << 
			greatestPEU << "|" << y5 << "-" << m5 << "-" << d5 << "|" << 
			greatestPEUo << "|" << y6 << "-" << m6 << "-" << d6 << "\n";

		f.getline(buf, 500);
	}
}
