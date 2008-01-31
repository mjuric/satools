// orbfit.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "orbfit.h"

#include <astro/system/preferences.h>
#include <astro/util.h>
#include <astro/constants.h>
#include <astro/coordinates.h>

#include "stuff.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// The one and only application object

CWinApp theApp;

using namespace std;

/*
      DOUBLE PRECISION FUNCTION tjm1(iday,month,iyear,h)
      IMPLICIT NONE

      DOUBLE PRECISION h
      INTEGER iday,month,iyear,iy,im,ib,k1,k2,k3
*/

Preferences pref;
/*
int _tmain(int argc, TCHAR* argv[], TCHAR* envp[])
{
	int nRetCode = 0;

	// initialize MFC and print and error on failure
	if (!AfxWinInit(::GetModuleHandle(NULL), NULL, ::GetCommandLine(), 0))
	{
		// TODO: change error code to suit your needs
		cerr << _T("Fatal Error: MFC initialization failed") << endl;
		nRetCode = 1;

	}
	else
	{
		// TODO: code your application's behavior here.
		CString strHello;
		strHello.LoadString(IDS_HELLO);
		cout << (LPCTSTR)strHello << endl;

		cout << (int)pref["observatory.code"] << "\n";

		cout << Util::approxSunLongitude(51258.2029426)/ctn::d2r << "\n";

		double ra = 55*ctn::d2r, dec = 48.2*ctn::d2r;
		double l, b;
		Coordinates::equecl(ra, dec, l, b);
		cout << l/ctn::d2r << " " << b/ctn::d2r << "\n";

//		Catalog *cat = Catalog::open("astorb.dat", "ASTORB2");
//		cout << cat->recordCount();
//
//		vector<Object> o;
//		cat->read(o, 100000, 100100);
//
//		delete cat;
	}

	return nRetCode;
}

*/
