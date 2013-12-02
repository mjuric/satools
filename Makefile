include compilers.mak

BASEDIR=$(shell pwd)

satools:
	# Build orbfitlib
	(cd orbfit && ./config -O $(F77) && make)
	# Build libpeyton
	(cd libpeyton/src && (test -f make.dep || make depend) && make && make parallel)
	# Build libpeyton perl modules
	(cd libpeyton/perl/LibPeyton/Coordinates && perl Makefile.PL PREFIX=$(BASEDIR)/perl_modules && make install)
	(cd libpeyton/perl/LibPeyton/Util && perl Makefile.PL PREFIX=$(BASEDIR)/perl_modules && make install)
	# Build satools
	(cd src && (test -f make.dep || make depend) && make)

clean:
	(cd orbfit && make clean)
	(cd libpeyton/src && make clean)
	(cd src && make clean)
	(rm -rf perl_modules)
