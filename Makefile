include compilers.mak

satools:
	# Build orbfitlib
	(cd orbfit && ./config -O $(F77) && make)
	# Build libpeyton
	(cd libpeyton/src && (test -f make.dep || make depend) && make && make parallel)
	# Build satools
	(cd src && (test -f make.dep || make depend) && make)

clean:
	(cd orbfit && make clean)
	(cd libpeyton/src && make clean)
	(cd src && make clean)
