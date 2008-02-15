# Note: Change the line below if your fortran compiler is not gfortran
F77=gfortran
export F77

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
