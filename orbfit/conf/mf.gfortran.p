# Makefile options for GNU gfortran compiler, optimized and GNU profiler

# Fortran compiler
FC=gfortran
# Options for Fortran compiler:
FFLAGS=-pg -O3 -malign-double -funroll-loops  -finline-functions -ffast-math -I../include
# "ranlib" command: if it is not needed, use "RANLIB=touch"
RANLIB=ranlib
VPATH=../include
