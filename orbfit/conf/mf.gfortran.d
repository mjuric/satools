# Makefile options for GNU gfortran compiler, debugging

# Fortran compiler
FC=gfortran
# Options for Fortran compiler:
FFLAGS=-g -C -I../include
# "ranlib" command: if it is not needed, use "RANLIB=touch"
RANLIB=ranlib
VPATH=../include
