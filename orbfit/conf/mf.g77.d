# Makefile options for GNU g77 compiler, debugging

# Fortran compiler
FC=$(G77)
# Options for Fortran compiler:
FFLAGS=-g -C -I../include
# "ranlib" command: if it is not needed, use "RANLIB=touch"
RANLIB=ranlib
VPATH=../include
