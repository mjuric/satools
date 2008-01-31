.f.o:
	$(FC) $(FFLAGS) -c $*.f -o $*.o 

include ../make.flags

all: fitobs.x catpro.x

#packages

FIT=fitsubs.o fgauss.o stacop.o fobpre.o

FCLAN=fclan.o virimp.o preob4.o subobs4.o fclanout.o clanew.o 

FSTPRO=fstpro.o fsteph.o outobc.o outcov.o outmul.o

FIDENT=fident.o identsub.o

FINOUT=finopt.o finobs.o finele.o fitio.o plocbd.o wriedb.o 

FMULTI=fmuobs.o fmupro.o fmuplo.o 

FITOBS= fitobs.o $(FIT) $(FSTPRO) $(FIDENT) $(FINOUT) $(FMULTI) $(FCLAN)

FITOBSDEPS= $(FITOBS) ../lib/libprop.a ../lib/libgauss.a ../lib/libsuit.a

# Executables

fitobs.x: $(FITOBSDEPS)
	$(FC) $(FFLAGS) -o fitobs.x  $(FITOBSDEPS)

catpro.x: catpro.o cinopt.o  \
	../lib/libprop.a ../lib/libgauss.a ../lib/libsuit.a 
	$(FC) $(FFLAGS) catpro.o cinopt.o \
	../lib/libprop.a ../lib/libgauss.a ../lib/libsuit.a -o catpro.x

depend:
	./mkdep *.f *.h ../include/*.h > make.dep

clean:
	-(rm -f *.x *.o *% *~ core)

distclean: clean
	../lib/deinst_src

# Windows targets
win: ..\..\bin\fitobs.exe

..\..\bin\fitobs.exe:$(FITOBS) ..\lib\prop.lib ..\lib\suit.lib ..\lib\gauss.lib
	$(FC) $(FFLAGS) /link /out:$@ $**

windist:
	@del *.o
	@del *.pdb
	@del ..\..\bin\*.pdb
	@del ..\..\bin\*.ilk

winclean:windist
	@del ..\..\bin\fitobs.*

# Dependencies
include make.dep
