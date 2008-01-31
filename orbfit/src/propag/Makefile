.f.o:
	$(FC) $(FFLAGS) -c $*.f -o $*.o 

include ../make.flags

all: ../lib/libprop.a 

rrdot.o: rrdot.f
	$(FC) -g -C -I../include -c rrdot.f -o rrdot.o

PROP= rmodel.o trange.o selste.o selmet.o force.o genrel.o eihrel.o masjpl.o \
     propa2.o propag.o ra15v.o propin.o bdnstev.o rkimp.o rkstep.o legnum.o \
      graha.o subobs.o catst.o clocms.o inipro.o selpert.o\
     nomoid.o lowmoid.o nodedi.o earth.o rasubs.o \
     yarkdi.o yarkse.o yarkinit.o j2sun.o times.o srtept.o

CLOAP= cloapp.o falsi.o subclan.o clotest.o

PUB=proele.o preobs.o preobn.o ephemc.o

DIFCOR=alfdel.o rrdot.o srtoss.o difcor.o appmag.o start.o graha1.o difvin.o\
       weakdi.o julian.o minsol.o reject.o rejini.o difini.o sincor.o \
       magest.o alfdel2.o meanti.o rescov.o intstep.o bizarre.o \
       fdifco.o fmulti.o mmulti.o nights.o

../lib/libprop.a: $(PROP) $(DIFCOR) $(PUB) $(CLOAP) 
	ar r ../lib/libprop.a $(PROP) $(DIFCOR) $(PUB) $(CLOAP)
	$(RANLIB) ../lib/libprop.a

depend:
	./mkdep *.f *.h ../include/*.h > make.dep

clean:
	-(rm -f *.x *.o ../lib/libprop*.a *% *~ core); 

distclean: clean
	../lib/deinst_src

# Windows targets
win: ..\lib\prop.lib 

..\lib\prop.lib:$(PROP) $(DIFCOR) $(PUB) $(CLOAP)
	lib /out:$@ $**

winclean:
	@del *.o
	@del *.pdb
	@del ..\lib\prop.lib

# Dependencies
include make.dep
