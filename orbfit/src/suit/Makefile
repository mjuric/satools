.f.o:
	$(FC) $(FFLAGS) -c $*.f -o $*.o 

include ../make.flags

all: ../lib/libsuit.a

interp.o: interp.f
	$(FC) -O -I../include -c interp.f -o interp.o

#iers.o: iers.f
#	$(FC) -O -I../include -c iers.f -o iers.o

# LIBRARY OBJECT FILES

# Orbital elements
ELEM_OBJ=coocha.o prop2b.o carequ.o equcar.o kepequ.o equkep.o eqpequ.o \
         equeqp.o ekensd.o rdelem.o wro1lh.o wro1lr.o outele.o outel1.o \
         wromlh.o wromlr.o rdast1.o rdast2.o oefdet.o rdmpca.o mpcpds.o \
         mpcdat.o rdoef.o fixcnm.o rdmpca2.o

# Observations & Statistics
OBS_OBJ=obsrms.o rdorms.o wrorms.o wrores.o fitwgt.o iaucod.o iaucod2.o \
        wrirwo.o wrirms.o sortob.o rearwo.o addobs.o inobs.o wrirwg.o \
        astrow.o magrms.o rrmscl.o accstr.o crmscl.o pvobs2.o errel.o \
        astrwb.o crmscn.o rdcorm.o obscor.o fcorob.o fcsfun.o

# Mathematical routines
MATH_OBJ=princ.o prscal.o prvec.o vsize.o prodmv.o mulmat.o lincom.o \
         plegm.o plegd1.o evdis.o tchol.o mult3.o rotmt.o trsp.o \
         matvet.o prodmm.o vetmat.o snorm.o snormd.o vdiff.o \
         transp.o matin.o ident.o vcopy.o mcopy.o primea.o pridif.o intlo.o \
         inthi.o bilin.o lincog.o vsumg.o prscag.o inv22.o mulmav.o tchinv.o \
         covprs.o norprs.o polar.o assmat.o chebym.o chebyd.o inver2.o \
         lgnint.o pcwlgi.o pdmat.o pd1mat.o pd2mat.o prodvs.o rotmt1.o \
         rotmt2.o smoocn.o summat.o sumv.o tchol2.o trsp3.o

# JPL Digital Ephemeris and binary ephemeris
EPH_OBJ=rdbep.o jpllis.o fszer2.o dpleph.o state.o interp.o const.o split.o 

# File operations
FILE_OBJ=filopn.o filclo.o filass.o filopl.o libini.o filopf.o dlifex.o \
         filnam.o

# Header namelists
HNAM_OBJ=rdfnam.o rdfcha.o rdfint.o rdflog.o rdfrea.o rdftim.o splkvc.o \
         rdfref.o getrsc.o wrfcha.o wrfini.o wrfint.o wrfinv.o wrflog.o \
         wrfrea.o wrfrev.o wrftim.o

# Main option namelist
OPT_OBJ=getkv.o namini.o rdnam.o rdnam1.o rdnam2.o rdnam3.o rdklst.o \
        chkkey.o rdncha.o rdnint.o rdnlog.o rdnrea.o rdntim.o rdnref.o \
        chkfln.o rdmint.o rdmrea.o rdmcha.o rdvint.o rdvrea.o rdvcha.o \
        rdkls1.o sv2int.o

# Character strings
CHA_OBJ=lench.o rmsp.o norstr.o strcnt.o upcase.o stspli.o isnum.o islett.o \
        nitchs.o locase.o spflds.o titast.o isbnum.o filstr.o

# Reference systems
REF_OBJ=rotpn.o obleq.o rnut80.o prec.o nutn80.o equequ.o gmst.o aber1.o \
        aber2.o obscoo.o chkref.o ch2ref.o posobs.o pvobs.o

# Time and timescales
TIME_OBJ=ch2tim.o intmon.o tjm1.o timnf.o itaiut.o chktsc.o cnvtim.o \
         deltt.o mjddat.o chmon.o bessep.o chmo2i.o

# Handling of IERS time series
IERS_OBJ=delthp.o diurot.o dut1r.o dut1s.o equeqd.o gmsnom.o gmstd.o \
         ierini.o iersts.o isbadr.o nutarg.o nutnd.o nutwhr.o obleqd.o \
         precd.o rdbula.o rnutd.o rotpv.o rotsys.o xypol.o

# I/O routines
IO_OBJ=mpcin.o mpctr.o rdanga.o oporbf.o clorbf.o rdorb.o sessag.o mpcrad.o \
	jplin.o jplrad.o astrad.o angvcf.o

# Various utilities
UTIL_OBJ=chkpdf.o eildri.o eildrf.o eildrl.o roff.o tee.o truncat.o epsini.o menu.o

# EISPACK routines (to be substituted?)
EIS_OBJ=rs.o tred1.o tqlrat.o tred2.o tql2.o

SUIT_OBJ=$(FILE_OBJ) $(HNAM_OBJ) $(CHA_OBJ) $(UTIL_OBJ) $(TIME_OBJ) \
         $(IO_OBJ) $(ELEM_OBJ) $(MATH_OBJ) $(EPH_OBJ) $(REF_OBJ) \
         $(EIS_OBJ) $(OPT_OBJ) $(OBS_OBJ) $(IERS_OBJ)

../lib/libsuit.a: $(SUIT_OBJ)
	ar r ../lib/libsuit.a $(SUIT_OBJ)
	$(RANLIB) ../lib/libsuit.a

depend:
	./mkdep *.f *.h ../include/*.h > make.dep

clean:
	-(rm -f *.x *.o ../lib/libsuit.a *% *~ core); 

distclean: clean
	../lib/deinst_src

# Windows targets
win: ..\lib\suit.lib

..\lib\suit.lib:$(SUIT_OBJ)
	lib /out:$@ $**

winclean:
	@del *.o
	@del *.pdb
	@del ..\lib\suit.lib

# Dependencies
include make.dep
