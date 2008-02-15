.f.o:
	$(F77) $(FFLAGS) -I../orbfit/src/include -I./include -c $*.f -o $*.o

.cpp.o:
	$(CXX) $(CCFLAGS) -I../include -c $*.cpp -o $*.o

all: id2.x propagate.x createcache.x createcache2.x showcache.x phi.x extractrun.x \
	nearest.x out2sdss.x proc.x whatis.x \
	makempec.x filtercatalog.x properidx.x updatematches.x \
	calcsky.x calcsky2.x sky2binary.x skyresample.x mockCatalog.x oiddbconvert.x \
	calcpos.x

FSRC=   preobs2.f
OBJS=	preobs2.o orbfitlib.o observationcache.o observationcalculator.o identrecord.o sarecord.o safile.o
CXXSRC=	orbfitlib.cpp observationcache.cpp observationcalculator.cpp identrecord.cpp sarecord.cpp safile.cpp \
	id2.cpp propagate.cpp createcache.cpp createcache2.cpp showcache.cpp phi.cpp extractrun.cpp \
	nearest.cpp out2sdss.cpp proc.cpp cleanup.cpp whatis.cpp mergempec.cpp \
	makempec.cpp filtercatalog.cpp properidx.cpp updatematches.cpp oiddbconvert.cpp calcpos.cpp
SRC=    $(FSRC) $(CXXSRC)
LIBS=	../libpeyton/lib/libastro.a
ORBFIT=	../orbfit/src/lib/libprop.a ../orbfit/src/lib/libsuit.a

../libpeyton/lib/libastro.a:
	(cd ../libpeyton/src; make all parallel)

p:
	rm -f propagate.o

propagate.x: $(OBJS) $(LIBS) propagate.o
	$(CXX) $(CCFLAGS) propagate.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran ../libpeyton/lib/libparallel.a -o propagate.x
	cp propagate.x ../bin

mockCatalog.x: $(LIBS) mockCatalog.o
	$(CXX) $(CCFLAGS) mockCatalog.o $(LIBS) -o mockCatalog.x
	cp mockCatalog.x ../bin

id2.x: $(OBJS) $(LIBS) id2.o cleanup.o
	$(CXX) $(CCFLAGS) id2.o cleanup.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o id2.x
	cp id2.x ../bin

createcache.x: $(OBJS) $(LIBS) createcache.o
	$(CXX) $(CCFLAGS) createcache.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o createcache.x
	cp createcache.x ../bin

createcache2.x: $(OBJS) $(LIBS) createcache2.o
	$(CXX) $(CCFLAGS) createcache2.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran ../libpeyton/lib/libparallel.a -o createcache2.x
	cp createcache2.x ../bin

calcsky.x: $(OBJS) $(LIBS) calcsky.o
	$(CXX) $(CCFLAGS) calcsky.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o calcsky.x
	cp calcsky.x ../bin

calcpos.x: $(OBJS) $(LIBS) calcpos.o
	$(CXX) $(CCFLAGS) calcpos.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o calcpos.x
	cp calcpos.x ../bin

calcsky2.x: $(OBJS) $(LIBS) calcsky2.o
	$(CXX) $(CCFLAGS) calcsky2.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o calcsky2.x
	cp calcsky2.x ../bin

sky2binary.x: $(OBJS) $(LIBS) sky2binary.o
	$(CXX) $(CCFLAGS) sky2binary.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o sky2binary.x
	cp sky2binary.x ../bin

skyresample.x: $(OBJS) $(LIBS) skyresample.o
	$(CXX) $(CCFLAGS) skyresample.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o skyresample.x
	cp skyresample.x ../bin

showcache.x: $(OBJS) $(LIBS) showcache.o
	$(CXX) $(CCFLAGS) showcache.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o showcache.x
	cp showcache.x ../bin

extractrun.x: $(OBJS) $(LIBS) extractrun.o
	$(CXX) $(CCFLAGS) extractrun.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o extractrun.x
	cp extractrun.x ../bin

phi.x: $(OBJS) $(LIBS) phi.o
	$(CXX) $(CCFLAGS) phi.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o phi.x
	cp phi.x ../bin

nearest.x: $(OBJS) $(LIBS) nearest.o cleanup.o
	$(CXX) $(CCFLAGS) nearest.o cleanup.o $(LIBS) -o nearest.x
	cp nearest.x ../bin

out2sdss.x: $(OBJS) $(LIBS) out2sdss.o
	$(CXX) $(CCFLAGS) out2sdss.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o out2sdss.x
	cp out2sdss.x ../bin

makempec.x: $(OBJS) $(LIBS) makempec.o
	$(CXX) $(CCFLAGS) makempec.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o makempec.x
	cp makempec.x ../bin
	
proc.x: $(OBJS) $(LIBS) proc.o
	$(CXX) $(CCFLAGS) proc.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o proc.x
	cp proc.x ../bin

whatis.x: $(OBJS) $(LIBS) whatis.o
	$(CXX) $(CCFLAGS) whatis.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o whatis.x
	cp whatis.x ../bin

setpairs.x: $(OBJS) $(LIBS) setpairs.o
	$(CXX) $(CCFLAGS) setpairs.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o setpairs.x
	cp setpairs.x ../bin

updatematches.x: $(OBJS) $(LIBS) updatematches.o
	$(CXX) $(CCFLAGS) updatematches.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o updatematches.x
	cp updatematches.x ../bin

mergempec.x: $(OBJS) $(LIBS) mergempec.o
	$(CXX) $(CCFLAGS) mergempec.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o mergempec.x

analysis.x: $(OBJS) $(LIBS) analysis.o
	$(CXX) $(CCFLAGS) analysis.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o analysis.x
	cp analysis.x ../bin

astorb_analysis.x: $(OBJS) $(LIBS) astorb_analysis.o
	$(CXX) $(CCFLAGS) astorb_analysis.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o astorb_analysis.x
	cp astorb_analysis.x ../bin

oiddbconvert.x: $(OBJS) $(LIBS) oiddbconvert.o
	$(CXX) $(CCFLAGS) oiddbconvert.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o oiddbconvert.x
	cp oiddbconvert.x ../bin

renamesent.x: $(OBJS) $(LIBS) renamesent.o
	$(CXX) $(CCFLAGS) renamesent.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o renamesent.x
	cp renamesent.x ../bin

filtercatalog.x: $(OBJS) $(LIBS) filtercatalog.o
	$(CXX) $(CCFLAGS) filtercatalog.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o filtercatalog.x
	cp filtercatalog.x ../bin

properidx.x: $(OBJS) $(LIBS) properidx.o
	$(CXX) $(CCFLAGS) properidx.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o properidx.x
	cp properidx.x ../bin

photutils: johnson johnson_gr_r

johnson.x: $(OBJS) $(LIBS) johnson.o
	$(CXX) $(CCFLAGS) johnson.o $(LIBS) -lgfortran -o johnson.x
	cp johnson.x ../bin

johnson_gr_r.x: $(OBJS) $(LIBS) johnson_gr_r.o
	$(CXX) $(CCFLAGS) johnson_gr_r.o $(OBJS) $(LIBS) $(ORBFIT) -lgfortran -o johnson_gr_r.x
	cp johnson_gr_r.x ../bin

depend:
	$(CXX) -M -I../include $(CXXSRC) > make.dep
	## $(F77) -M -I../orbfit/src/include -I../include $(FSRC) >> make.dep

clean:
	-(rm -f *.o ../bin/*.x *.x make.dep)

ifneq ($(MAKECMDGOALS),depend)
ifneq ($(MAKECMDGOALS),clean)
include make.dep
endif
endif
