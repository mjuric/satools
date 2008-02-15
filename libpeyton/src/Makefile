#
# The following variables are site dependent:
#
# NOTE: define these environment variables in your .cshrc to point
#       to relevant FITS library directories
#
# FITSFLAGS= -I/usr/local/include/CCfits
# FITSFLAGS= -I$(LFS)/usr/local/ftools/include -I$(LFS)/usr/local/include/CCfits
#


#
# Don't touch below this line
#

.cpp.o:
	$(CXX) -O0 -g -fPIC -D_GNU_SOURCE -I../include $(FITSFLAGS) -c $*.cpp -o $*.o

all: ../lib/libastro.a
parallel: ../lib/libparallel.a

PARALLELOBJS=	net/Socket.o \
		net/RemoteInstance.o
PARALLELSRC=	net/Socket.cpp \
		net/RemoteInstance.cpp

../lib/libparallel.a: $(PARALLELOBJS)
	ar r ../lib/libparallel.a $(PARALLELOBJS)
	ranlib ../lib/libparallel.a

OBJS=	asteroids/catalogs/MPCCatalog.o asteroids/catalogs/BowellCatalog.o asteroids/catalogs/NativeCatalog.o asteroids/catalogs/MemoryCatalog.o asteroids/Catalog.o asteroids/Asteroids.o \
		asteroids/catalogs/CometCatalog.o \
		system/Dir.o system/MemoryMap.o system/Error.o system/Log.o system/Mixed.o system/Shell.o system/Preferences.o system/System.o system/Config.o system/FS.o system/Options.o \
		Exceptions.o \
		Coordinates.o SkyPoint.o Time.o Util.o sdss/Photometry.o \
		image/Map.o io/Compress.o io/Format.o \
		sdss/RunGeometry.o

SRC=	asteroids/catalogs/MPCCatalog.cpp asteroids/catalogs/BowellCatalog.cpp asteroids/catalogs/NativeCatalog.cpp asteroids/catalogs/MemoryCatalog.cpp asteroids/Catalog.cpp asteroids/Asteroids.cpp \
		system/Dir.cpp system/MemoryMap.cpp system/Error.cpp system/Log.cpp system/Mixed.cpp system/Shell.cpp system/Preferences.cpp system/System.cpp system/Config.cpp system/FS.cpp system/Options.cpp \
		Exceptions.cpp \
		Coordinates.cpp SkyPoint.cpp Time.cpp Util.cpp sdss/Photometry.cpp \
		image/Map.cpp io/Compress.cpp io/Format.cpp \
		sdss/RunGeometry.cpp

../lib/libastro.a: $(OBJS)
	ar r ../lib/libastro.a $(OBJS)
	ranlib ../lib/libastro.a

depend:
	$(CXX) -M -I../include $(FITSFLAGS) $(SRC) $(PARALLELSRC) > make.dep

clean:
	-(rm -f $(OBJS) $(PARALLELOBJS) ../lib/libastro.a ../lib/libparallel.a)

cpar:
	-(rm -f $(PARALLELOBJS) ../lib/libparallel.a)

docs:
	(cd ../docs; doxygen libpeyton.conf)

includes:
	../build/mkuseall.pl > ../include/astro/useall.h

ifneq ($(MAKECMDGOALS),depend)
include make.dep
endif
