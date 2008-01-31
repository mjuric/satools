PEYTON=	$(HOME)/projects/libpeyton
FFTW=	$(HOME)/projects/fftw_float

LFSINC=	$(LFS)/usr/local/include
LFSLIB=	$(LFS)/usr/local/lib

CFLAGS= -g
#CFLAGS= -O3

SRC=	
OBJ=	

.cpp.o:
	g++ $(CFLAGS) -I$(PEYTON)/include -I$(FFTW)/include -I$(LFSINC) -c $*.cpp -o $*.o

.c.o:
	gcc $(CFLAGS) -I$(PEYTON)/include -I$(FFTW)/include -I$(LFSINC) -c $*.c -o $*.o



all:	printf.x

printf.x:	$(OBJ) printf.o
	g++ -g printf.o $(OBJ) -L$(FFTW)/lib -L$(LFSLIB) -L$(PEYTON)/lib -lastro -lrfftw -lfftw -lCCfits -lcfitsio -lm -lz -o printf.x

clean:
	rm -rf $(OBJ) printf.o *.x
