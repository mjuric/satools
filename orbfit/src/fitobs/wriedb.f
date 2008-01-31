c =======================================================
c WRIEDB
c interface to Xephem, writes edb format files
c patch 1.6.1, A. Milani, May 2, 1998 
c ============INTERFACE==================================
      SUBROUTINE wriedb(tdt,elm,astna,hmag,gmag,norb)
      IMPLICIT NONE
c ========INPUT==========================================
c number of lternative solutions
      INTEGER norb
c epoch time (TDT), orbital elements (equinoctal)
      DOUBLE PRECISION tdt,elm(6,norb)
c asteroid name
      CHARACTER*18 astna
c magnitudes
      DOUBLE PRECISION hmag,gmag
c ========OUTPUT TO .edb FILE============================
c keplerian elements
      DOUBLE PRECISION ek(6)
c ==========END INTERFACE================================
c trigonometric constants
      INCLUDE 'trig.h'
c mass of the Sun, gravitaional constant
      INCLUDE 'sunmass.h'
c loop indexes
      INTEGER i,j
c scalar temporaries
      DOUBLE PRECISION enne
c calendar date
      INTEGER iday,imo,iy
      DOUBLE PRECISION hour,day
c asteroid name manipulations
      CHARACTER alt*4,astnam*23
      INTEGER ld,lench
c output unit
      INTEGER iun
      CHARACTER*60 file
      CHARACTER*180 rec
c =======================================================
c open output file (for disk interface)
      file='./xephem/altorb.edb'
      CALL filopn(iun,file,'UNKNOWN')
c main loop on alternate orbits
      DO 1 i=1,norb
c conversion to keplerian elements, angles in degrees
        CALL coocha(elm(1,i),'EQU',gms,ek,'KEP',enne)      
        DO j=3,6
          ek(j)=ek(j)*degrad
        ENDDO
c calendar date from mjd 
      CALL mjddat(tdt,iday,imo,iy,hour)
      day=hour/24.d0
      day=day+iday
c alternate names
      WRITE(alt,101)i
 101  FORMAT(i4)
      ld=lench(astna)
      astnam=astna(1:ld)//'_'//alt
      CALL rmsp(astnam,ld)
c intermediate internal output
      rec=' '
      WRITE(rec,102)astnam(1:ld)
 102  FORMAT(A)
      WRITE(rec(ld+1:180),100)ek(3),ek(4),ek(5),ek(1),enne,ek(2),ek(6),
     +   imo,day,iy,hmag,gmag
 100  FORMAT(',e,',3(f14.9,','),2(f15.10,','),2(f14.9,','),
     +   i2,'/',f9.6,'/',i4,',2000,',f5.2,',',f4.2)
c external output 
      WRITE(iun,102)rec
 1    continue
      CALL filclo(iun,' ')
      RETURN
      END




