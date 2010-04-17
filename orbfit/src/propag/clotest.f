c ====================================================
c CLOTEST
c tests for presence of a close approach at the time t0 for 
c asteroid with equinoctal elements east. On output iplnet is the number
c of the approaching planet, 0 if none. 
c texit is delay time advisable to get out of the close approach sphere (in days).
c ********************************************************
c WARNING: this version actually checks only for close approaches to the Earth
c should be fixed to test for all planets.
c=====================================================
      SUBROUTINE clotest(t0,east,iplanet,texit)
      IMPLICIT NONE
c input: time, asteroid elements
      DOUBLE PRECISION t0,east(6)
c output: planet being approached (0 if none), time to get clear
      INTEGER iplanet
      DOUBLE PRECISION texit
c end interface
      DOUBLE PRECISION xea(6),xast(6),dist,enne
c mass of the sun
      INCLUDE 'sunmass.h'
c radius of the close appooach sphere for the planets
      INCLUDE 'parbep.h'
      INCLUDE 'masses.h'
c JPL Earth vector at observation time
      CALL earcar(t0,xea,1)
c cartesian coordinates of the asteroid
      CALL coocha(east,'EQU',gms,xast,'CAR',enne)
c distance 
      dist=sqrt((xea(1)-xast(1))**2+(xea(2)-xast(2))**2+
     +      (xea(3)-xast(3))**2)
c test
      IF(dist.le.dmin(3))THEN
c        write(99,*)' clotest: warning! close approach to Earth'
c        write(99,*)' at the initial epoch, dist=',dist
         iplanet=3
         texit=20.d0
      ELSE
         iplanet=0
         texit=0.d0
      ENDIF
      RETURN
      END
      
