c ==========================================
c EARTH
c get Earth elements
      SUBROUTINE earth(t0,eqp)
      IMPLICIT NONE
c input: epoch time
      DOUBLE PRECISION t0
c output: elements of Earth (equinoctal, ecliptic)
      DOUBLE PRECISION eqp(6)
c =============JPL EPHEM===============
c data for masses
      INCLUDE 'jplhdr.h'
c output of JPL routine, Julian date, rotation matrix
      double precision et(2),rot(3,3),rrd(6),xea(6),enne
c integers for call to JPl routines
      integer ntarg,ncent,istate
c masses
      INCLUDE 'sunmass.h'
      INCLUDE 'parbep.h'
      INCLUDE 'masses.h'
c ====================================
c JPL Earth vector at observation time
      et(1)=2400000.5d0
      et(2)=t0
      ntarg=3
      ncent=11
c duplicate computation of gmse, in case masjpl has not been called yet
      gmse=gms*(1.d0+cval(11)/cval(18))
* ****** added on Sat Jun 14 1997 ******
* first istate need to be=2  (dpleph calculates also vel.)
      istate=2
      call dpleph(et,ntarg,ncent,rrd,istate)
* Change of reference system EQUM00 ---> ECLM00
      call rotpn(rot,'EQUM','J2000',0.d0,'ECLM','J2000',0.d0)
      call prodmv(xea,rot,rrd)
      call prodmv(xea(4),rot,rrd(4))
c elements of Earth
      call coocha(xea,'CAR',gmse,eqp,'EQU',enne)
      RETURN
      END
c ======================================================================
c EARCAR - get Earth cartesian coordinates (ecliptic J2000)
c ======================================================================
      SUBROUTINE earcar(t0,xea,ifla)
      IMPLICIT NONE
c input: epoch time, flag for getting Earth (heliocentric; ifla=1)
c        or Sun (barycentric; ifla=2)
      DOUBLE PRECISION t0
      INTEGER ifla
c output: heliocentric state vector of Earth (equinoctal, ecliptic)
c      or barycentric state vector of Sun (equinoctal, ecliptic)
      DOUBLE PRECISION xea(6)
c =============JPL EPHEM===============
c data for masses
      INCLUDE 'jplhdr.h'
c output of JPL routine, Julian date, rotation matrix
      double precision et(2),rot(3,3),rrd(6)
c integers for call to JPl routines
      integer ntarg,ncent,istate
c ====================================
c JPL Earth vector at observation time
      et(1)=2400000.5d0
      et(2)=t0
      if (ifla.eq.1) then
       ntarg=3
       ncent=11
      else
       ntarg=11
       ncent=12
      endif
* ****** added on Sat Jun 14 1997 ******
* first istate need to be=2  (dpleph calculates also vel.)
      istate=2
      call dpleph(et,ntarg,ncent,rrd,istate)
* Change of reference system EQUM00 ---> ECLM00
      call rotpn(rot,'EQUM','J2000',0.d0,'ECLM','J2000',0.d0)
      call prodmv(xea,rot,rrd)
      call prodmv(xea(4),rot,rrd(4))
      RETURN
      END



