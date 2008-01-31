c last update  Jun 14 1997 by A. Milani
c =====================================================================
c PROPA2 
c =====================================================================
c 2-body problem propagator
c
c This version uses JPL ephemerides as source for the Earth position
c
c  WARNING: the input elements east and the output cartesian
c           coordinates and derivatives are in ecliptic (mean of J2000.0)
c           coordinates.
c =====================================================================
c
c  input: t0 epoch time
c         t2 prediction time
c         east equinoctal orbital elements vector a,h,k,p,q,lambda 
c               at time t0
c         ider 0=no partials 1= partials dx/deast (3x6 matrix)
c              2=second derivatives (3x6x6 tensor) all in 2-body approximation
c  output:
c        xast position and velocity vector in heliocentric cartesian 
c               coordinates for the asteroid at time t1
c        xea  position and velocity vector in heliocentric cartesian 
c               coordinates for the Earth at time t1
c        dxde first derivatives of position vector with respect to elements 
c
c        ddxde second derivatives of position vector with respect to elem.
c
c ======================INTERFACE=======================================
      subroutine propa2(t0,east,t2,xast,xea,ider,dxde,ddxde)
      implicit none
c =========INPUT========================================================
c times
      double precision t0,t2,east(6)
      integer ider
c =========OUTPUT=======================================================
      double precision xast(6),xea(6),dxde(6,6),ddxde(3,6,6)
c =====================================================================
c options common
      include 'model.h'
c ======== constant of gravitation ==============
      include 'sunmass.h'
c workspace for rotations, strings to define ref. systems
      double precision rrd(6),et(2),rot(3,3)
c integers for call to JPl routines
      integer ntarg,ncent,istate
c initialisation: needed only for rotation matrix rot
* **************************************
c static memory allocation only for:
      save rot,lflag
* **************************************
      integer lflag
      data lflag /0/
c =====================================================================
c JPL Earth vector at observation time
      et(1)=2400000.5d0
      et(2)=t2
      ntarg=3
      ncent=11
* ****** added on Sat Jun 14 1997 ******
* first istate need to be=2  (also vel. required for aberration)
      istate=2
* **************************************
      call dpleph(et,ntarg,ncent,rrd,istate)
* Change of reference system EQUM00 ---> ECLM00
      if(lflag.eq.0)then
         call rotpn(rot,'EQUM','J2000',0.d0,'ECLM','J2000',0.d0)
         lflag=1
      endif
      call prodmv(xea,rot,rrd)
      call prodmv(xea(4),rot,rrd(4))
*******************
c two body propagation
      call prop2b(t0,east,t2,xast,gms,ider,dxde,ddxde)
c
      return
      end
c =====================================================================




