c =====================================================================
c PROELE-PROELC 
c =====================================================================
c interface to N+1-body problem propagator (subroutine propag)
c
c This version uses JPL ephemerides as source for the planetary positions
c
c  WARNING: the input and output elements east0, east1 
c           are in the ecliptic (mean of J2000.0) system
c
c  WARNING: input and output use the same coo. To transform
c           also the coordinate type, combine with coocha/cooder
c =====================================================================
c PROELE- version with elements only, no covariance
c =====================================================================
c
c  input: coo coordinate type EQU, KEP, CAR, EQP
c         t0 epoch time (MJD)
c         t1 prediction time (MJD)
c         east0  orbital elements vector  at time t0
c  output:
c         east1  orbital elements for the asteroid at time t1
c ============INTERFACE===================================================
      subroutine proele(coo,t0,east0,t1,east1)
      implicit none
c equinoctal elements and epoch times
      character*3 coo
      double precision east0(6),east1(6),t0,t1
c ============END INTERFACE===============================================
c cartesian coordinates, mean motion, derivatives(not used)
      double precision xastr(6),xear(6),enne,dxde(6,6),ddxde(3,6,6)
c equinoctal elements
      double precision eq(6)
c ======== constant of gravitation ==============
      include 'sunmass.h'
c****************
c   static memory not required
c****************
c =====================================================================
c change coordinates
      call  coocha(east0,coo,gms,eq,'EQU',enne)
c =====================================================================
c call private propagation routine, derivatives not required
      call propag(t0,eq,t1,xastr,xear,0,dxde,ddxde)
c compute new elements
      call coocha(xastr,'CAR',gms,east1,coo,enne)
      return
      end
c =====================================================================
c PROELC- version with elements and covariance propagation
c =====================================================================
c
c  input: coo coordinate type EQU, KEP, CAR, EQP
c         t0 epoch time (MJD)
c         t1 prediction time (MJD)
c         east0 orbital elements vector at time t0
c         gamma0 covariance matrix for the elements east0 at epoch t0
c         c0 normal matrix for the same
c  output:
c         east1 orbital elements for the asteroid at time t1
c         gamma1 covariance matrix for the elements east1 at epoch t1
c         c1 normal matrix for the same 
c ============INTERFACE===================================================
      subroutine proelc(coo,t0,east0,gamma0,c0,t1,east1,gamma1,c1)
      implicit none
c elements and epoch times
      character*3 coo
      double precision east0(6),east1(6),t0,t1
c normal matrices
      DOUBLE PRECISION c0(6,6),c1(6,6)
c covariance matrices
      double precision gamma0(6,6),gamma1(6,6)
c ============END INTERFACE===============================================
c equinoctal elements
      double precision eq(6)
c cartesian coordinates, mean motion, derivatives
      double precision xastr(6),xear(6),enne,dxde(6,6),ddxde(3,6,6)
c derivatives of elements w.r. to cartesian, ele w.r. elements, workspaces
      double precision dedx(6,6),dede(6,6),dedet(6,6),tmp(6,6),der0(6,6)
      DOUBLE PRECISION dedein(6,6),det,dedint(6,6)
      INTEGER ising,invop
c ======== constant of gravitation ==============
      include 'sunmass.h'
c****************
c   static memory not required
c****************
c =====================================================================
c     change coordinates; der0=d(eq)/d(east0)
      call  cooder(east0,coo,gms,eq,'EQU',enne,der0)
c =====================================================================
c call private propagation routine, requiring derivatives
      call propag(t0,eq,t1,xastr,xear,1,dxde,ddxde)
c compute new elements, wtih partial derivatives
      call cooder(xastr,'CAR',gms,east1,coo,enne,dedx)
c chain rule to obtain d(east1)/d(east0)
      call mulmat(dedx,6,6,dxde,6,6,tmp)
      call mulmat(tmp,6,6,der0,6,6,dede)
c covariance matrix is propagated by similarity transformation
      call mulmat(dede,6,6,gamma0,6,6,tmp)
      call transp(dede,6,6,dedet)
      call mulmat(tmp,6,6,dedet,6,6,gamma1)
c normal matrix is propagated by similarity transformation
      call mcopy(6,6,dede,dedein)
      invop=1
      CALL matin(dedein,det,6,6,6,ising,invop)
      IF(ising.ne.0)THEN
         write(99,*)'PROELC: this should not happen'
      ENDIF
      CALL mulmat(c0,6,6,dedein,6,6,tmp)
      CALL transp(dedein,6,6,dedint)
      CALL mulmat(dedint,6,6,tmp,6,6,c1)
      return
      end




