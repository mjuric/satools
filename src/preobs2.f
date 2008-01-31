c =====================================================================
c PREOBS2 (majuric)
c =====================================================================
c PREOBS2- version with angles only, no covariance, two body
c =====================================================================
c  input:  coo   = coordinate type EQU, KEP, CAR, EQP
c          t0    = epoch time (MJD)
c          idsta = station code
c          t1    = prediction time (MJD)
c          east0 = orbital elements vector at time t0
c          h     = absolute magnitude
c          g     = opposition effect coefficient
c          iobs  = observation type
c  output: 
c          IF iobs=1000+x
c          x = right ascension (equatorial J2000), radians
c          y = declination (equatorial J2000), radians
c          hmagn = apparent magnitude, as predicted, from h and g given
c  WARNING: the magnitudes are often very poorly predictable
c
c          IF iobs=2000 + x
c          x = range in AU
c          y = range rate in AU/day
c                         (hmagn is dummy)
c          IF iobs=4000's
c          x = d alpha/dt (radians/day)
c          y = d delta/dt (radians/day)
c                         (hmagn is dummy)
c
c ============INTERFACE===================================================
      subroutine preobs2(coo,t0,idsta,t1,east0,iobs,x,y,h,g,hmagn)
      implicit none
c elements and epoch times
      character*3 coo
      double precision east0(6),t0,t1
c observation type
      integer iobs
c magnitude input and output (only for iobs=1000's)
      double precision h,g,hmagn
c station code
      integer idsta
c observations
      double precision x,y
c ============END INTERFACE===============================================
c asteroid equinoctal elements and cartesian coordinates
      double precision eq(6),enne  
c partial derivatives of alpha, delta, w.r. to elements (not used)  
      double precision dxde(6),dyde(6)
c second derivatives of alpha, delta, w.r. to elements (not used)
      double precision ddxde(6,6),ddyde(6,6)
c vector observations
      double precision obs(4),dobde(4,6)
c functions
      double precision appmag
c control on derivatives
      integer ider
c elongation,distance to Earth, distance to Sun (to compute magnitude)
      include 'phase.h'
c ======== constant of gravitation ==============
      include 'sunmass.h'
c flag for 2-body approximation; must be .false. for full n-body computation
      logical twobo
      twobo=.true.
c****************
c   static memory not required
c****************
c =====================================================================
c coordinate change
      call coocha(east0,coo,gms,eq,'EQU',enne) 
c compute observation   
      ider=0
      IF(iobs/1000.eq.1)THEN   
c astrometric measurements of angles      
         call alfdel (eq,t0,t1,idsta,x,y,dxde,dyde,ider,twobo,
     +        ddxde,ddyde)
c compute apparent magnitude at time of observation
         hmagn=appmag(h,g,dsun,dis,pha)
      ELSEIF(iobs/1000.eq.2)THEN
c radar data
         call rrdot (eq,iobs,t0,t1,idsta,x,y,dxde,dyde,ider,twobo)
      ELSEIf(iobs/1000.eq.4)THEN
c proper motion
         call alfdel2 (eq,t0,t1,idsta,obs,dobde,ider,twobo)
         x=obs(3)
         y=obs(4)
      ELSE
         WRITE(*,*)'preobs: this we have not invented yet, iobs=',iobs
         STOP
      ENDIF
      return
      end


c**************************************
c	Common block variables return
c**************************************
	subroutine phvars(R, phse, umg, dist)
	implicit none
	double precision R, phse, umg, dist
c elements and epoch times
	include 'phase.h'
c return the elements
	R = dsun
	phse = pha
	umg = umag
	dist = dis
	return
	end
