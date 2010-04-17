c =====================================================================
c ALFDEL2
c =====================================================================
c Computation of alpha, delta, alphadot, deltadot  and their derivatives
c =====================================================================
c
c Input
c    tk: epoch time
c    tauj: observation time
c    ioj: station code
c    east: equinoctal orbital elements vector (a,h,k,p,q,lambda) 
c                    at time tk
c    ider: flag for derivatives options:
c        0 no derivatives
c        1 only first deriv. of alpha,delta w.r. to (east,tk,tauj)
c    twobo: logical flag for 2-body approximation; if .true., 2-body
c        approximation (but Earth position comes from JPL ephem); if .false.,
c        all orbit propagations are full n-body
c Output
c   alj,dej: alpha,delta  computed at time tauj
c   adot,ddot their time derivatives
c   dade,ddde:  matrices of first derivatives (if required)
c   ddade,dddde: matrices of second deriv. (if required, only in 2-body approx)
c
c ==============INTERFACE============================================
      subroutine alfdel2 (east,tk,tauj,iocj,obs,dobde,ider,twobo)
      implicit none
c ==============INPUT==========================
c flag to control two-body approximation: if .false., full n-body
      logical twobo
c times: epoch time for asteroid elements, observation time (MJD)
      double precision tk,tauj
c asteroid equinoctal elements 
      double precision east(6)
c cartesian coordinates of the Earth
      double precision xea(6)
c observatory code
      integer iocj
c flag to control computation of derivatives
      integer ider
c ============OUTPUT==============================
c observations: alpha (right ascension) delta (declination), in RAD
c alphadot, deltadot in RAD/day
      double precision obs(4)
c partial derivatives of obs, w.r. to asteroid coordinates
      double precision dobde(4,6)
c =============END INTERFACE=========================================
c asteroid cartesian coordinates
      double precision xast(6)
c first partial derivatives of alpha, delta, adot, ddotw.r. to ast. coordinates
      double precision dobdx(4,6)
c first, second derivatives of cartesian coordinates with respect to elements
      double precision dxde(6,6),ddxde(3,6,6)
c *************************************************************************
c****************
c   static memory not required
c****************
      IF(ider.gt.1.or.ider.lt.0)THEN
         write(99,*)'alfdel2; ider=',ider,' not understood'
         STOP
      ENDIF
c Orbit propagation:
      if(twobo)then
c 2 body version
         call propa2 (tk,east,tauj,xast,xea,ider,dxde,ddxde)
      else
c full n-body numerical integration
         call propag (tk,east,tauj,xast,xea,ider,dxde,ddxde)
      endif
c Computation of observations
      call ossdif2(xast,xea,tauj,iocj,obs,ider,dobdx)
      if(ider.lt.1)return
c derivatives with respect to equinoctal elements
      CALL mulmat(dobdx,4,6,dxde,6,6,dobde)
      return
      end
c =====================================================================
c OSSDIF2
c =====================================================================
c Corrections to observations
c =====================================================================
c Input
c xast asteroid cartesian coordinates at time tauj
c xea Earth cartesian coordinates at time tauj
c     both in the ecliptic system
c tauj observation time
c ioc station code
c ider flag for derivatives options:
c       =0 no derivatives
c       <2 only first deriv. of $\alpha,\delta$ w.r. 
c to positions
c       great equal 2 also second partial derivatives 
c (approximation with 2-body case)
c
c Output
c obs=alpha,delta,adot,ddot computed at time tauj (in the equatorial system)
c (if required)
c dobdx matrix of first derivatives w. r. to ecliptic positions 
c (if required)
c =====================================================================
      subroutine ossdif2(xast,xea,tauj,ioc,obs,ider,dobdx)
      implicit none
c =====================================================================
      include 'trig.h'
      include 'model.h'
c =====================================================================
c observations alpha, delta (equatorial, radians), aodt,ddot and time
      double precision obs(4),tauj
c cartesian coordinates, ecliptic, of asteroid, of earth, vector difference
c WARNING: even if the velocities are not always propagated, they are available
c at the time of the observations
      double precision xast(6),xea(6),d(6)
c partials of equatorial alpha,delta,adot,ddot w.r.t. cartesian ecliptic coord. 
      double precision dobdx(4,6)
c topocentric position of the observatory, obs. code
      double precision xo(3),vo(3)
      integer idst,ioc
c elongation,distance to Earth, distance to Sun (to compute magnitude)
      include 'phase.h'
c phase and elongation cosine
      double precision cospha,coselo
c control on no. derivatives, bits in mantissa
      integer ider,n
c rounding off, auxiliary var.
      double precision errm,dz
c real functions
      double precision roff,vsize,prscal
c integer loop index
      integer i
c rotation matrices, rotated vectors
      double precision rot(3,3),rotinv(3,3),deq(6),tmp(3),ddd(3)
c scalar variables to preserve old code
      double precision alpha,delta
c partials of equatorial alpha, delta w.r. to cartesian ecliptic coord. 
      double precision dadx(3),dddx(3),ddadx(3,3),ddddx(3,3)
c aux. var for computation of second derivatives
      double precision x,y,z
      double precision den,x2,y2,z2,x4,y4
      double precision x2y2,x2z2,y2z2
c =====================================================================
      integer lflag
*********************************
* static memory allocation only for:
      save lflag,errm, rot,rotinv
********************************* 
      data lflag/0/
      if(lflag.eq.0)then
         errm=roff(n)
* Change of reference system EQUM00 ---> ECLM00
c Rotation matrix from the reference system in which the
c observations are given (mean equatorial J2000) to the reference 
c system in which the orbit is computed
         call rotpn(rot,'EQUM','J2000',0.d0,'ECLM','J2000',0.d0)
         call transp(rot,3,3,rotinv)
         lflag=1
      endif
c =====================================================================
c Difference vector
      call vdiff (xast,xea,d)
      call vdiff(xast(4),xea(4),d(4))
c =====================================================================
c Displacement of the station with respect to the center of the Earth
      if(istat.gt.0)then
         idst=ioc
         call pvobs(tauj,idst,xo,vo)
         call vdiff(d,xo,d)
         call vdiff(d(4),vo,d(4))
      endif
c =====================================================================
c Aberration (only time delay)
      if(iaber.gt.0)then
         call aber1(d,xast(4),d)
      endif
c =====================================================================
c Computation of solar distance, earth distance, phase, elongation
      dsun=vsize(xast)
      dis=vsize(d)
      cospha=prscal(d,xast)/(dis*dsun)
      pha=acos(cospha)
      coselo=-prscal(d,xea)/(dis*vsize(xea))
      elo=acos(coselo)
c	  write(99,*)'alfdel2; dsun=',dsun,'phase=',pha
c =====================================================================
c rotation to the equatorial reference system
      call prodmv(deq,rotinv,d)
      call prodmv(deq(4),rotinv,d(4))
      call vcopy(6,deq,d)
c =====================================================================
c galactic latitude
      gallat=pig/2d0-acos((d(1)*gax+d(2)*gay+d(3)*gaz)/dis)
c =====================================================================
c Computation of observation: right ascension (radians)
      dz=d(1)**2+d(2)**2
      if (dz.le.errm) then
          alpha=0.d0
      else
          alpha=atan2(d(2),d(1))
          if (alpha.lt.0.d0) then
              alpha=alpha+dpig
          endif
      endif
c Computation of observation: declination (radians)
      delta=asin(d(3)/dis)
c =====================================================================
c Computation of first derivatives of $\alpha$ and $\delta$ w.r. to positions 
c (if required): we derived eq. (2.20)
      dadx(1)=-d(2)/dz
      dadx(2)=d(1)/dz
      dadx(3)=0.d0
      dddx(1)=-d(3)*(d(1)/(sqrt(dz)*dis**2))
      dddx(2)=-d(3)*(d(2)/(sqrt(dz)*dis**2))
      dddx(3)=sqrt(dz)/dis**2
c =====================================================================
c Apparent motion: 
      adot=prscal(dadx,d(4))
      ddot=prscal(dddx,d(4))
c store into obs vector
      obs(1)=alpha
      obs(2)=delta
      obs(3)=adot
      obs(4)=ddot
c check if observation partials are required
      if(ider.eq.0)RETURN
c =====================================================================
c partials of alpha, delta have already been computed
c partials of adot,ddot with respect to velocities are the same
c rotation to the equatorial reference system
      call prodmv(tmp,rot,dadx)
      do  i=1,3
        dobdx(1,i)=tmp(i)
        dobdx(1,i+3)=0.d0
        dobdx(3,i+3)=tmp(i)
      enddo
      call prodmv(tmp,rot,dddx)
      do i=1,3
        dobdx(2,i)=tmp(i)
        dobdx(2,i+3)=0.d0
        dobdx(4,i+3)=tmp(i)
      enddo
c =====================================================================
c partials of adot,ddot with respect to positions require the 
c second derivatives of alpha, delta with respect to the
c equatorial reference system
c =====================================================================
c Computation of second derivatives of $\alpha$ w.r. to positions
      ddadx(1,1)=2.d0*d(1)*d(2)/dz**2
      ddadx(1,2)=(d(2)**2-d(1)**2)/dz**2
      ddadx(2,1)=ddadx(1,2)
      ddadx(2,2)=-ddadx(1,1)
      ddadx(3,1)=0.d0
      ddadx(3,2)=0.d0
      ddadx(3,3)=0.d0
      ddadx(2,3)=0.d0
      ddadx(1,3)=0.d0
c Computation of second derivatives of $\delta$ w.r. to positions
      den=1.d0/(dis**2*dz*sqrt(dz))
      x=d(1)
      y=d(2)
      z=d(3)
*
      x2=x*x
      y2=y*y
      z2=z*z
      x4=x2*x2
      y4=y2*y2
*
      x2y2=x2*y2
      x2z2=x2*z2
      y2z2=y2*z2
*
      ddddx(1,1)=z*(2.d0*x4+x2y2-y2z2-y4)*den
      ddddx(2,2)=z*(2.d0*y4+x2y2-x2z2-x4)*den
      ddddx(1,2)=x*y*z*(z2+3.d0*x2+3.d0*y2)*den
      ddddx(2,1)=ddddx(1,2)
      ddddx(3,3)=-2.d0*z*dz**2*den
      ddddx(1,3)=x*dz*(z2-x2-y2)*den
      ddddx(3,1)=ddddx(1,3)
      ddddx(2,3)=y*dz*(z2-x2-y2)*den
      ddddx(3,2)=ddddx(2,3)
c =======================================================
c chain rule for derivatives of adot
      call prodmv(ddd,ddadx,d(4))
c =======================================================
c rotation to the equatorial reference system
      call prodmv(tmp,rot,ddd)
      do  i=1,3
        dobdx(3,i)=tmp(i)
      enddo
c =======================================================
c chain rule for derivatives of adot
      call prodmv(ddd,ddddx,d(4))
c =======================================================
c rotation to the equatorial reference system
      call prodmv(tmp,rot,ddd)
      do i=1,3
        dobdx(4,i)=tmp(i)
      enddo
c
      return
      end





