c =====================================================================
c PREOBS-PREOBC
c =====================================================================
c PREOBS- version with angles only, no covariance
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
      subroutine preobs(coo,t0,idsta,t1,east0,iobs,x,y,h,g,hmagn)
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
      twobo=.false.
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
         WRITE(0,*)'preobs: this we have not invented yet, iobs=',iobs
         STOP
      ENDIF
      return
      end
c =====================================================================
c PREOBC- version with covariance, linear theory
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
c          IF iobs=1000's
c          x = right ascension (equatorial J2000), radians
c          y = declination (equatorial J2000), radians
c          hmagn = apparent magnitude, as predicted, from h and g given
c  WARNING: the magnitudes are often very poorly predictable
c
c          IF iobs=2000's
c          x = range in AU
c          y = range rate in AU/day
c                         (hmagn is dummy)
c          IF iobs=4000's
c          x = d alpha/dt (radians/day)
c          y = d delta/dt (radians/day)
c                         (hmagn is dummy)
c
c  In the linear approximation, the ellipse of confidence has principal axes
c          along axes; the semiaxes lenghts are sig 
c
c ============INTERFACE===================================================
      subroutine preobc(coo,t0,idsta,t1,east0,h,g,gamm0,
     +    iobs,x,y,hmagn,gamad,sig,axes)
      implicit none
c ============= input ====================================================
c elements and epoch times, covraiance at t0
      character*3 coo
      double precision east0(6),t0,t1,gamm0(6,6)
c magnitude
      double precision h,g,hmagn
c station code
      integer idsta
c observation type
      integer iobs
c ============= output ===================================================
c observations
      double precision x,y
c covariance
      double precision gamad(2,2),axes(2,2),sig(2)
c ============END INTERFACE===============================================
c asteroid equinoctal elements and cartesian coordinates
      double precision eq(6),enne  
c partial derivatives of alpha, delta, w.r. to elements (by columns, by rows)
      double precision dxydet(6,2),dadde(2,6)
c second derivatives of alpha, delta, w.r. to elements (not used)
      double precision ddade(6,6),dddde(6,6)
c jacobian matrices of partial derivatives, eigenvalues, workspace
      double precision eigval(2),tmp(2,6),fv1(2),fv2(2)
c covariance, normal matrices
      DOUBLE PRECISION gameq(6,6)
c jacobian matrices of partial derivatives
      DOUBLE PRECISION deqde(6,6),deqdet(6,6),tmp6(6,6)
c vector observations
      double precision obs(4),dobde(4,6)
c control on derivatives
      integer ider
c error flag
      integer ierr
c loop indexes 
      integer i,j
c functions
      double precision appmag
c elongation,distance to Earth, distance to Sun (to compute magnitude)
      include 'phase.h'
c ======== constant of gravitation ==============
      include 'sunmass.h'
c flag for 2-body approximation; must be .false. for full n-body computation
      logical twobo
      twobo=.false.
c****************
c   static memory not required
c****************
c =====================================================================
c coordinate change
      call cooder(east0,coo,gms,eq,'EQU',enne,deqde)
c compute normal and covariance matrix for equinoctal elements
      CALL mulmat(deqde,6,6,gamm0,6,6,tmp6)
      CALL transp(deqde,6,6,deqdet)
      CALL mulmat(tmp6,6,6,deqdet,6,6,gameq)
c compute observation   
      ider=1
      IF(iobs/1000.eq.1)THEN   
c astrometric measurements of angles      
         call alfdel (eq,t0,t1,idsta,x,y,dxydet(1,1),dxydet(1,2),
     +        ider,twobo,ddade,dddde)
c compute apparent magnitude at time of observation
         hmagn=appmag(h,g,dsun,dis,pha)
      ELSEIF(iobs/1000.eq.2)THEN
c radar data
         call rrdot (eq,iobs,t0,t1,idsta,x,y,dxydet(1,1),dxydet(1,2),
     +        ider,twobo)
      ELSEIf(iobs/1000.eq.4)THEN
c proper motion
         call alfdel2 (eq,t0,t1,idsta,obs,dobde,ider,twobo)
         x=obs(3)
         y=obs(4)
         DO i=1,2
           DO j=1,6
             dxydet(j,i)=dobde(i+2,j)
           ENDDO
         ENDDO
      ELSE
         WRITE(0,*)'preobc2: this we have not invented yet, iobs=',iobs
         STOP
      ENDIF
c =====================================================================
c compute covariance of alpha, delta
      call transp(dxydet,6,2,dadde)
      call mulmat(dadde,2,6,gameq,6,6,tmp)
      call mulmat(tmp,2,6,dxydet,6,2,gamad)
c compute ellipse of confidence
c eigenvalues
      call rs(2,2,gamad,eigval,1,axes,fv1,fv2,ierr)
      do 1 i=1,2
        if(eigval(i).gt.0.d0)then
           sig(i)=sqrt(eigval(i))
        else
           write(0,*) 'non positive eigenvalue'
           sig(i)=0.d0
        endif
 1    continue

      return
      end




