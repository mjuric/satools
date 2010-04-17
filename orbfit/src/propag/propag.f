c last update  March 1999 by A. Milani
c =====================================================================
c PROPAG vers. 1.9
c =====================================================================
c N+1-body problem propagator
c
c This version uses JPL ephemerides as source for the planetary positions
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
c              2=second derivatives (3x6x6 tensor) 2-body approximation
c  output:
c        xast position and velocity vector in heliocentric cartesian 
c               coordinates for the asteroid at time t1
c        xea  position and velocity vector in heliocentric cartesian 
c               coordinates for the Earth at time t1
c        dxde first derivatives of position vector with respect to elements 
c
c        ddxde second derivatives of position vector with respect to elem.
c
c This routine cannot compute n-body second derivatives: if ider.ge.2,
c the matrix ddxde is computed by prop2b. 
c
c ================INTERFACE===========================================
      subroutine propag(t0,east,t2,xast,xea,ider,dxde,ddxde)
      implicit none
c ===============INPUT==============================
c elements (equinoctal), epoch of the elements (MJD), target epoch
      double precision east(6),t0,t2
c order of derivatives 0,1,2
      integer ider
c ===============OUTPUT=============================
c cartesia coord., at time t2, of the asteroid, of the Earth, derivatives 
      double precision xast(6),xea(6),dxde(6,6),ddxde(3,6,6)
c ===========END INTERFACE===========================================
c ========INCLUDE HEADERS===================
c options common
      include 'model.h'
      include 'iclap.h'
      include 'comint.h'
c ======== constant of gravitation ==============
      include 'sunmass.h'
c =============STATE VECTOR AND DERIVATIVES======================
c main state vector for only one asteroid + variational eq.
      INCLUDE 'nvarx.h'
      double precision y1(nvarx),y2(nvarx)
c cartesian pos. of asteroid, 2-body approximation
      double precision xast2b(6)
c matrices of partial derivatives
      double precision dx0de(6,6),dxdx0(6,6),ddummy(6,6)
c =============RESTART CONTROL=========================
c times: current, epoch at previous call
      double precision t1,t0s
c asteroid elements at the previous call
      double precision easts(6)
c restart control
      integer nfl
c dirty trick to force restart anyway
      include 'restart.h'
c startup from a non-close approaching state at each integration restart
      INCLUDE 'closta.h'
c close approach at initial time monitoring
      INTEGER iplam
      DOUBLE PRECISION texit
c ===================================================
c stepsize: as given by selste, current, previous
      double precision hgiv,h,hs
c flag for interpolation of position only or pos and vel.
      integer istate
      common/cstate/istate
c =============
c integers for loop control
      integer i
c integers for dimensions
      integer nv,nv1,nvar,nvar2
c other reals: eccentricity, mean motion 
      double precision ecc,enne
c dirty trick to pass ider to right hand side
      integer ide
      common/deriv/ide
c initialisation control (to have dummy initial conditions)
      integer lflag
* **************************************
c static memory allocation
      save
c
      data lflag/0/
c **************************************
c  dummy initial conditions to force restart the first time
      if(lflag.eq.0)then
         lflag=1      
c Store fictitious epoch time and elements for the asteroid
c (to be sure they are different the first time)
         t0s=-1d+55
         do 72 i=1,6
 72        easts(i)=0.d0
      endif
c =====================================================================
c JPL Earth vector at observation time
      CALL earcar(t2,xea,1)
c =====================================================================
c Check if time and/or elements changed: we could not need to compute 
c initial conditions for the asteroid
c     write(99,*)t0,t1,t2,restar
      if(t0.ne.t0s.or.easts(1).ne.east(1).or.easts(2).ne.east(2).
     +   or.easts(3).ne.east(3).or.easts(4).ne.east(4).
     +   or.easts(5).ne.east(5).or.easts(6).ne.east(6)
     +   .or.abs(t2-t1).gt.abs(t2-t0).or.restar
     +   )then
c     write(99,*)east

*  new arc; propin needs to be informed
         nfl=0
c also cloapp needs to be informed
         clost=.true.
         CALL clotest(t0,east,iplam,texit)  
         IF(iplam.ne.0)write(99,*)'initial close app. with plan. ',iplam 
c =====================================================================
c Compute asteroid cartesian elements at time $t0$
         call prop2b(t0,east,t0,xast,gms,1,dx0de,ddxde)
         DO  i=1,6
           easts(i)=east(i)
         ENDDO
* =====================================================================
c choices about integration method and stepsize
         if(imet.eq.0) then
c automatical choise of numerical integration method
           call selmet(east)
         else
            icmet=imet
         endif
c read masses from JPL header (stored in a common...)
* ****** moved March 12, 1999 ***********
c alignement of masses might have changed, because of different list of 
c asteroids (to avoid self perturbation) 
         call masjpl
* ****** added on Sat Jun 14 1997 ******
c control if both position and vel. are needed in dpleph
         if(icmet.eq.3.and.iclap.eq.1) then
           istate=2
         elseif(irel.eq.1)then
           istate=2
         else
           istate=1
         endif
* ***************************************
* selection of an appropriate stepsize 
         if(icmet.ne.3)then
c for the multistep:
c the starter uses the same stepsize
            ecc=sqrt(east(2)**2+east(3)**2) 
            enne=sqrt(gms/east(1)**3)
            call selste(ecc,enne,error,mms,hms,hgiv)
            h=hgiv
         elseif(icmet.eq.3)then
c for Everhart: step selection is automatic, hev is the maximum
            h=hev
         endif
c sign control:
         if(t2-t0.lt.0.d0)then
            h=-h
         endif
c =====================================================================
c position vector dimension
         nv=3
c derivatives required?
         ide=ider
         if(ider.ge.1)then
c Variational equations: 3 X 6 components
            nv1=18
         else
            nv1=0
         endif
c total number of variables (and total number of positions)
         nvar2=nv+nv1
         nvar=nvar2*2
c =====================================================================
c Vector y1, length 6, contains at least
c the asteroid position and velocity 
         DO  i=1,3
            y1(i)=xast(i)
            y1(i+nvar2)=xast(i+3)
         ENDDO
         if(ider.ge.1)then
c in this case the vector y1 contain also the matrices 
c of partial derivatives; then we need to
c initialise the  variation matrix as the 6 X 6 identity
            CALL inivar(y1,nvar2,nvar)
         endif
c =====================================================================
c Restart from $t0$
         t0s=t0
         t1=t0
      else
c old arc:
c stepsize was already decided
         if(icmet.eq.3)then
            h=hev
         else
            h=hgiv
         endif
c sign control:
         if(t2-t1.lt.0.d0)then
            h=-h
         endif
c need to check that direction is not changed
         if(h*hs.lt.0.d0)then
c restart is necessary because of U-turn
            nfl=0
         else
c restart not necessary; propin can go on 
            nfl=1
         endif
      endif
c =====================================================================
c propagator to compute asteroid and planets orbits
      call propin(nfl,y1,t1,t2,y2,h,nvar2,nvar)
      hs=h
c =====================================================================
c Asteroid coordinates
      do  i=1,3
          xast(i)=y2(i)
          xast(i+3)=y2(i+nvar2)
      enddo
      if(ider.ge.1)then
c if partial derivative are required
c First parzial derivatives: rewrap vector into 6x6 matrix
         CALL varwra(y2,dxdx0,nvar,nvar2)
c =====================================================================
c Chain rule: we compute dxde=dxdx0*dx0de
         call mulmat(dxdx0,6,6,dx0de,6,6,dxde)
c second derivatives can be computed only in 2 body approximation
         if(ider.ge.2)then
            call prop2b(t0,east,t1,xast2b,gms,2,ddummy,ddxde)
         endif
      endif
      return
      end
c =====================================================================
c inivar
c
c initialise the  variation matrix as the 6 X 6 identity
c ================================
      SUBROUTINE inivar(y1,nvar2,nvar)
      IMPLICIT NONE
      INTEGER nvar,nvar2
      DOUBLE PRECISION y1(nvar)
c end interfface
      INTEGER i,j,iii,ij        
      iii=3
      do 7 j=1,6
         do  i=1,3
            ij=i+3*(j-1)
            y1(iii+ij)=0.d0
            y1(iii+ij+nvar2)=0.d0
            if(i.eq.j)then
               y1(iii+ij)=1.d0
            elseif(j.eq.i+3)then
               y1(iii+ij+nvar2)=1.d0
            endif
         enddo
 7    continue
      RETURN
      END   
c ====================================================
c varwar
c
c First parzial derivatives: rewrap vector into 6x6 matrix
c ====================================================
      SUBROUTINE varwra(y2,dxdx0,nvar,nvar2)
      IMPLICIT NONE
      INTEGER nvar,nvar2
      DOUBLE PRECISION dxdx0(6,6),y2(nvar)
      INTEGER i,j,ij
c ====================================================
      DO j=1,3
         DO  i=1,3
            ij=i+3*(j-1)+3
            dxdx0(i,j)=y2(ij)
            dxdx0(i,j+3)=y2(ij+9)
            dxdx0(i+3,j)=y2(ij+nvar2)
            dxdx0(i+3,j+3)=y2(ij+9+nvar2)
         ENDDO 
      ENDDO
      RETURN
      END





