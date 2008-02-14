c ===================================================================
c WHICOR
c ===================================================================
c interrogation routine for inew, icor
      subroutine whicor(inter,icor,ncor,inew)
      implicit none
      integer iansw,inew,ncor,icor(6),inter,i
c Choose method (NOW FIXED AT PSEUDO-NEWTON)
      inew=2
c60   write(0,*)' 1=true Newton 2=pseudo Newton'
c     read(*,*)inew
c     if(inew.ne.1.and.inew.ne.2)then
c           write(0,*)'This we have not invented yet'
c           goto 60
c     endif
c interactive/automatic default version
      if(inter.eq.0)then
         do 59 i=1,6
           icor(i)=1
 59      enddo
         ncor=6
         return
      else
c interactive version
c
c Component of orbital element vector that need to be corrected
         ncor=0
         do 63 i=1,6
           write(0,*)'Element:  ',i,   ' 1=correct, 0=no'
           read(*,*) iansw
           if(iansw.ne.0)then
              ncor=ncor+1
              icor(i)=iansw
           else
              icor(i)=0
           endif
 63      enddo
      endif
      return
      end
c ====================================================================
c CHEREQ
c ====================================================================
c checking the availability of the required data, for propagation
c and preciction of observations
c ====================================================================
      subroutine chereq(icov,ini,cov,t,iun20,iun8,ok)
      implicit none
      integer icov
      logical ini,cov
      integer iun20,iun8
      double precision t
      logical ok
c ===================================================================
c logical check: initial conditions available
c ===================================================================
      if(ini)then
         write(iun20,222) t
         ok=.true.
      else
         write(0,*)' initial conditions not available'
         ok=.false.
      endif
c ===================================================================
c logical check: if covariance has to be propagated,
c it needs to be available for the initial time
c ===================================================================
      if(icov.gt.1)then
         if(.not.cov)then
            write(0,*)' initial covariance not available'
            ok=.false.
         elseif(iun8.gt.0)THEN
            write(iun8,*)
            write(iun8,222)t
         endif 
      endif
  222 format('  Current epoch value (MJD): ',f8.1)
      return
      end
c ====================================================================
c CHEOBS
c ====================================================================
c check availability of observations and initial condition
c ===================================================================
      subroutine cheobs(obs,ini,ok)
      logical obs,ini,ok
      if(ini)then
         if(obs)then
            ok=.true. 
         else
            write(0,*)'missing observations for this arc'
            ok=.false.
         endif
      else
         write(0,*)'missing initial conditions for this arc'
         ok=.false.
      endif
      return
      end
c ====================================================
c CHETIM check availability of required time-dependent data
c     input: t1,t2 dates MJD; assumed t1.le.t2
c     ok: .true. if available
c ====================================================
      subroutine chetim(t1,t2,ok)
      implicit none
      double precision t1,t2
      logical ok
c ======== time spans for JPL data etc. =========
      include 'timespan.h'
      ok=.true.
c ==================================================================
c check availability of JPL ephemerides
      if(t1.lt.tejpl1.or.t2.gt.tejpl2)then
         write(0,*)' JPL epehemerides not available for =',t1,t2
         write(0,*)' but only for interval ',tejpl1,tejpl2
         ok=.false.
      endif
c ===================================================================
c check availability of ET-UT table
      if(t1.lt.temut1.or.t2.gt.temut2)then
         write(0,*)' ET-UT not available for ',t1,t2
         write(0,*)' but only for interval ',temut1,temut2
         if(temute) then
             write(0,*)' however, extrapolation will be used'
         else
             ok=.false.
         endif
      endif
      return
      end
c ===================================================
c SELEPH
c select time interval, step
      SUBROUTINE seleph(tut1,tdt1,tut2,tdt2,dt,idsta)
      IMPLICIT NONE
c output
      DOUBLE PRECISION  tut1,tdt1,tut2,tdt2,dt     
      INTEGER idsta
c times in various formats used internally
      CHARACTER*3 scale
      INTEGER mjd,mjdtdt
      DOUBLE PRECISION sec,sectdt
      WRITE(0,*)' Initial time (MJD UTC)?'
      READ(*,*)tut1
      WRITE(0,*)' Final time (MJD UTC)?'
      READ(*,*)tut2
      WRITE(0,*)' Time interval (days)?'
      READ(*,*)dt
      WRITE(0,*)' Observatory code?'
      READ(*,*)idsta
      scale='UTC'
c =========== TIME CONVERSION ================
c starting time
      mjd=tut1
      sec=(tut1-mjd)*86400
      call cnvtim(mjd,sec,scale,mjdtdt,sectdt,'TDT')
      tdt1=sectdt/86400.d0+float(mjdtdt)
c stopping time
      mjd=tut2
      sec=(tut2-mjd)*86400
      call cnvtim(mjd,sec,scale,mjdtdt,sectdt,'TDT')
      tdt2=sectdt/86400.d0+float(mjdtdt)
      RETURN
      END

c ====================================================
c ASSTIM assign time for predictions
c operations controlled by flag icov
c  ICOV=1,2,3  ask the user to assign prediciton time t1
c  ICOV=4      ask the user to assign observation number im
c                      then t1=tau(im)
c  for ICOV.ne.4 also station code ids and obs. type iob1
c        have to be assigned by the user
c ====================================================
      subroutine asstim(icov,iobs,tau,tut,idsta,m,mall,im,
     +       iob1,t1,tut1,ids)
      IMPLICIT NONE
c ==============input=============
      INTEGER icov,m,mall,mp,idsta(mall),iobs(mall)
      DOUBLE PRECISION tau(mall),tut(mall)
c ==============output=============
      INTEGER im,iob1,ids
      DOUBLE PRECISION t1,tut1
c time conversion
      INTEGER mjd1,mjd2,intlo
      DOUBLE PRECISION sec1,sec2
c ===================================================================
      if(mall.lt.m)then
         write(0,*)'asstim: this should not happen, m,mall ',m,mall
         mp=0
      else
         mp=mall-m
      endif
c assign observation time
      if(icov.eq.4)then
c compare confidence boundary with observations
 184     write(0,*)' observed arcs: from, to, no. obs'
         write(0,182)tau(1),tau(m),m
 182     format('arc 1: ',2f8.1,i6)
         IF(mall.gt.m)THEN
            write(0,183)tau(m+1),tau(mall),mp
 183        format('arc 2: ',2f8.1,i6)
         ENDIF
         write(0,*)' observation number?   '
         read(*,*)im
         if(im.lt.0.or.im.gt.mall)then
            write(0,*)' observation no. im=',im,' not available'
            goto 184
         endif
         t1=tau(im)
         tut1=tut(im)
         ids=idsta(im)
         iob1=iobs(im)
      else
c dummy obs. number (not used, to avoid out of bounds)
         im=1
c assign arbitrary time
         write(0,*)' give time of prediction (MJD)   '
         read(*,*)t1
c universal time of the required observation 
         mjd1=intlo(t1)
         sec1=(t1-float(mjd1))*86400.d0
         CALL cnvtim(mjd1,sec1,'TDT',mjd2,sec2,'UTC')
         tut1=sec2/86400.d0+float(mjd2)
c         write(0,*)t1,tut1
c assign observation type
c185     write(0,*)' observation type 1=RA,DEC  9=proper motion?'
 185     write(0,*)' observation type 1=RA,DEC 2=radar 4=proper motion?'
         read(*,*)iob1
         IF(iob1.ne.1.and.iob1.ne.2.and.iob1.ne.4)GOTO 185
         iob1=iob1*1000            
c assign observatory code
         write(0,*)
         write(0,*)' observatory code (geocenter=500)?   '
         read(*,*) ids
c secret nationalistic feature
         if(ids.lt.0)then
            ids=599
         endif
      endif
      return
      end

c ===================================================================
c  ASSCBD
c ===================================================================
c alpha, delta, magnitude, covariance and confidence boundary;
c input specification of set of points
c ===================================================================
      SUBROUTINE asscbd(iun20,npox,npo,sigma,ibv)
      IMPLICIT NONE
      INTEGER npox,npo,ibv,iun20
      DOUBLE PRECISION sigma
      WRITE(0,*)' How many sigmas?   '
      READ(*,*) sigma
 1    WRITE(0,*)' 1=confidence boundary 2=line of max variation 0=auto'
      READ(*,*) ibv
      IF(ibv.ne.1.and.ibv.ne.2.and.ibv.ne.0)THEN
         WRITE(0,*)' option not understood ',ibv
         GOTO 1
      ENDIF
      WRITE(0,*)' how many points (even, please)?   '
      READ(*,*) npo
      IF(npo+2.gt.npox)THEN
         WRITE(0,*)' npo=',npo,' too large for npox=',npox
         npo=npox-2
         WRITE(0,*)' used npox-2'
      ENDIF
      WRITE(iun20,*)'no. points ',npo     
      RETURN
      END
