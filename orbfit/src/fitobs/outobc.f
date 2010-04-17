c =====================================================================
c OUTOBC
c =====================================================================
c  output of predicted observation, possibly with confidence ellipse
c   input: iun   = output unit
c          iobs  = observation type
c          ids = station code
c          t1 = time of observation (UTC)
c          alpha, delta, hmagn = observation
c          adot,ddot = proper motion
c          elo,dis = elongation, distance from Earth
c
c          icov  = 1 for observations only, 2 to add confidence ellipse
c          gamad,sig,axes = covariance matrix, sigmas along axes 
c                      (only for icov=2, otherwise dummy)
c =====================================================================
      subroutine outobc(iun,iobs,ids,t1,alpha,delta,hmagn,adot,ddot,
     +     elo,dis,icov,gamad,sig,axes)
      implicit none
      include 'trig.h'
c needs AU value in km
      INCLUDE 'jplhdr.h'
c output unit, station code, obs. type
      integer iun,ids,iobs
c observations
      double precision t1,alpha,delta,hmagn,adot,ddot,elo,dis
c covariance
      integer icov
      double precision gamad(2,2),axes(2,2),sig(2)
c ================end interface===============================
      double precision princ  
      integer i,j
c time variables
      integer ideg,iday,imonth,iyear,ihour,imin,isec,ln,truncat
      double precision hour,minu,sec
      CHARACTER*22 timstr
      CHARACTER*19 tmpstr
      CHARACTER*12 rastri,rdstri
      CHARACTER*1 signo
c convert time
      CALL mjddat(t1,iday,imonth,iyear,hour)
c convert hour to 12:12:12
      ihour=truncat(hour,1d-7)
      minu=(hour-ihour)*60.d0
      imin=truncat(minu,1d-5)
      sec=(minu-imin)*60.d0
      isec=truncat(sec,1d-3)
      WRITE(timstr,192) iyear,imonth,iday,ihour,imin,isec,sec-isec
 192  FORMAT(I4,'/',I2.2,'/',I2.2,1x,I2.2,':',I2.2,':',I2.2,f3.2)
c =================== select by observation type ===================
      IF(iobs/1000.eq.1)THEN
c %%%%%%%%%%%% ASTROMETRY %%%%%%%%%%%%%%%%
c convert RA
         alpha=princ(alpha)
         CALL sessag(alpha*degrad/15.d0,signo,ihour,imin,sec)
         IF(signo.eq.'-')STOP 'wrirms error: negative right ascension.'
c prepare RA string
         WRITE(tmpstr,FMT='(F6.3)') sec
         CALL rmsp(tmpstr,ln)
         IF(ln.lt.6)tmpstr='0'//tmpstr
         WRITE(rastri,130)ihour,imin,tmpstr
 130     FORMAT(I2.2,':',I2.2,':',a6)
c convert DEC
         CALL sessag(delta*degrad,signo,ideg,imin,sec)
c prepare DEC string
         WRITE(tmpstr,FMT='(F5.2)') sec
         CALL rmsp(tmpstr,ln)
         IF(ln.lt.5)tmpstr='0'//tmpstr
         WRITE(rdstri,170)signo,ideg,imin,tmpstr
 170     FORMAT(A1,I2.2,1x,I2.2,1x,a5)

         write(iun,101)timstr,t1,ids,
     +        rastri,alpha*degrad,
     +        rdstri,delta*degrad,
     +        secrad*adot/24.d0,secrad*ddot/24.d0,
     +        dis,elo*degrad,hmagn
         write(99,101)timstr,t1,ids,
     +        rastri,alpha*degrad,
     +        rdstri,delta*degrad,
     +        secrad*adot/24.d0,secrad*ddot/24.d0,
     +        dis,elo*degrad,hmagn
 101     format('Astrometric Observation Prediction'/
     +        'For ',a19,' (UTC); ',f12.5,'(MJD)'/
     +        'Observatory code= ',i3.3/
     +        'RA= ',a12,' (HH:MM:SS); ',f11.5,' (deg)'/
     +        'DEC= ',a12,' (deg min sec); ',f11.5,' (deg)'/
     +        'RA/DEC Apparent motion=',2(2x,f9.2),' (arcsec/hour)'/
     +        'Earth distance= ',f6.4,' (AU)'/
     +        'Solar elongation= ',f6.2,' (deg)'/
     +        'Apparent magnitude= ',f5.2)
         IF(icov.eq.1)RETURN
c rescaling in arcsec
         do  i=1,2
            sig(i)=sig(i)*secrad
            do  j=1,2
               gamad(i,j)=gamad(i,j)*secrad**2
            enddo
         enddo
         write(iun,201)(sig(j),(axes(i,j),i=1,2),j=1,2)
         write(99,201)(sig(j),(axes(i,j),i=1,2),j=1,2)
 201     format(
     +'Size and orientation of 1-sigma uncertainty ellipse'/
     +'Short axis : Size= ',1p,g12.6 ,' (arcsec); Direction= ',
     + 0p,2(1x,f8.5)/
     +'Long axis : Size= ',1p,g12.6 ,' (arcsec); Direction= ',
     + 0p,2(1x,f8.5))
      ELSEIF(iobs/1000.eq.2)THEN
c %%%%%%%%%%%% RADAR %%%%%%%%%%%%%%%%
         write(iun,102)t1,ids,alpha*au,delta*au
         write(99,102)t1,ids,alpha*au,delta*au
 102     format('time, MJD=',f13.6,'  station=',i4/
     +       ' range (KM)         = ',f16.5/
     +       ' range rate (KM/DAY)=  ',f15.5)
         IF(icov.eq.1)RETURN
c rescaling in km, km/day
         do  i=1,2
            sig(i)=sig(i)*au
            do  j=1,2
               gamad(i,j)=gamad(i,j)*au**2
            enddo
         enddo
         write(iun,202)(sig(j),(axes(i,j),i=1,2),j=1,2)
         write(99,202)(sig(j),(axes(i,j),i=1,2),j=1,2)
 202     format(' in the range (KM), range-rate (KM/DAY) plane'/
     +          ' sigma1 = ',1p,g14.7 ,' axis1= ',2(1x,g12.5)/
     +          ' sigma2 = ',1p,g14.7 ,' axis2= ',2(1x,g12.5))
      ELSEIF(iobs/1000.eq.4)THEN
c %%%%%%%%%%%% PROPER MOTION %%%%%%%%%%%%%%%%
         write(iun,109)t1,ids,alpha*secrad/24.d0,delta*secrad/24.d0
         write(99,109)t1,ids,alpha*secrad/24.d0,delta*secrad/24.d0
 109     format('time, MJD=',f13.6,'  station=',i4/
     +       ' RA motion (arcsec/hour)     = ',f9.2/
     +       ' DEC motion (arcsec/hour)    = ',f9.2)
         IF(icov.eq.1)RETURN
c rescaling in arcsec/hour
         do  i=1,2
            sig(i)=sig(i)*secrad/24.d0
            do  j=1,2
               gamad(i,j)=gamad(i,j)*(secrad/24.d0)**2
            enddo
         enddo
         write(iun,209)(sig(j),(axes(i,j),i=1,2),j=1,2)
         write(99,209)(sig(j),(axes(i,j),i=1,2),j=1,2)
 209     format('sigma1 (arcsec/hr)= ',1p,g14.7 ,' axis1= ',2(1x,g12.5)/
     +          'sigma2 (arcsec/hr)= ',1p,g14.7 ,' axis2= ',2(1x,g12.5))
      ELSE
         write(99,*)'outobs: iobs=',iobs,' not understood'
      ENDIF
      return
      end








