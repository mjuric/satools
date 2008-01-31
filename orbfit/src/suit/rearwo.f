* Copyright (C) 1998 by Steven Chesley (chesley@dm.unipi.it)
* Version: Jan. 22, 1998
* Version: September 10, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R E A R W O                           *
*  *                                                               *
*  *                 Reads observations, apriori rms,              *
*  *                   and post fit redisuals                      *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  Input file name
*           NLEF      -  max dimension for arrays
* OUTPUT:
*           OBJID     -  IAU Identifier for each observation
*           IOBS      -  Observation type 1000+x astrometry 2000+x radar
*           TAU       -  Time(MJD, TDT)
*           TUTM      -  Time (MJD, UTM)
*           OBSCOD    -  Observatory code
*           ALPHA     -  Right Ascension (radians)
*           RMSA      -  A-priori RMS of right ascension (rad)
*           RESA      -  Residuals in right ascension (rad) (aka O-C)
*           DELTA     -  Declination (radians)
*           RMSD      -  A-priori RMS of declination (rad)
*           RESD      -  Residuals in declination (rad)
*           SMAG      -  Magnitude observations (string)
*           RMSMAG       A-priori RMS of magnitude
*           RESMAG    -  Residuals in magnitude
*           SEL       -  Selection indicator (0=don't use; 1=use for fit;
*                              2=use for fit & Gauss method)
*           CHI2      -  CHI**2 value for each observation
*           N         -  Number of observations
*
      SUBROUTINE rearwo(file,objid,iobs,tau,tutm,obscod,
     +     alpha,rmsa,resa,
     +     delta,rmsd,resd,
     +     smag,rmsmag,resmag,
     +     sel,chi2,n,nlef)
      IMPLICIT NONE
c input file, space left in arrays
      CHARACTER*(*) file
      INTEGER nlef
c number of obs, observatory, selection flag, obs. type, object identifier
      INTEGER n,obscod(nlef),sel(nlef),iobs(nlef)
      CHARACTER*(*) objid(nlef)
c time of observation      
      DOUBLE PRECISION tutm(nlef),tau(nlef)
c observations and residuals
      DOUBLE PRECISION alpha(nlef),delta(nlef),rmsa(nlef),rmsd(nlef)
      DOUBLE PRECISION resa(nlef),resd(nlef),chi2(nlef)
c magnitudes (string), a priori rms'
      CHARACTER*6 smag(nlef)
      DOUBLE PRECISION rmsmag(nlef)
c fit residuals, rms
      DOUBLE PRECISION resmag(nlef)
c =================END INTERFACE=============================
      INCLUDE 'trig.h'
      INCLUDE 'parcmc.h'
      INCLUDE 'jplhdr.h'
c ===========units for err,pro,clo files ========
      INCLUDE 'proout.h'

      INTEGER unit,i,iday,month,year,deg,mindec,hr,minra,isec,imin,ihour
      DOUBLE PRECISION day,secra,secdec

      CHARACTER*140 rec,tmprec
      CHARACTER*1 obstyp,sign
      CHARACTER*3 scale
      CHARACTER*37 rstri
      CHARACTER*3 radtyp
      CHARACTER*9 chistr
      DOUBLE PRECISION chi,sec,sect
      INTEGER mjd,mjdt,ll,iotr,iore

      DOUBLE PRECISION tjm1
      EXTERNAL tjm1

      CALL filopn(unit,file,'UNKNOWN')

c ========= PROCESS RECORDS SEQUENTIALLY ===========
      n=0
      DO 1  i=1,nlef+20
         READ(unit,100,END=99,ERR=10)rec
 100     FORMAT(A)
c skip comments
         tmprec=rec
         CALL rmsp(tmprec,ll)
         IF(tmprec(1:1).eq.comcha) GOTO 1
c otherwise get observation code
         obstyp=rec(13:13)
         IF(obstyp.ne.'R'.and.obstyp.ne.'r'.and.obstyp.ne.'S')THEN
            n=n+1
            IF(n.gt.nlef) STOP 'rearwo: nobs > nobx.'
            iobs(n)=1000+ichar(obstyp)
c Read astrometry observation
            READ(rec,101,ERR=10) objid(n),year,month,day,
     +           hr, minra, secra, rmsa(n),resa(n),
     +           sign,deg,mindec,secdec,rmsd(n),resd(n),
     +           smag(n),rmsmag(n),resmag(n),
     +           obscod(n),chistr,sel(n)
 101        FORMAT(1x,A9,5x,I4,I3,F10.6,
     +           2x,I2,1x,I2,F7.3,F7.2,G9.3,
     +           1x,A1,I2,1x,I2,F6.2,F7.2,G9.1,2x,
     +           a6,1x,f5.2,1x,f6.2,
     +           2X,I3.3,A9,2X,I1,3x)
            READ(chistr,FMT='(F9.2)',ERR=11)chi
            chi2(n)=chi**2
            GOTO 12
 11         WRITE(ierrou,*)'rearwo: error in chi ',chistr
            WRITE(*,*)'rearwo: error in chi ',chistr
            chi2(n)=0.d0
            numerr=numerr+1
 12         CONTINUE
            IF(rec(100:112).eq.'             ') rmsmag(n)=-1.d0
c            IF(rmsmag(n).eq.0.d0) rmsmag(n)=-1.d0
c convert time
            IF(year.LT.1972) THEN
               scale='UT1'
            ELSE
               scale='UTC'
            ENDIF
            iday=day
            sec=(day-iday)*86400.d0
            mjd=nint(tjm1(iday,month,year,0.d0))
            CALL cnvtim(mjd,sec,scale,mjdt,sect,'TDT')
            tutm(n)=mjd+sec/86400.d0
            tau(n)=mjdt+sect/86400.d0 !test
c convert DEC
            delta(n)=(deg*3600.d0+mindec*60.d0+secdec)/secrad
            IF(sign.eq.'-')delta(n)=-delta(n)
            rmsd(n)=rmsd(n)/secrad
            resd(n)=resd(n)/secrad
c convert RA (residuals conversion depends upon delta)
            alpha(n)=15.d0*(hr*3600.d0+minra*60.d0+secra)/secrad
            rmsa(n)=rmsa(n)/secrad
            resa(n)=resa(n)/cos(delta(n))/secrad
c Radar Observation
         ELSEIF(obstyp.eq.'R'.or.obstyp.eq.'r')THEN
            n=n+1
            IF(n.gt.nlef) STOP 'rearwo: nobs > nobx.'
c Read radar observation
            READ(rec,102,ERR=10)objid(n),year,month,iday,ihour,
     +           imin,isec,radtyp,rstri,
     +           iotr,iore,chi,sel(n)
 102        FORMAT(1x,A9,2x,1x,2x,I4,1x,I2,1x,I2,1x,I2,':',I2,':',I2,1x,
     +           A3,1x,A37,
     +           1X,I3.3,1X,I3.3,f9.2,2X,I1,3x)
            obscod(n)=iotr*10000+iore
            chi2(n)=chi**2
c           WRITE(*,102) objid(n),year,month,iday,ihour,imin,isec,
c    +           rstri,vstri,
c    +           iotr,iore,chi,sel(n)
c convert time
            sec=isec+60.d0*imin+3600.d0*ihour
            mjd=nint(tjm1(iday,month,year,0.d0))
            tutm(n)=mjd+sec/86400.d0
            IF(year.LT.1972) THEN
               scale='UT1'
            ELSE
               scale='UTC'
            ENDIF
            CALL cnvtim(mjd,sec,scale,mjdt,sect,'TDT')
            tau(n)=mjdt+sect/86400.d0
            IF(radtyp.eq.'DEL')THEN
c Prcess range string
               READ(rstri,141,ERR=10)alpha(n),rmsa(n),resa(n)
 141           FORMAT(f16.5,1x,f9.5,1x,f10.5)
               alpha(n)=alpha(n)/au
               rmsa(n)=rmsa(n)/au
               resa(n)=resa(n)/au
               iobs(n)=2002
               delta(n)=0.d0
               rmsd(n)=-1.d0
            ELSEIF(radtyp.eq.'DOP')THEN
c Prcess range rate string
               READ(rstri,141,ERR=10)delta(n),rmsd(n),resd(n)
               delta(n)=delta(n)/au
               rmsd(n)=rmsd(n)/au
               resd(n)=resd(n)/au
               iobs(n)=2003
               alpha(n)=0.d0
               rmsa(n)=-1.d0
            ELSE
               STOP'*** rearwo: internal error (1) ***'
            ENDIF
c photometry has no meaning
            smag(n)='      '
c surface bounce correction required
            IF(obstyp.eq.'r')iobs(n)=iobs(n)+100
         ELSE
c           Skip record if unknown type          
            WRITE(*,*) 'Unknown obs type ',obstyp,' at line ',
     +           i,' in ',file
         ENDIF
c spaghetti code: only go to 10 on error, else skip line 10.
         GOTO 1
 10      WRITE(ierrou,*) 'ERROR while reading line ',i,' of ',file
         WRITE(ierrou,*) 'skipping record: ',rec
         WRITE(*,*) 'ERROR while reading line ',i,' of ',file
         WRITE(*,*) 'skipping record: ',rec
         n=n-1
         numerr=numerr+1
 1    ENDDO
c ============================================
 99   CALL filclo(unit,' ')
      RETURN
      END
