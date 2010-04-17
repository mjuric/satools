* Copyright (C) 1998 by Steven Chesley (chesley@dm.unipi.it)
* Version: Dec. 15, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R I R M S                           *
*  *                                                               *
*  *       Writes a-priori standard deviation of observations      *
*  *                       and NOT fit residuals                   *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  Output file name
*           OBJID     -  IAU Identifier for each observation
*           IOBS      -  Observation type 
*           TUTM      -  Time (MJD, UTM)
*           OBSCOD    -  Observatory code
*           ALPHA     -  Right Ascension (radians)
*           RMSA      -  A-priori RMS of right ascension (rad)
*           DELTA     -  Declination (radians)
*           RMSD      -  A-priori RMS of declination (rad)
*           SMAG      -  Magnitude observations (string)
*           RMSMAG       A-priori RMS of magnitude
*           SEL       -  Selection indicator (0=don't use; 1=use for fit;
*                              2=use for fit & Gauss method)
*           N         -  Number of observations
*
      SUBROUTINE wrirms(file,objid,iobs,tutm,obscod,alpha,rmsa,
     +     delta,rmsd,smag,rmsmag,sel,n)
      IMPLICIT NONE

      CHARACTER*(*) file
      INTEGER n,obscod(n),sel(n),iobs(n)
      CHARACTER*(*) objid(n)
      DOUBLE PRECISION tutm(n),alpha(n),delta(n),rmsa(n),rmsd(n)
c magnitudes (string), a priori rms'
      CHARACTER*6 smag(n)
      DOUBLE PRECISION rmsmag(n)
c =================END INTERFACE=============================
      INCLUDE 'trig.h'
      INCLUDE 'parcmc.h'
      INCLUDE 'jplhdr.h'
      INCLUDE 'parobx.h'

      INTEGER unit,i,is(nobx),ln
      INTEGER iyear,imonth,iday,ihour,imin,isec,ideg
      DOUBLE PRECISION day,hour,minu,sec
      CHARACTER*19 magstri,tmpstr
      CHARACTER*30 rastri,rdstri
      CHARACTER*37 rstri
      CHARACTER*3 radtyp
      CHARACTER*1 obstr,signo
      LOGICAL radar
      INTEGER truncat,iobcur,iotr,iore

      CALL filopn(unit,file,'UNKNOWN')

c check data set first
      radar=.false.
      DO i=1,n
         IF(iobs(i)/1000.eq.2)THEN
            radar=.true.
         ELSEIF(iobs(i)/1000.ne.1)THEN
            write(99,*)'wrirms: obs.type ',iobs(i),' unknown, rec.no=',i
            STOP
         ENDIF
      ENDDO
c ========= SORT OBSERVATIONS BY TIME ==============
      call sortob(tutm,is,n)
c ========= HANDLE ASTROMETRY OBSERVATIONS =========
      WRITE(unit,120) comcha
 120  FORMAT(A1,'++ OBJ ++ OBS +++++ DATE ++++++',
     +     '  +++++ RIGHT ASCENSION ++++++',
     +     '  ++++++ DECLINATION ++++++++',
     +     '  +++++ APP MAG +++++',
     +     '  ++++ QUALITY +++')
      WRITE(unit,221) comcha
 221  FORMAT(A1,'+ DESIG + TYP YYYY MM DD.dddddd',
     +     '  HH MM SS.sss   rms     resid',
     +     '  DD MM SS.ss   rms     resid',
     +     '  MAG COL rms   resid',
     +     '  OBS     CHI  SEL')
c ========= ASTROMETRY LOOP==========================
      DO   i=1,n
      IF(iobs(is(i))/1000.eq.1)THEN
         obstr=char(iobs(is(i))-1000)
c convert time
         CALL mjddat(tutm(is(i)),iday,imonth,iyear,hour)
         day=iday+hour/24.d0
******************
c convert RA
         CALL sessag(alpha(is(i))*degrad/15.d0,signo,ihour,imin,sec)
         IF(signo.eq.'-')STOP 'wrirms error: negative right ascension.'
c prepare RA string
         WRITE(tmpstr,FMT='(F6.3)') sec
         CALL rmsp(tmpstr,ln)
         IF(ln.lt.6)tmpstr='0'//tmpstr
         WRITE(rastri,130)ihour,imin,tmpstr,rmsa(is(i))*secrad
 130     FORMAT(2x,I2.2,1x,I2.2,1x,a6,F7.2,9x)
c convert DEC
         CALL sessag(delta(is(i))*degrad,signo,ideg,imin,sec)
c prepare DEC string
         WRITE(tmpstr,FMT='(F5.2)') sec
         CALL rmsp(tmpstr,ln)
         IF(ln.lt.5)tmpstr='0'//tmpstr
         WRITE(rdstri,170)signo,ideg,imin,tmpstr,rmsd(is(i))*secrad
 170     FORMAT(1x,A1,I2.2,1x,I2.2,1x,a5,F7.2,9x)
c prepare MAG string
         IF(rmsmag(is(i)).lt.0)THEN
            magstri=smag(is(i))//'            '
         ELSE
            WRITE(magstri,121)smag(is(i)),rmsmag(is(i))
 121        FORMAT(a6,1x,f5.2,1x,6x)
         ENDIF
c output  astrometry             
         WRITE(tmpstr,FMT='(F9.6)') day
         CALL rmsp(tmpstr,ln)
         IF(ln.lt.9)tmpstr='0'//tmpstr
         WRITE(unit,101) objid(is(i)),obstr,iyear,imonth,tmpstr,
     +        rastri,rdstri,magstri,
     +        obscod(is(i)),sel(is(i))
 101     FORMAT(1x,A9,2x,a1,2x,I4,1x,I2.2,1x,A9,
     +        A30,A29,2x,A19,
     +        2X,I3.3,9x,2X,I1,3x)
      ENDIF
      ENDDO
c ========= HANDLE RADAR OBSERVATIONS ==========
      If(.not.radar) GOTO 99
c radar header      
      WRITE(unit,128) comcha
 128  FORMAT(A1,'++ OBJ ++ OBS ++++++ DATE +++++++ ',
     +     '++++++++ RADAR RANGE/RANGE RATE +++++++++ ',
     +     '++++++ QUALITY +++++')
      WRITE(unit,228) comcha
 228  FORMAT(A1,'+ DESIG + TYP YYYY MM DD hh:mm:ss ',
     +     'TYP   KM or KM/DAY  a priori rms residual ',
     +     'TRX REC     CHI  SEL')
c ========= RADAR LOOP==========================
      DO i=1,n
         IF(iobs(is(i))/1000.eq.2)THEN
            IF(iobs(is(i))-2000.ge.100)THEN
c surface bounce
               iobcur=iobs(is(i))-2100
               obstr='r'
            ELSE
c already corrected to center of mass
               iobcur=iobs(is(i))-2000
               obstr='R'
            ENDIF
c convert time
            CALL mjddat(tutm(is(i)),iday,imonth,iyear,hour)
c convert hour to 12:12:12
            ihour=truncat(hour,1d-7)
            minu=(hour-ihour)*60.d0
            imin=truncat(minu,1d-5)
            sec=(minu-imin)*60.d0
            isec=truncat(sec,1d-3)
            IF(iobcur.eq.2)THEN
c range observation
               radtyp='DEL'               
               WRITE(rstri,141)alpha(is(i))*au,rmsa(is(i))*au
 141           FORMAT(f16.5,1x,f9.5,1x,10x)
            ELSEIF(iobcur.eq.3)THEN
c range-rate observation
               radtyp='DOP'               
               WRITE(rstri,141)delta(is(i))*au,rmsd(is(i))*au
            ELSE
               STOP '*** wrirms.f: internal error(1) ***'
            ENDIF
c find codes of two observatories
            iotr= obscod(is(i))/10000
            iore= obscod(is(i))-iotr*10000
c output radar data              
            WRITE(unit,102) objid(is(i)),obstr,iyear,imonth,iday,
     +           ihour,imin,isec,radtyp,rstri,
     +           iotr,iore,sel(is(i))
 102        FORMAT(1x,A9,2x,a1,2x,I4,1x,I2.2,1x,I2.2,1x,
     +           I2.2,':',I2.2,':',I2.2,1x,A3,1x,A37,
     +           1X,I3.3,1X,I3.3,9x,2X,I1,3x)
         ENDIF
      ENDDO
c ============================================
 99   CALL filclo(unit,' ')
      END
