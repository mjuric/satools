* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 16, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D B U L A                           *
*  *                                                               *
*  *            Read predictions of EOP time series                *
*  *                   from IERS Bulletin A                        *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  Input file name
*           MJD2      -  Time (MJD,UTC) of last point read
*           TIERS     -  Time of data points (MJD, TDT)
*           XIERS     -  Value of data points:
*                            xiers(1) = X pole (arcsec)
*                            xiers(2) = Y pole (arcsec)
*                            xiers(3) = TDT-UT1 (s)
*                            xiers(4) = Dpsi (arcsec)
*                            xiers(5) = Depsilon (arcsec)
*           NIERS     -  No. of data points already contained in arrays
*           NIERSX    -  Physical DIMENSION of arrays
*           ISAMP     -  Undersampling factor
*           NPT       -  Number of data points read so far
*           IUTSMO    -  Smoothing used for UT1 (xiers(i,3)) :
*                            0 = none
*                            1 = UT1R
*                            2 = UT1S
*
* OUTPUT:   MJD2      -  Time (MJD,UTC) of last point read
*           TIERS     -  (updated by adding values from Bulletin A)
*           XIERS     -  (updated by adding values from Bulletin A)
*           NIERS     -  (updated by adding values from Bulletin A)
*
      SUBROUTINE rdbula(file,mjd2,tiers,xiers,niers,niersx,isamp,npt,
     +                  iutsmo)
      IMPLICIT NONE

      INTEGER mjd2,niers,niersx,isamp,npt,iutsmo
      DOUBLE PRECISION tiers(niersx),xiers(niersx,5)
      CHARACTER*(*) file

      INCLUDE 'etmtai.h'

      INTEGER unit,lf,nr,n,year,month,day,mjd,mjdc,mjdp,mjd1,mjdf,skip,i
      DOUBLE PRECISION x,y,dt1,dtsec,dt,tjme,du,dud,dudd,dtsf
      CHARACTER*100 rec
      LOGICAL fill,strrec

      INTEGER lench,itaiut
      DOUBLE PRECISION tjm1
      LOGICAL isbadr
      EXTERNAL lench,itaiut,tjm1,isbadr

      lf=lench(file)
      CALL filopl(unit,file)
      nr=0
      n=0

* First scan of Bulletin A file: understand at which record data start
      skip=0
      strrec=.false.
 1    CONTINUE
      READ(unit,100,END=20) rec
 100  FORMAT(A)
      nr=nr+1
      IF(isbadr(rec)) THEN
          IF(strrec) THEN
              IF(nr-skip.GE.15) GOTO 2
          ELSE
              skip=nr-1
              strrec=.true.
          END IF
      ELSE
          skip=0
          strrec=.false.
      END IF
      GOTO 1

 2    CONTINUE
      IF(.NOT.strrec) GOTO 20

* Skip non-data records
      REWIND(unit)
      DO 3 i=1,skip
      READ(unit,100,END=20) rec
 3    CONTINUE
      nr=skip

 4    CONTINUE
      nr=nr+1
      READ(unit,100,END=10,ERR=30) rec
      IF(.NOT.isbadr(rec)) GOTO 10
      READ(rec,*,ERR=30) year,month,day,mjd,x,y,dt1
      fill=.false.

* Check on MJD value
      mjdc=NINT(tjm1(day,month,year,0.D0))
      IF(mjdc.NE.mjd) THEN
          WRITE(0,203) mjdc,mjd
          GOTO 30
      END IF
 203  FORMAT('ERROR: inconsistent MJD:',2I8)

* Check continuity with EOPC04 data
      IF(n.EQ.0) THEN
          mjd1=mjd
* Why IERS issues predictions starting 2 days later the last record
* present in their EOPC04 file? If it's so, here is the fix
          IF(mjd.EQ.mjd2+2) THEN
              fill=.true.
          ELSEIF(mjd.NE.mjd2+1) THEN
              WRITE(0,204) file(1:lf),mjd,mjd2
          END IF
      ELSE
* Check time consistency
          IF(mjd-mjdp.NE.1) THEN
              WRITE(0,205) mjd,mjdp
              GOTO 30
          END IF
      END IF
 204  FORMAT('ERROR: beginning date of file "',A,'" (MJD=',I5,
     +       ') is not consistent'/
     +       '       with ending date of EOPC04 data (MJD=',I5,')'/
     +       '       Try to get updated versions of IERS files')
 205  FORMAT('WRONG time sequence:',2I7)

      mjdp=mjd
* TDT-UT1 = (TDT-TAI) + (TAI-UTC) + (UTC-UT1)
      dtsec=etmtai+itaiut(mjd)
      dt=dtsec-dt1
* Transformation TJM(UTC) -> TJM(TDT)
* TDT ( = ET ) = (ET-TAI) + (TAI-UTC) + UTC
      tjme=mjd+dtsec/86400.d0
* Applying required smoothing to TDT-UT1
      IF(iutsmo.eq.0) THEN
          CONTINUE
      ELSEIF(iutsmo.EQ.1) THEN
          CALL dut1r(tjme,du,dud,dudd,0)
          dt=dt+du
      ELSEIF(iutsmo.EQ.2) THEN
          CALL dut1s(tjme,du,dud,dudd,0)
          dt=dt+du
      ELSE
          STOP '**** rdbula: internal error (01) ****'
      END IF
* Interpolation of missing record
      IF(fill) THEN
          npt=npt+1
          IF(MOD(npt,isamp).EQ.0) THEN
              niers=niers+1
              IF(niers.GT.niersx)
     +            STOP '**** rdbula: niers > niersx ****'
              mjdf=mjd2+1
              dtsf=etmtai+itaiut(mjdf)
              tiers(niers)=mjdf+dtsf/86400.d0
              xiers(niers,1)=(x+xiers(niers-1,1))/2
              xiers(niers,2)=(y+xiers(niers-1,2))/2
              xiers(niers,3)=(dt+xiers(niers-1,3))/2
              xiers(niers,4)=0.D0
              xiers(niers,5)=0.D0
          END IF
      END IF
      npt=npt+1
      IF(MOD(npt,isamp).NE.0) GOTO 4
* Storing values in arrays
      niers=niers+1
      n=n+1
      IF(niers.GT.niersx) STOP '**** rdbula: niers > niersx ****'
      mjd2=mjd
      tiers(niers)=tjme
      xiers(niers,1)=x
      xiers(niers,2)=y
      xiers(niers,3)=dt
      xiers(niers,4)=0.D0
      xiers(niers,5)=0.D0
      GOTO 4

 10   CONTINUE
      WRITE(0,206) n,mjd1,mjd2
 206  FORMAT('    Bulletin A data:',I6,' points (from MJD=',
     +       I5,' to MJD=',I5,')')
      CALL filclo(unit,' ')

      RETURN

 20   CONTINUE
      WRITE(0,200) file(1:lf)
 200  FORMAT('ERROR: sorry, unexpected format of file "',A,'"'/
     +       '       try running the program with ',
     +       '"IERS.eopc04.bulA.use = .false."')
      STOP '**** rdbula: abnormal end ****'

 30   CONTINUE
      WRITE(0,201) file(1:lf),nr
 201  FORMAT('ERROR in reading file "',A,'" at record ',I5)

      END
