* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 11, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C H 2 T I M                           *
*  *                                                               *
*  *     Translation of a character string into time value         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    STRING    -  Time string
*
* OUTPUT:   MJD       -  Modified Julian Date (integer part)
*           SEC       -  Seconds within the day
*           SCALE     -  Time scale (UTC/UT1/TAI/TDT/ET/GPS)
*           ERROR     -  Error flag
*
      SUBROUTINE ch2tim(string,mjd,sec,scale,error)
      IMPLICIT NONE

      CHARACTER*(*) string,scale
      INTEGER mjd
      DOUBLE PRECISION sec
      LOGICAL error

      CHARACTER*80 c,c1,rest1,date,tmp,hhmmss
      CHARACTER*10 format
      INTEGER l,posb,intval,year,lt,month,day,hh,mm
      DOUBLE PRECISION secval,reaval,ss
      LOGICAL tmperr

      INTEGER lench,intmon
      LOGICAL isnum,islett
      DOUBLE PRECISION tjm1
      EXTERNAL lench,intmon,isnum,islett,tjm1

      error=.true.
      IF(lench(string).GT.80) RETURN
      c=string
      CALL norstr(c,l)
      posb=INDEX(c,' ')
      IF(posb.LE.1) RETURN

* Time format
      CALL strcnt(c,format,rest1,tmperr)
      IF(tmperr) RETURN
      CALL upcase(format)
      c=rest1
      posb=INDEX(c,' ')

* Time value
      IF(format.EQ.'JD' .OR. format.EQ.'MJD') THEN
* Format A
          IF(INDEX(c(1:posb),'.').EQ.0) THEN
              READ(c(1:posb-1),*,ERR=1) intval
              c1=c(posb+1:)
              c=c1
              posb=INDEX(c,' ')
              IF(posb.LE.0)
     +            STOP '**** ch2tim: internal error (01) ****'
              IF(posb.EQ.1) RETURN
              READ(c(1:posb-1),*,ERR=1) secval
              c1=c(posb+1:)
              c=c1
          ELSE
* Format B
              READ(c(1:posb-1),*,ERR=1) reaval
              c1=c(posb+1:)
              c=c1
              intval=reaval
              IF(intval.GT.reaval) intval=intval-1
              secval=(reaval-intval)*86400.d0
          END IF
      ELSEIF(format.EQ.'CAL') THEN
* Year
          date=c(1:posb-1)
          CALL stspli(date,'/',tmp,tmperr)
          IF(tmperr) RETURN
          READ(tmp,*,ERR=1) year
* Month
          CALL stspli(date,'/',tmp,tmperr)
          lt=lench(tmp)
          IF(isnum(tmp(1:lt))) THEN
              READ(tmp,*,ERR=1) month
          ELSEIF(islett(tmp(1:lt))) THEN
              month=intmon(tmp)
              IF(month.LE.0) GOTO 1
          ELSE
              GOTO 1
          END IF
*  Day
          READ(date,*,ERR=1) day
          mjd=NINT(tjm1(day,month,year,0.d0))
          c1=c(posb+1:)
          c=c1
          posb=INDEX(c,' ')
          IF(posb.EQ.1) GOTO 1
* hh:mm:ss.sss
          hhmmss=c(1:posb-1)
          CALL stspli(hhmmss,':',tmp,tmperr)
          IF(tmperr) RETURN
          READ(tmp,*,ERR=1) hh
          CALL stspli(hhmmss,':',tmp,tmperr)
          IF(tmperr) RETURN
          READ(tmp,*,ERR=1) mm
          READ(hhmmss,*,ERR=1) ss
          sec=hh*3600+mm*60+ss
          c1=c(posb+1:)
          c=c1
      ELSE
          RETURN
      END IF

      IF(format.EQ.'MJD') THEN
          mjd=intval
          sec=secval
      ELSEIF(format.EQ.'JD') THEN
          mjd=intval-2400001
          sec=secval+43200.d0
      ELSEIF(format.EQ.'CAL') THEN
          CONTINUE
      ELSE
          STOP '**** ch2tim: internal error (02) ****'
      END IF

* Timescale
      CALL strcnt(c,scale,rest1,tmperr)
      IF(tmperr) RETURN
      IF(lench(rest1).GT.0) RETURN
      CALL chktsc(scale,tmperr)
      IF(tmperr) RETURN
      CALL timnf(mjd,sec,scale)
      error=.false.
      RETURN

* Error termination
 1    CONTINUE
      error=.true.

      END
