* Author: Mario Carpino (carpino@brera.mi.astro.it)
* Version: April 1, 1996
*
*  *****************************************************************
*  *                                                               *
*  *                          T I M N F                            *
*  *                                                               *
*  *              Reduction of time to normal form                 *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    MJD       -  Modified Julian Day (integer part)
*           SEC       -  Seconds within day
*           SCALE     -  Time scale
*
* OUTPUT:   MJD,SEC are normalized, namely SEC is reduced within
*           the limits 0 <= SEC < LOD, and MJD is changed accordingly;
*           LOD (length of the day) is usually 86400 s, but can be
*           different from that value in the case of UTC, due to
*           leap seconds
*
      SUBROUTINE timnf(mjd,sec,scale)
      IMPLICIT NONE
      INTEGER mjd
      DOUBLE PRECISION sec
      CHARACTER*3 scale

* Tuning parameters
      INTEGER nitmax,nitutc
      PARAMETER (nitmax=5)
      PARAMETER (nitutc=20)

      INTEGER mjd1,nit,idur,isec,k
      DOUBLE PRECISION sec1,fsec
      EXTERNAL itaiut
      INTEGER itaiut

* Input values are stored for error messages
      mjd1=mjd
      sec1=sec

* Non-trivial case: UTC (the duration of the day can be different
* from 86400 s)

      IF(scale.EQ.'UTC') THEN
          nit=0
 1        CONTINUE
          nit=nit+1
          IF(nit.GT.nitutc) THEN
              WRITE(0,100) mjd1,sec1,scale
              STOP' **** timnf: abnormal END ****'
          END IF
          IF(sec.LT.0.d0) THEN
* Duration in seconds of the previous day
              idur=86400+itaiut(mjd)-itaiut(mjd-1)
              sec=sec+idur
              mjd=mjd-1
              GOTO 1
          END IF
* Decomposition of SEC into integer part (ISEC) + fraction (FSEC),
* where 0 <= FSEC < 1
          isec=sec
          fsec=sec-isec
* Duration in seconds of today (MJD)
 2        idur=86400+itaiut(mjd+1)-itaiut(mjd)
* Renormalization of time
          IF(isec.GE.idur) THEN
              isec=isec-idur
              mjd=mjd+1
              GOTO 2
          END IF
          sec=isec+fsec

* Trivial case: the duration of the day is always 86400 s
* Also this case requires iterations, due to rounding-off problems.
* EXAMPLE: Let's suppose that the starting values are MJD=48000,
* SEC=-1.d-14. The result of the first iteration is then:
*    SEC --> SEC+86400 = 86400 EXACTLY (due to rounding off)
*    MJD --> MJD-1 = 47999
* Therefore, a second iteration is required, giving:
*    SEC --> SEC-86400 = 0
*    MJD --> MJD+1 = 48000

      ELSE
          nit=0
 3        nit=nit+1
          IF(nit.GT.nitmax) THEN
              WRITE(0,100) mjd1,sec1,scale
              STOP' **** timnf: abnormal END ****'
          END IF
          k=sec/86400.d0
          IF(sec.LT.0.d0) k=k-1
          IF(k.EQ.0) RETURN
          mjd=mjd+k
          sec=sec-k*86400.d0
          GOTO 3
      END IF

 100  FORMAT(' **** timnf: too many iterations ****'/
     .       i7,f20.12,1x,a)
      END
