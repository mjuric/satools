* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 8, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          G M S T D                            *
*  *                                                               *
*  *                 Greenwich Mean Sidereal Time                  *
*  *          as a function of UT1 (with time derivatives)         *
*  *                                                               *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    MJDU      -  Modified Julian Time (UT1): integer part
*           SECU      -  Modified Julian Time (UT1): seconds within
*                        the day (0 <= SECU < 86400)
*           NDER      -  Required order for derivatives (0/1/2)
*
* OUTPUT:   GMST      -  Greenwich Mean Sidereal Time referred
*                        to the mean equinox of date (rad)
*           GMSTD1    -  dGMST/dUT1 (rad/s) (only if NDER >= 1)
*           GMSTD2    -  d^2GMST/dUT1^2 (rad/s^2) (only if NDER = 2)
*
      SUBROUTINE gmstd(mjdu,secu,gmst,gmstd1,gmstd2,nder)
      IMPLICIT NONE

      INTEGER mjdu,nder
      DOUBLE PRECISION secu,gmst,gmstd1,gmstd2

      INCLUDE 'trig.h'

      DOUBLE PRECISION s2r,s2c,s2c2,a,b,c,d,fday,tu,gmst0
      DOUBLE PRECISION ssum,dsum,ddsum,c0,c1,c2,c3,t
      LOGICAL first
      SAVE first,s2r,s2c,s2c2,a,b,c,d
      DATA first/.true./

* Computation of a 3rd degree polynomial with derivatives
      ssum  (c0,c1,c2,c3,t) = ((c3*t+c2)*t+c1)*t+c0
      dsum (c0,c1,c2,c3,t) = (3.d0*c3*t+2.d0*c2)*t+c1
      ddsum(c0,c1,c2,c3,t) = 6.d0*c3*t+2.d0*c2

      IF(nder.LT.0.OR.nder.GT.2) STOP '**** gmstd: nder = ? ****'

      IF(first) THEN
          first=.false.
          s2r = pig/(12*3600)
          s2c = 1.d0/(36525.d0*86400.d0)
          s2c2= s2c**2
* Polynomial representation of GMST (see Supplement to the
* Astronomical Almanac, 1984, S15)
          a  =    24110.54841d0  * s2r
          b  =  8640184.812866d0 * s2r
          c  =        0.093104d0 * s2r
          d  =       -6.2d-6     * s2r
      END IF

* Fraction of the day elapsed from 0h UT1 to SECU
      fday=secu/86400.d0

* Number of centuries elapsed from 2000 January 1, 12h UT1
* to 0h UT1 of the current day (TJM=MJDU)
      tu=(mjdu-51544.5d0)/36525.d0

* GMST at 0h UT1 (TJM=MJDU)
      gmst0=ssum(a,b,c,d,tu)

* dGMST/dUT1 is computed at the midpoint between MJDU and MJDU+FDAY
      tu=(mjdu+fday/2.d0-51544.5d0)/36525.d0
      gmstd1=dsum(a,b,c,d,tu)*s2c+dpig/86400.d0

* GMST at TJM=MJDU+FDAY
      gmst=gmst0+gmstd1*secu
      gmst=MOD(gmst,dpig)
      IF(gmst.LT.0.d0) gmst=gmst+dpig

* Derivatives are computed again at TJM=MJDU+FDAY
* only when requested
      tu=(mjdu+fday-51544.5d0)/36525.d0
      IF(nder.GE.1) gmstd1=dsum(a,b,c,d,tu)*s2c+dpig/86400.d0
      IF(nder.GE.2) gmstd2=ddsum(a,b,c,d,tu)*s2c2

      END
