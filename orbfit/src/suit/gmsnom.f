* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 8, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         G M S N O M                           *
*  *                                                               *
*  * Nominal (strictly linear with time) Greenwich Mean Sid. Time  *
*  *                     as a function of ET                       *
*  *                                                               *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    MJD       -  Modified Julian Time (ET): integer part
*           SEC       -  Modified Julian Time (ET): seconds within
*                        the day (0 <= SECU < 86400)
*
* OUTPUT:   GMST      -  Greenwich Mean Sidereal Time referred
*                        to the mean equinox of date (rad)
*           GMSTD1    -  dGMST/dUT1 (rad/s)
*
      SUBROUTINE gmsnom(mjd,sec,gmst,gmstd1)
      IMPLICIT NONE

      INTEGER mjd
      DOUBLE PRECISION sec,gmst,gmstd1

      INTEGER mjd0,icheck
      DOUBLE PRECISION gms0,gms1,sec0
      COMMON/cmngms/gms0,gms1,mjd0,icheck,sec0

      DOUBLE PRECISION dt

      IF(icheck.NE.33) STOP '**** gmsnom: COMMON not initialized ****'

* GMSNOM(t) is linear with time (ET) by definition:
* GMSNOM(t) = GMSNOM(t_0) + GMSTD1*(t-t_0)
      dt=(mjd-mjd0)*86400.d0+(sec-sec0)
      gmst=gms0+gms1*dt
      gmstd1=gms1

      END
