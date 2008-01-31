* Copyright (C) 1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 11, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          P O L A R                            *
*  *                                                               *
*  *    Transformation from cartesian to polar coordinates         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    X         -  Cartesian coordinates
*
* OUTPUT:   ALPHA     -  Right ascension (rad)
*           DELTA     -  Declination (rad)
*           R         -  Distance
*
      SUBROUTINE polar(x,alpha,delta,r)
      IMPLICIT NONE

      DOUBLE PRECISION x(3),alpha,delta,r

      INCLUDE 'trig.h'

      DOUBLE PRECISION send,cosd,sina,cosa

      r=SQRT(x(1)**2+x(2)**2+x(3)**2)
      IF(r.EQ.0.d0) THEN
          alpha=0.d0
          delta=0.d0
          RETURN
      END IF

      send=x(3)/r
      delta=ASIN(send)
      cosd=COS(delta)
      IF(cosd.EQ.0.d0) THEN
          alpha=0.d0
          RETURN
      END IF

      cosa=x(1)/(r*cosd)
      sina=x(2)/(r*cosd)
      alpha=ATAN2(sina,cosa)
      IF(alpha.LT.0.d0) alpha=alpha+dpig

      END
