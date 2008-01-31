* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          X Y P O L                            *
*  *                                                               *
*  *     Computation of the coordinates of the terrestrial pole    *
*  *                 with their time derivatives                   *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Day (TDT)
*           NTDER     -  Required order of time derivatives (0/1/2)
*
* OUTPUT:   XPOL      -  (x,y) pole coordinates (rad)
*           X1POL     -  d(x,y)/dET (rad/s) (only if NDER >= 1)
*           X2POL     -  d^2 (x,y)/dET^2 (rad/s^2) (only if NDER = 2)
*
      SUBROUTINE xypol(tjme,xpol,x1pol,x2pol,nder)
      IMPLICIT NONE

      INTEGER nder
      DOUBLE PRECISION tjme,xpol(2),x1pol(2),x2pol(2)

      INCLUDE 'trig.h'

      DOUBLE PRECISION c0,c1,c2,eop(5),eopd(5),eopdd(5)
      INTEGER i
      LOGICAL first

      SAVE first,c0,c1,c2
      DATA first/.true./

      IF(first) THEN
          first=.false.
          c0=pig/(180.d0*3600.d0)
          c1=c0/86400.d0
          c2=c1/86400.d0
      END IF

* Interpolation of IERS data
      CALL iersts(tjme,eop,eopd,eopdd,nder)

* Transformation of units from  arcsec, arcsec/d, arcsec/d^2
* to rad, rad/s, rad/s^2
      DO 1 i=1,2
      xpol(i)=eop(i)*c0
      x1pol(i)=eopd(i)*c1
      x2pol(i)=eopdd(i)*c2
 1    CONTINUE

      END
