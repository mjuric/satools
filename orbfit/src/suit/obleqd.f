* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 8, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         O B L E Q D                           *
*  *                                                               *
*  *      Mean obliquity of ecliptic (with time derivatives)       *
*  *            (see Astronomical Almanac 1987, B18)               *
*  *                                                               *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Time (ET)
*           NDER      -  Required order for derivatives (0/1/2)
*
* OUTPUT:   OBL       -  Mean obliquity of ecliptic (rad)
*           OBL1      -  dOBL/dt (rad/s) (only if NDER >= 1)
*           OBL2      -  d^2OBL/dt^2 (rad/s^2) (only if NDER = 2)
*
      SUBROUTINE obleqd(tjme,obl,obl1,obl2,nder)
      IMPLICIT NONE

      INTEGER nder
      DOUBLE PRECISION tjme,obl,obl1,obl2

      INCLUDE 'trig.h'

      DOUBLE PRECISION s2r,s2c,s2c2,ob0,ob1,ob2,ob3
      DOUBLE PRECISION ts
      LOGICAL first


      SAVE first,s2r,s2c,s2c2,ob0,ob1,ob2,ob3

      DOUBLE PRECISION ssum,dsum,ddsum,c0,c1,c2,c3,t
      DATA first/.true./

* Computation of a 3rd degree polynomial with derivatives
      ssum  (c0,c1,c2,c3,t) = ((c3*t+c2)*t+c1)*t+c0
      dsum (c0,c1,c2,c3,t) = (3.d0*c3*t+2.d0*c2)*t+c1
      ddsum(c0,c1,c2,c3,t) = 6.d0*c3*t+2.d0*c2

      IF(nder.LT.0 .OR. nder.GT.2) STOP '**** obleqd: nder = ? ****'

      IF(first) THEN
          first=.false.
          s2r=pig/(180.d0*3600.d0)
          s2c = 1.d0/(36525.d0*86400.d0)
          s2c2= s2c**2

* Polynomial coefficients of the representation of OBL (see
* Astronomical Almanac 1984, S21)
          ob0 =  (23*3600+26*60+21.448d0) * s2r
          ob1 =  -46.8150d0  * s2r
          ob2 =  -0.00059d0  * s2r
          ob3 =   0.001813d0 * s2r
      END IF

      ts  = (tjme-51544.5d0)/36525.d0
      obl = ssum(ob0,ob1,ob2,ob3,ts)
      IF(nder.GE.1) obl1 =  dsum(ob0,ob1,ob2,ob3,ts) * s2c
      IF(nder.GE.2) obl2 = ddsum(ob0,ob1,ob2,ob3,ts) * s2c2

      END
