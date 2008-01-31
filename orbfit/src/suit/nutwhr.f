* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 8, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         N U T W H R                           *
*  *                                                               *
*  *              Nutation angles with derivatives                 *
*  *   (Wahr's nutation series for axis b for Earth model 1066a)   *
*  *                                                               *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Time (ET)
*           NDER      -  Required order for derivatives (0/1/2)
*
* OUTPUT:   DPSI      -  Nutation in longitude (rad)
*           DEPS      -  Nutation in obliquity (rad)
*           DPSI1     -  dPSI/dt (rad/s) (only if NDER >= 1)
*           DEPS1     -  dEPS/dt (rad/s) (only if NDER >= 1)
*           DPSI2     -  d^2PSI/dt^2 (rad/s^2) (only if NDER = 2)
*           DEPS2     -  d^2EPS/dt^2 (rad/s^2) (only if NDER = 2)
*
      SUBROUTINE nutwhr(tjme,dpsi,deps,dpsi1,deps1,dpsi2,deps2,nder)
      IMPLICIT NONE

      INTEGER nder
      DOUBLE PRECISION tjme,dpsi,deps,dpsi1,deps1,dpsi2,deps2

      INTEGER i
      DOUBLE PRECISION x(9,107)
      DOUBLE PRECISION x1(180),x2(180),x3(180),x4(180),x5(180),x6(63)
      DOUBLE PRECISION s2r,s2c,s2c2,dl0,dl1,dl2,dl3,dp0,dp1,dp2,dp3
      DOUBLE PRECISION df0,df1,df2,df3,dd0,dd1,dd2,dd3,dn0,dn1,dn2,dn3
      DOUBLE PRECISION t1,dl,dp,df,dd,dn,el,elp,f,d,omega
      DOUBLE PRECISION el1,elp1,f1,d1,omega1,el2,elp2,f2,d2,omega2
      DOUBLE PRECISION arg,coefp,coefe,sina,cosa,darg,dcoefp,dcoefe
      DOUBLE PRECISION dsina,dcosa,ddarg,ddsina,ddcosa

      LOGICAL first

      EQUIVALENCE(x(1,  1),x1(1))
      EQUIVALENCE(x(1, 21),x2(1))
      EQUIVALENCE(x(1, 41),x3(1))
      EQUIVALENCE(x(1, 61),x4(1))
      EQUIVALENCE(x(1, 81),x5(1))
      EQUIVALENCE(x(1,101),x6(1))

      INCLUDE 'trig.h'

      SAVE first,x,x1,x2,x3,x4,x5,x6,s2r,s2c,s2c2,dl0,dl1,dl2,dl3
      SAVE dp0,dp1,dp2,dp3,df0,df1,df2,df3,dd0,dd1,dd2,dd3
      SAVE dn0,dn1,dn2,dn3

      DOUBLE PRECISION ssum,dsum,ddsum,c0,c1,c2,c3,t

* Table of multiples of arguments (l,l',F,D,Omega) and coefficients
* for the nutation in longitude and obliquity (unit =0.0001 arcsec);
* see: The Astronomical Almanac, 1984, page S23
      DATA x1/ 3.,  0.,  0.,  0.,  0.,       2.,    0.0,      0.,  0.0,
     +         2.,  1.,  0., -2.,  0.,       1.,    0.0,      0.,  0.0,
     +         2.,  0., -2.,  0.,  0.,      11.,    0.0,      0.,  0.0,
     +         2.,  0.,  0., -2.,  0.,      48.,    0.0,      1.,  0.0,
     +         2.,  0.,  0., -4.,  0.,      -1.,    0.0,      0.,  0.0,
     +         2.,  0.,  0.,  2.,  0.,       1.,    0.0,      0.,  0.0,
     +         2.,  0.,  0.,  0.,  0.,      29.,    0.0,     -1.,  0.0,
     +         1., -1.,  0., -1.,  0.,      -3.,    0.0,      0.,  0.0,
     +         1., -1.,  0., -2.,  0.,       1.,    0.0,      0.,  0.0,
     +         1., -1.,  0.,  0.,  0.,       5.,    0.0,      0.,  0.0,
     +         1.,  1.,  0., -2.,  0.,      -7.,    0.0,      0.,  0.0,
     +         1.,  1.,  0.,  0.,  0.,      -3.,    0.0,      0.,  0.0,
     +         1.,  0., -2., -2.,  0.,      -1.,    0.0,      0.,  0.0,
     +         1.,  0., -2.,  2.,  0.,      -1.,    0.0,      0.,  0.0,
     +         1.,  0., -2.,  0.,  0.,       4.,    0.0,      0.,  0.0,
     +         1.,  0.,  2., -2.,  0.,      -1.,    0.0,      0.,  0.0,
     +         1.,  0.,  2.,  0.,  0.,       3.,    0.0,      0.,  0.0,
     +         1.,  0.,  0., -1.,  0.,      -4.,    0.0,      0.,  0.0,
     +         1.,  0.,  0., -2.,  0.,    -158.,    0.0,     -1.,  0.0,
     +         1.,  0.,  0., -4.,  0.,      -1.,    0.0,      0.,  0.0/
      DATA x2/ 1.,  0.,  0.,  2.,  0.,       6.,    0.0,      0.,  0.0,
     +         1.,  0.,  0.,  0.,  0.,     712.,    0.1,     -7.,  0.0,
     +         0.,  1., -2.,  2.,  0.,      -1.,    0.0,      0.,  0.0,
     +         0.,  1.,  2., -2.,  0.,      -1.,    0.0,      0.,  0.0,
     +         0.,  1.,  0., -2.,  0.,      -4.,    0.0,      0.,  0.0,
     +         0.,  1.,  0.,  2.,  0.,      -1.,    0.0,      0.,  0.0,
     +         0.,  1.,  0.,  1.,  0.,       1.,    0.0,      0.,  0.0,
     +         0.,  0.,  2., -2.,  0.,     -22.,    0.0,      0.,  0.0,
     +         0.,  0.,  2.,  0.,  0.,      26.,    0.0,     -1.,  0.0,
     +         0.,  0.,  0.,  2.,  0.,      63.,    0.0,     -2.,  0.0,
     +         0.,  0.,  0.,  1.,  0.,      -4.,    0.0,      0.,  0.0,
     +        -1., -1.,  0.,  2.,  1.,       1.,    0.0,      0.,  0.0,
     +        -1.,  0.,  2., -2.,  1.,      -2.,    0.0,      1.,  0.0,
     +        -1.,  0.,  2.,  2.,  1.,     -10.,    0.0,      5.,  0.0,
     +        -1.,  0.,  2.,  0.,  1.,      21.,    0.0,    -10.,  0.0,
     +         0., -2.,  2., -2.,  1.,      -2.,    0.0,      1.,  0.0,
     +        -1.,  0.,  0.,  2.,  1.,      16.,    0.0,     -8.,  0.0,
     +        -1.,  0.,  0.,  1.,  1.,       1.,    0.0,      0.,  0.0,
     +        -1.,  0.,  0.,  0.,  1.,     -58.,   -0.1,     32.,  0.0,
     +        -2.,  0.,  2.,  0.,  1.,      46.,    0.0,    -24.,  0.0/
      DATA x3/-2.,  0.,  0.,  2.,  1.,      -6.,    0.0,      3.,  0.0,
     +        -2.,  0.,  0.,  0.,  1.,      -2.,    0.0,      1.,  0.0,
     +         2.,  0., -2.,  0.,  1.,       1.,    0.0,      0.,  0.0,
     +         2.,  0.,  2., -2.,  1.,       1.,    0.0,     -1.,  0.0,
     +         2.,  0.,  2.,  0.,  1.,      -5.,    0.0,      3.,  0.0,
     +         2.,  0.,  0., -2.,  1.,       4.,    0.0,     -2.,  0.0,
     +         2.,  0.,  0.,  0.,  1.,       2.,    0.0,     -1.,  0.0,
     +         1.,  1.,  0., -2.,  1.,      -1.,    0.0,      0.,  0.0,
     +         1.,  0.,  2., -2.,  1.,       6.,    0.0,     -3.,  0.0,
     +         1.,  0.,  2.,  2.,  1.,      -1.,    0.0,      1.,  0.0,
     +         1.,  0.,  2.,  0.,  1.,     -51.,    0.0,     27.,  0.0,
     +         1.,  0.,  0., -2.,  1.,     -13.,    0.0,      7.,  0.0,
     +         1.,  0.,  0.,  2.,  1.,      -1.,    0.0,      0.,  0.0,
     +         1.,  0.,  0.,  0.,  1.,      63.,    0.1,    -33.,  0.0,
     +         0., -1.,  2., -2.,  1.,      -5.,    0.0,      3.,  0.0,
     +         0., -1.,  2.,  0.,  1.,      -1.,    0.0,      0.,  0.0,
     +         0., -1.,  0.,  0.,  1.,     -12.,    0.0,      6.,  0.0,
     +         0.,  1.,  2., -2.,  1.,       4.,    0.0,     -2.,  0.0,
     +         0.,  1.,  2.,  0.,  1.,       1.,    0.0,      0.,  0.0,
     +         0.,  1.,  0.,  0.,  1.,     -15.,    0.0,      9.,  0.0/
      DATA x4/ 0.,  0., -2.,  2.,  1.,       1.,    0.0,      0.,  0.0,
     +         0.,  0., -2.,  0.,  1.,      -1.,    0.0,      0.,  0.0,
     +         0.,  0.,  2., -2.,  1.,     129.,    0.1,    -70.,  0.0,
     +         0.,  0.,  2.,  2.,  1.,      -7.,    0.0,      3.,  0.0,
     +         0.,  0.,  2.,  0.,  1.,    -386.,   -0.4,    200.,  0.0,
     +         0.,  0.,  0., -2.,  1.,      -5.,    0.0,      3.,  0.0,
     +         0.,  0.,  0.,  2.,  1.,      -6.,    0.0,      3.,  0.0,
     +         0.,  0.,  0.,  0.,  1.,       0.,    0.0,      0.,  0.0,
     +        -1., -1.,  2.,  2.,  2.,      -3.,    0.0,      1.,  0.0,
     +        -1.,  0.,  4.,  0.,  2.,       1.,    0.0,      0.,  0.0,
     +        -1.,  0.,  2.,  4.,  2.,      -2.,    0.0,      1.,  0.0,
     +        -1.,  0.,  2.,  2.,  2.,     -59.,    0.0,     26.,  0.0,
     +        -1.,  0.,  2.,  0.,  2.,     123.,    0.0,    -53.,  0.0,
     +        -1.,  0.,  0.,  0.,  2.,       1.,    0.0,     -1.,  0.0,
     +        -2.,  0.,  2.,  4.,  2.,      -1.,    0.0,      1.,  0.0,
     +        -2.,  0.,  2.,  2.,  2.,       1.,    0.0,     -1.,  0.0,
     +        -2.,  0.,  2.,  0.,  2.,      -3.,    0.0,      1.,  0.0,
     +         3.,  0.,  2., -2.,  2.,       1.,    0.0,      0.,  0.0,
     +         3.,  0.,  2.,  0.,  2.,      -3.,    0.0,      1.,  0.0,
     +         2.,  0.,  2., -2.,  2.,       6.,    0.0,     -3.,  0.0/
      DATA x5/ 2.,  0.,  2.,  2.,  2.,      -1.,    0.0,      0.,  0.0,
     +         2.,  0.,  2.,  0.,  2.,     -31.,    0.0,     13.,  0.0,
     +         1., -1.,  2.,  0.,  2.,      -3.,    0.0,      1.,  0.0,
     +         1.,  1.,  2., -2.,  2.,       1.,    0.0,     -1.,  0.0,
     +         1.,  1.,  2.,  0.,  2.,       2.,    0.0,     -1.,  0.0,
     +         1.,  0.,  2., -2.,  2.,      29.,    0.0,    -12.,  0.0,
     +         1.,  0.,  2.,  2.,  2.,      -8.,    0.0,      3.,  0.0,
     +         1.,  0.,  2.,  0.,  2.,    -301.,    0.0,    129., -0.1,
     +         1.,  0.,  0.,  0.,  2.,      -2.,    0.0,      1.,  0.0,
     +         0., -1.,  2.,  2.,  2.,      -3.,    0.0,      1.,  0.0,
     +         0., -1.,  2.,  0.,  2.,      -7.,    0.0,      3.,  0.0,
     +         0.,  1.,  2.,  0.,  2.,       7.,    0.0,     -3.,  0.0,
     +         0.,  1.,  0.,  0.,  2.,       1.,    0.0,      0.,  0.0,
     +         0.,  0.,  4., -2.,  2.,       1.,    0.0,      0.,  0.0,
     +         0.,  0.,  2., -1.,  2.,      -1.,    0.0,      0.,  0.0,
     +         0.,  0.,  2.,  4.,  2.,      -1.,    0.0,      0.,  0.0,
     +         0.,  0.,  2.,  2.,  2.,     -38.,    0.0,     16.,  0.0,
     +         0.,  0.,  2.,  1.,  2.,       2.,    0.0,     -1.,  0.0,
     +         0.,  0.,  2.,  0.,  2.,   -2274.,   -0.2,    977., -0.5,
     +         0.,  0.,  0.,  0.,  2.,    2062.,    0.2,   -895.,  0.5/
      DATA x6/ 0.,  2.,  0.,  0.,  0.,      17.,   -0.1,      0.,  0.0,
     +         0.,  1.,  0.,  0.,  0.,    1426.,   -3.4,     54., -0.1,
     +         0., -1.,  2., -2.,  2.,     217.,   -0.5,    -95.,  0.3,
     +         0.,  2.,  2., -2.,  2.,     -16.,    0.1,      7.,  0.0,
     +         0.,  1.,  2., -2.,  2.,    -517.,    1.2,    224., -0.6,
     +         0.,  0.,  2., -2.,  2.,  -13187.,   -1.6,   5736., -3.1,
     +         0.,  0.,  0.,  0.,  1., -171996., -174.2,  92025.,  8.9/

      DATA first/.true./

* Computation of a 3rd degree polynomial with derivatives
      ssum  (c0,c1,c2,c3,t) = ((c3*t+c2)*t+c1)*t+c0
      dsum (c0,c1,c2,c3,t) = (3.d0*c3*t+2.d0*c2)*t+c1
      ddsum(c0,c1,c2,c3,t) = 6.d0*c3*t+2.d0*c2

      IF(nder.LT.0 .OR. nder.GT.2) STOP '**** nutwhr: nder = ? ****'

      IF(first) THEN
          first=.false.
          s2r = pig/(180*3600)
          s2c = 1.d0/(36525.d0*86400.d0)
          s2c2= s2c**2
* Polynomial representation of the fundamental arguments
* see: The Astronomical Almanac, 1984, page S26
          dl0 =     485866.733d0 * s2r
          dl1 = 1717915922.633d0 * s2r
          dl2 =         31.310d0 * s2r
          dl3 =          0.064d0 * s2r
          dp0 =    1287099.804d0 * s2r
          dp1 =  129596581.224d0 * s2r
          dp2 =        - 0.577d0 * s2r
          dp3 =        - 0.012d0 * s2r
          df0 =     335778.877d0 * s2r
          df1 = 1739527263.137d0 * s2r
          df2 =       - 13.257d0 * s2r
          df3 =          0.011d0 * s2r
          dd0 =    1072261.307d0 * s2r
          dd1 = 1602961601.328d0 * s2r
          dd2 =        - 6.891d0 * s2r
          dd3 =          0.019d0 * s2r
          dn0 =     450160.280d0 * s2r
          dn1 =  - 6962890.539d0 * s2r
          dn2 =          7.455d0 * s2r
          dn3 =          0.008d0 * s2r
      END IF

* Computation of fundamental arguments
      t1=(tjme-51544.5d0)/36525.d0
      dl = ssum(dl0,dl1,dl2,dl3,t1)
      dp = ssum(dp0,dp1,dp2,dp3,t1)
      df = ssum(df0,df1,df2,df3,t1)
      dd = ssum(dd0,dd1,dd2,dd3,t1)
      dn = ssum(dn0,dn1,dn2,dn3,t1)
      el    = MOD(dl,dpig)
      elp   = MOD(dp,dpig)
      f     = MOD(df,dpig)
      d     = MOD(dd,dpig)
      omega = MOD(dn,dpig)

* First derivatives of the fundamental arguments (rad/d)
      IF(nder.GE.1) THEN
          el1    = dsum(dl0,dl1,dl2,dl3,t1) * s2c
          elp1   = dsum(dp0,dp1,dp2,dp3,t1) * s2c
          f1     = dsum(df0,df1,df2,df3,t1) * s2c
          d1     = dsum(dd0,dd1,dd2,dd3,t1) * s2c
          omega1 = dsum(dn0,dn1,dn2,dn3,t1) * s2c
      END IF

* Second derivatives of the fundamental arguments (rad/d^2)
      IF(nder.GE.2) THEN
          el2    = ddsum(dl0,dl1,dl2,dl3,t1) * s2c2
          elp2   = ddsum(dp0,dp1,dp2,dp3,t1) * s2c2
          f2     = ddsum(df0,df1,df2,df3,t1) * s2c2
          d2     = ddsum(dd0,dd1,dd2,dd3,t1) * s2c2
          omega2 = ddsum(dn0,dn1,dn2,dn3,t1) * s2c2
      END IF

* Sum of the terms of the nutation series
      dpsi  = 0.d0
      deps  = 0.d0
      dpsi1 = 0.d0
      deps1 = 0.d0
      dpsi2 = 0.d0
      deps2 = 0.d0

      DO 10 i = 1,107

* Formation of multiples of arguments
      arg    = x(1,i)*el  + x(2,i)*elp  + x(3,i)*f  + x(4,i)*d
     .                    + x(5,i)*omega
      arg    = dmod(arg,dpig)

* Formation of coefficients (first degree polynomials)
      coefp  = x(6,i) + x(7,i)*t1
      coefe  = x(8,i) + x(9,i)*t1

* Evaluate nutation term
      sina   =  SIN(arg)
      cosa   =  COS(arg)
      dpsi   = coefp*sina + dpsi
      deps   = coefe*cosa + deps

      IF(nder.GE.1) THEN
* Formation of first derivatives of multiples of arguments
          darg   = x(1,i)*el1 + x(2,i)*elp1 + x(3,i)*f1 + x(4,i)*d1
     +                        + x(5,i)*omega1

* Formation of first derivatives of coefficients
          dcoefp = x(7,i) * s2c
          dcoefe = x(9,i) * s2c

* Evaluate first derivative of nutation term
          dsina  =  cosa*darg
          dcosa  = -sina*darg
          dpsi1  = dcoefp*sina + coefp*dsina + dpsi1
          deps1  = dcoefe*cosa + coefe*dcosa + deps1
      END IF

      IF(nder.GE.2) THEN
* Formation of second derivatives of multiples of arguments
          ddarg  = x(1,i)*el2 + x(2,i)*elp2 + x(3,i)*f2 + x(4,i)*d2
     +                        + x(5,i)*omega2

* Evaluate second derivative of nutation term
          ddsina =  dcosa*darg + cosa*ddarg
          ddcosa = -dsina*darg - sina*ddarg
          dpsi2  = 2.d0*dcoefp*dsina + coefp*ddsina + dpsi2
          deps2  = 2.d0*dcoefe*dcosa + coefe*ddcosa + deps2
      END IF

 10   CONTINUE

      dpsi  = dpsi  * 1.0d-4 * s2r
      deps  = deps  * 1.0d-4 * s2r
      dpsi1 = dpsi1 * 1.0d-4 * s2r
      deps1 = deps1 * 1.0d-4 * s2r
      dpsi2 = dpsi2 * 1.0d-4 * s2r
      deps2 = deps2 * 1.0d-4 * s2r

      END
