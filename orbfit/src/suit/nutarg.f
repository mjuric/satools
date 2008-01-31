* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         N U T A R G                           *
*  *                                                               *
*  *   Fundamental arguments of the IAU 1980 theory of nutation    *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Day (TDT)
*           NTDER     -  Required order of time derivatives
*
* OUTPUT:   ARG(5)    -  Fundamental arguments (rad)
*           ARGD(5)   -  dARG/dt (rad/d)
*           ARGDD(5)  -  d2ARG/dt2 (rad/d**2)
*
* The fundamental arguments are:
*   arg(1) = l     = mean anomaly of the Moon
*   arg(2) = l'    = mean anomaly of the Sun
*   arg(3) = F     = argument of latitude of the Moon
*   arg(4) = D     = mean elongation of the Moon from the Sun
*   arg(5) = Omega = mean longitude of the ascending node of the Moon
*
      SUBROUTINE nutarg(tjme,arg,argd,argdd,ntder)
      IMPLICIT NONE

      INTEGER ntder
      DOUBLE PRECISION tjme,arg(5),argd(5),argdd(5)

      INCLUDE 'trig.h'

      INTEGER i
      DOUBLE PRECISION s2r,d2c,d2c2,dl0,dl1,dl2,dl3,dp0,dp1,dp2,dp3
      DOUBLE PRECISION df0,df1,df2,df3,dd0,dd1,dd2,dd3,dn0,dn1,dn2,dn3
      DOUBLE PRECISION t1
      LOGICAL first

      SAVE first,d2c,d2c2,dl0,dl1,dl2,dl3,dp0,dp1,dp2,dp3
      SAVE df0,df1,df2,df3,dd0,dd1,dd2,dd3,dn0,dn1,dn2,dn3

      DOUBLE PRECISION ssum,dsum,ddsum,c0,c1,c2,c3,t
      DATA first/.true./

* Computation of a 3rd degree polynomial and derivatives
      ssum  (c0,c1,c2,c3,t) = ((c3*t+c2)*t+c1)*t+c0
      dsum (c0,c1,c2,c3,t) = (3.d0*c3*t+2.d0*c2)*t+c1
      ddsum(c0,c1,c2,c3,t) = 6.d0*c3*t+2.d0*c2

      IF(ntder.LT.0 .OR. ntder.GT.2) STOP '**** nutarg: ntder = ? ****'

      IF(first) THEN
          first=.false.
          s2r = pig/(180*3600)
          d2c = 1.d0/36525.d0
          d2c2= d2c**2
* Coefficients of polynomial representation of fundamental arguments
* from The Astronomical Almanac, 1984, page S26
*
* l = mean anomaly of the Moon
          dl0 =     485866.733d0 * s2r
          dl1 = 1717915922.633d0 * s2r
          dl2 =         31.310d0 * s2r
          dl3 =          0.064d0 * s2r
* l' = mean anomaly of the Sun
          dp0 =    1287099.804d0 * s2r
          dp1 =  129596581.224d0 * s2r
          dp2 =        - 0.577d0 * s2r
          dp3 =        - 0.012d0 * s2r
* F = argument of latitude of the moon
          df0 =     335778.877d0 * s2r
          df1 = 1739527263.137d0 * s2r
          df2 =       - 13.257d0 * s2r
          df3 =          0.011d0 * s2r
* D = mean elongation of the Moon from the Sun
          dd0 =    1072261.307d0 * s2r
          dd1 = 1602961601.328d0 * s2r
          dd2 =        - 6.891d0 * s2r
          dd3 =          0.019d0 * s2r
* Omega = mean longitude of the ascending node of the Moon
          dn0 =     450160.280d0 * s2r
          dn1 =  - 6962890.539d0 * s2r
          dn2 =          7.455d0 * s2r
          dn3 =          0.008d0 * s2r
      END IF

* Time in Julian centuries since J2000.0
      t1=(tjme-51544.5d0)/36525.d0

      arg(1) = ssum(dl0,dl1,dl2,dl3,t1)
      arg(2) = ssum(dp0,dp1,dp2,dp3,t1)
      arg(3) = ssum(df0,df1,df2,df3,t1)
      arg(4) = ssum(dd0,dd1,dd2,dd3,t1)
      arg(5) = ssum(dn0,dn1,dn2,dn3,t1)

      DO 1 i=1,5
      arg(i)=MOD(arg(i),dpig)
 1    CONTINUE

* First derivatives of the fundamental arguments
      IF(ntder.GE.1) THEN
          argd(1) = dsum(dl0,dl1,dl2,dl3,t1) * d2c
          argd(2) = dsum(dp0,dp1,dp2,dp3,t1) * d2c
          argd(3) = dsum(df0,df1,df2,df3,t1) * d2c
          argd(4) = dsum(dd0,dd1,dd2,dd3,t1) * d2c
          argd(5) = dsum(dn0,dn1,dn2,dn3,t1) * d2c
      END IF

* Second derivatives of the fundamental arguments
      IF(ntder.GE.2) THEN
          argdd(1) = ddsum(dl0,dl1,dl2,dl3,t1) * d2c2
          argdd(2) = ddsum(dp0,dp1,dp2,dp3,t1) * d2c2
          argdd(3) = ddsum(df0,df1,df2,df3,t1) * d2c2
          argdd(4) = ddsum(dd0,dd1,dd2,dd3,t1) * d2c2
          argdd(5) = ddsum(dn0,dn1,dn2,dn3,t1) * d2c2
      END IF

      END
