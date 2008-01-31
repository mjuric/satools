* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 10, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         P D 2 M A T                           *
*  *                                                               *
*  *     Second time derivative of the product of two matrices     *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    R1        -  R1 matrix
*           R1D       -  dR1/dt
*           R1DD      -  d^2 R1/dt^2
*           R2        -  R2 matrix
*           R2D       -  dR2/dt
*           R2DD      -  d^2 R2/dt^2
*
* OUTPUT:   R2DD      -  d^2 (R1 R2)/dt^2
*
* The time derivative of the product R1 R2 is computed according
* to the formula
* 
* d^2 (R1 R2)/dt^2 =
*       = d^2 R1/dt^2 R2 + 2 (dR1/dt)*(dR2/dt) + R1 d^2 R2/dt^2
*
      SUBROUTINE pd2mat(r1,r1d,r1dd,r2,r2d,r2dd)
      IMPLICIT NONE

      DOUBLE PRECISION r1(3,3),r1d(3,3),r1dd(3,3)
      DOUBLE PRECISION r2(3,3),r2d(3,3),r2dd(3,3)

      INTEGER i,j
      DOUBLE PRECISION p1(3,3),p2(3,3),p3(3,3)

      CALL prodmm(p1,r1dd,r2)
      CALL prodmm(p2,r1d,r2d)
      CALL prodmm(p3,r1,r2dd)

      DO 1 i=1,3
      DO 1 j=1,3
      r2dd(i,j)=p1(i,j)+2.d0*p2(i,j)+p3(i,j)
 1    CONTINUE

      END
