* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 10, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         P D 1 M A T                           *
*  *                                                               *
*  *      First time derivative of the product of two matrices     *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    R1        -  R1 matrix
*           R1D       -  dR1/dt
*           R2        -  R2 matrix
*           R2D       -  dR2/dt
*
* OUTPUT:   R2D       -  d(R1 R2)/dt
*
* The time derivative of the product R1 R2 is computed according
* to the formula
* 
* d(R1 R2)/dt = dR1/dt R2 + R1 dR2/dt
*
      SUBROUTINE pd1mat(r1,r1d,r2,r2d)
      IMPLICIT NONE

      DOUBLE PRECISION r1(3,3),r1d(3,3),r2(3,3),r2d(3,3)

      DOUBLE PRECISION p1(3,3),p2(3,3)

      CALL prodmm(p1,r1d,r2)
      CALL prodmm(p2,r1,r2d)
      CALL summat(r2d,p1,p2)

      END
