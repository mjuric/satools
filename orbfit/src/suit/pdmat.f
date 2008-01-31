* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 9, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          P D M A T                            *
*  *                                                               *
*  *                  Product of two matrices                      *
*  *  (on output, the second input matrix contains the product)    *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    R1        -  R1 matrix
*           R2        -  R2 matrix
*
* OUTPUT:   R2        -  R1 R2
*
      SUBROUTINE pdmat(r1,r2)
      IMPLICIT NONE

      DOUBLE PRECISION r1(3,3),r2(3,3)

      DOUBLE PRECISION r(3,3)

      CALL prodmm(r,r1,r2)
      CALL assmat(r2,r)

      END
