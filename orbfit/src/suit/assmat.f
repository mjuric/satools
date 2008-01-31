* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 10, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         A S S M A T                           *
*  *                                                               *
*  *                   Matrix assignment (copy)                    *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    R2        -  Input matrix
*
* OUTPUT:   R1        -  R1 + R2
*
      SUBROUTINE assmat(r1,r2)
      IMPLICIT NONE

      DOUBLE PRECISION r1(3,3),r2(3,3)

      INTEGER i,j

      DO 1 i=1,3
      DO 1 j=1,3
      r1(i,j)=r2(i,j)
 1    CONTINUE

      END
