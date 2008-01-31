* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 10, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          T R S P 3                            *
*  *                                                               *
*  *                     Transpose of a matrix                     *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    R         -  Input matrix
*
* OUTPUT:   R         -  R' (transpose of input matrix)
*
      SUBROUTINE trsp3(r)
      IMPLICIT NONE

      DOUBLE PRECISION r(3,3)

      INTEGER i,j
      DOUBLE PRECISION rt

      DO 1 i=1,2
      DO 1 j=i+1,3
      rt=r(i,j)
      r(i,j)=r(j,i)
      r(j,i)=rt
 1    CONTINUE

      END
