* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 10, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         S U M M A T                           *
*  *                                                               *
*  *                     Sum of two matrices                       *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    B         -  B matrix
*           C         -  C matrix
*
* OUTPUT:   A         -  A = B + C
*
      SUBROUTINE summat(a,b,c)
      IMPLICIT NONE

      DOUBLE PRECISION a(3,3),b(3,3),c(3,3)

      INTEGER i,j

      DO 1 i=1,3
      DO 1 j=1,3
      a(i,j)=b(i,j)+c(i,j)
 1    CONTINUE

      END
