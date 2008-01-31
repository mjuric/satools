* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 10, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                           S U M V                             *
*  *                                                               *
*  *             Sum of two vectors (Y = X1 + X2)                  *
*  *                                                               *
*  *****************************************************************
*
      SUBROUTINE sumv(y,x1,x2)
      IMPLICIT NONE

      DOUBLE PRECISION y(3),x1(3),x2(3)

      INTEGER i

      DO 1 i=1,3
      y(i)=x1(i)+x2(i)
 1    CONTINUE

      END
