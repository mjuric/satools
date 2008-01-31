* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 10, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         P R O D V S                           *
*  *                                                               *
*  *            Scalar by vector product (Y = A X)                 *
*  *                                                               *
*  *****************************************************************
*
      SUBROUTINE prodvs(y,a,x)
      IMPLICIT NONE

      DOUBLE PRECISION y(3),x(3),a

      INTEGER i

      DO 1 i=1,3
      y(i)=a*x(i)
 1    CONTINUE

      END
