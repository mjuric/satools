* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 16, 1996
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                          V S I Z E                          *
*  *                                                             *
*  *                     Size of a 3-D vector                    *
*  *                                                             *
*  ***************************************************************
*
      DOUBLE PRECISION FUNCTION vsize(x)
      IMPLICIT NONE

      DOUBLE PRECISION x(3),s

      s=x(1)*x(1)+x(2)*x(2)+x(3)*x(3)
      vsize=SQRT(s)

      END
