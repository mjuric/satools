* Author: Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 5, 1996
*
*  ***************************************************************
*  *                                                             *
*  *                         P R O D M V                         *
*  *                                                             *
*  *            Product of a 3x3 matrix by a 3-D vector          *
*  *                           y = Ax                            *
*  *                                                             *
*  ***************************************************************
*
      SUBROUTINE prodmv(y,a,x)
      IMPLICIT NONE
      DOUBLE PRECISION x(3),y(3),a(3,3),s
      INTEGER j,l

      DO 2 j=1,3
      s=0.d0
      DO 1 l=1,3
 1    s=s+a(j,l)*x(l)
 2    y(j)=s

      END
