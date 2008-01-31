* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                         C H E B Y M                         *
*  *                                                             *
*  *       Recursive computation of Chebyschev polynomials       *
*  *                                                             *
*  ***************************************************************
*
* INPUT:    X         -  Independent variable
*           N         -  Degree
*
* OUTPUT:   P(i)      -  Chebyschev polynomials (i=0,N)
*
      SUBROUTINE chebym(x,n,p)
      IMPLICIT NONE

      INTEGER n
      DOUBLE PRECISION x,p(0:n)

      INTEGER j

      p(0)=1.d0
      IF(n.EQ.0) RETURN
      p(1)=x
      IF(n.EQ.1) RETURN
      DO 1 j=2,n
      p(j)=2.d0*x*p(j-1)-p(j-2)
 1    CONTINUE

      END
