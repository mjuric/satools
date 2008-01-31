* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 26, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C O V P R S                           *
*  *                                                               *
*  *      Covariance propagation for a NxN (square) matrix         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    C1        -  Covariance matrix E<dX1 dX1'>
*           JAC       -  Jacobian matrix dX2/dX1
*           N         -  Dimension (logical AND physical) of matrices
*
* OUTPUT:   C2        -  Covariance matrix E<dX2 dX2'> = JAC C1 JAC'
*
      SUBROUTINE covprs(c1,jac,n,c2)
      IMPLICIT NONE

      INTEGER n
      DOUBLE PRECISION c1(n,n),c2(n,n),jac(n,n)

      INTEGER nx
      PARAMETER (nx=10)

      INTEGER i,j,k
      DOUBLE PRECISION s,tmp(nx,nx)

      IF(n.GT.nx) STOP '**** covprs: n > nx ****'

      DO 2 i=1,n
      DO 2 k=1,n
      s=0
      DO 1 j=1,n
      s=s+jac(i,j)*c1(j,k)
 1    CONTINUE
      tmp(i,k)=s
 2    CONTINUE

      DO 4 i=1,n
      DO 4 k=1,n
      s=0
      DO 3 j=1,n
      s=s+tmp(i,j)*jac(k,j)
 3    CONTINUE
      c2(i,k)=s
 4    CONTINUE

      END
