* Copyright (C) 1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 19, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         N O R P R S                           *
*  *                                                               *
*  *              Propagation of a normal matrix (NxN)             *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    NOR1      -  Normal matrix (X1 coord.)
*           JAC       -  Jacobian matrix dX2/dX1
*           N         -  Dimension (logical AND physical) of matrices
*
* OUTPUT:   NOR2      -  Normal matrix (X2 coord.) =
*                           = JAC'^(-1) NOR1 JAC^(-1)
*           ERROR     -  Error flag (singular Jacobian matrix)
*
      SUBROUTINE norprs(nor1,jac,n,nor2,error)
      IMPLICIT NONE

      INTEGER n
      DOUBLE PRECISION nor1(n,n),nor2(n,n),jac(n,n)
      LOGICAL error

      INTEGER nx
      PARAMETER (nx=10)

      INTEGER i,j,k,ising
      DOUBLE PRECISION s,tmp(nx,nx),jacinv(nx,nx),det

      IF(n.GT.nx) STOP '**** norprs: n > nx ****'

* Inversion of Jacobian matrix
      DO 5 i=1,n
      DO 5 k=1,n
      jacinv(i,k)=jac(i,k)
 5    CONTINUE
      CALL matin(jacinv,det,n,0,nx,ising,1)
      error=(ising.NE.0)
      IF(error) THEN
          DO 6 i=1,n
          DO 6 k=1,n
          nor2(i,k)=0
 6        CONTINUE
          RETURN
      END IF

      DO 2 i=1,n
      DO 2 k=1,n
      s=0
      DO 1 j=1,n
      s=s+jacinv(j,i)*nor1(j,k)
 1    CONTINUE
      tmp(i,k)=s
 2    CONTINUE

      DO 4 i=1,n
      DO 4 k=1,n
      s=0
      DO 3 j=1,n
      s=s+tmp(i,j)*jacinv(j,k)
 3    CONTINUE
      nor2(i,k)=s
 4    CONTINUE

      END
