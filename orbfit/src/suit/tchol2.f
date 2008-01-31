* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         T C H O L 2                           *
*  *                                                               *
*  *     Tcholesky factorization of a positive-defined matrix      *
*  *                                                               *
*  *                 Written by Prof. Luigi Mussio                 *
*  *        Istituto di Topografia del Politecnico di Milano       *
*  *                   Modified by Mario Carpino                   *
*  *                                                               *
*  *****************************************************************
*
*    WARNING: only the upper triangle of the matrix is processed
*
* INPUT:    A         -  Matrix to be processed
*           NMAX      -  First dimension of A as declared in the
*                        DIMENSION statement of the calling program
*           N         -  Actual dimension of A
*
* OUTPUT:   A         -  Tcholesky factorization of the input
*                        matrix (to be supplied to SUBROUTINE INVER2)
*           INDP      -  if it is different from zero, the input matrix
*                        was not positive defined and the algorithm
*                        did not work
*
      SUBROUTINE tchol2(a,nmax,n,indp)
      IMPLICIT NONE

      INTEGER nmax,n,indp
      DOUBLE PRECISION a(nmax,n)

* NEEDED common blocks:
      INCLUDE 'comeps.h'

      INTEGER i,j,k,ii,l,nval
      DOUBLE PRECISION sn,s,pivmin

* Check for common block initialization
      IF(iiceps.NE.36) THEN
          CALL epsini
          IF(iiceps.NE.36) STOP' **** tchol2: error (01) ****'
      END IF

* Computation of matrix norm and minimum pivot
      sn=0.d0
      DO 7 i=1,n
      DO 7 k=i,n
    7 sn=sn+a(i,k)**2
      nval=(n*(n+1))/2
      sn=SQRT(sn/nval)
      pivmin=sn*eps*10.d0

* Tcholesky factorization
      DO 6 i=1,n
      l=i-1
      s=0.d0
      IF(l.EQ.0) GOTO 2
      DO 1 k=1,l
      s=s+a(k,i)*a(k,i)
    1 CONTINUE
    2 a(i,i)=a(i,i)-s
      IF(a(i,i).LE.pivmin) THEN
          indp=i
          RETURN
      END IF
      a(i,i)=SQRT(a(i,i))
      IF(i.EQ.n) GOTO 6
      ii=i+1
      DO 5 j=ii,n
      s=0.d0
      IF(l.EQ.0) GOTO 4
      DO 3 k=1,l
      s=s+a(k,i)*a(k,j)
    3 CONTINUE
    4 a(i,j)=(a(i,j)-s)/a(i,i)
    5 CONTINUE
    6 CONTINUE
      indp=0

      END
