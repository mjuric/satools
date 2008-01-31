* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         I N V E R 2                           *
*  *                                                               *
*  *            Inversion of a positive-defined matrix             *
*  *          (after Tcholesky factorization with TCHOL2)          *
*  *              (originally written by Luigi Mussio)             *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    A         -  Matrix to be processed, previously factorized
*                        by SUBROUTINE TCHOL2
*           V         -  Work array
*           NMAX      -  First dimension of A as declared in the
*                        DIMENSION statement of the calling program
*           N         -  Actual dimension of A
*
* OUTPUT:   A         -  Inverse of input matrix
*
      SUBROUTINE inver2(a,v,nmax,n)
      IMPLICIT NONE

      INTEGER nmax,n
      DOUBLE PRECISION a(nmax,n),v(n)

      INTEGER i,j,k,l
      DOUBLE PRECISION s

      DO 70 i=n,1,-1
      l=i+1
      IF(i.EQ.n) GOTO 40
      DO 30 j=n,l,-1
      s=0.d0
      DO 20 k=n,l,-1
      IF(j.LT.k) GOTO 10
      s=s+a(i,k)*a(k,j)
      GOTO 20
   10 s=s+a(i,k)*a(j,k)
   20 CONTINUE
      v(j)=-s/a(i,i)
   30 CONTINUE
   40 s=0.d0
      IF(i.EQ.n) GOTO 65
      DO 50 k=n,l,-1
      s=s+a(i,k)*v(k)
   50 CONTINUE
      DO 60 j=n,l,-1
      a(i,j)=v(j)
   60 CONTINUE
   65 a(i,i)=(1.d0/a(i,i)-s)/a(i,i)
   70 CONTINUE
      DO 80 i=2,n
      DO 80 j=1,i-1
   80 a(i,j)=a(j,i)

      END
