* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C H E B Y D                           *
*  *                                                               *
*  *         Recursive computation of Chebychev polynomials        *
*  *             with derivatives up to second order               *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    X         -  Independent variable
*           N         -  Degree
*           NTDER     -  Required time derivative (=0, 1 or 2)
*
* OUTPUT:   P(i)      -  (i=0,N) Chebychev polynomials of degree i P(x)
*           PD(i)     -  First derivative dP/dx (if NTDER >= 1)
*           PDD(i)    -  Second derivative d2P/dx2 (if NTDER >= 2)
*
      SUBROUTINE chebyd(x,n,ntder,p,pd,pdd)
      IMPLICIT NONE

      INTEGER n,ntder
      DOUBLE PRECISION x,p(0:n),pd(0:n),pdd(0:n)

      INTEGER j
      DOUBLE PRECISION x2

      IF(ntder.LT.0 .OR. ntder.GT.2) STOP '**** chebyd: ntder = ? ****'

      x2=2.d0*x

* Chebychev polynomials
      p(0)=1.d0
      IF(n.EQ.0) GOTO 2
      p(1)=x
      IF(n.EQ.1) GOTO 2
      DO 1 j=2,n
      p(j)=x2*p(j-1)-p(j-2)
 1    CONTINUE

* First derivatives
 2    CONTINUE
      IF(ntder.LT.1) RETURN
      pd(0)=0.d0
      IF(n.EQ.0) GOTO 4
      pd(1)=1.d0
      IF(n.EQ.1) GOTO 4
      DO 3 j=2,n
      pd(j)=x2*pd(j-1)+2*p(j-1)-pd(j-2)
 3    CONTINUE

* Second derivatives
 4    CONTINUE
      IF(ntder.LT.2) RETURN
      pdd(0)=0.d0
      IF(n.EQ.0) RETURN
      pdd(1)=0.d0
      IF(n.EQ.1) RETURN
      DO 5 j=2,n
      pdd(j)=x2*pdd(j-1)+4*pd(j-1)-pdd(j-2)
 5    CONTINUE

      END
