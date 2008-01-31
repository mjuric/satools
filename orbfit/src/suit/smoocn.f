* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         S M O O C N                           *
*  *                                                               *
*  *               Smooth Cn link between 0 and 1                  *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    X         -  Independent variable
*           NSMORD      -  Order of smoothness
*
* OUTPUT:   Y         -  y(x)
*           Y1        -  dy/dx
*           Y2        -  d2y/dx2
*
* The function y(x) satisfies the following conditions:
*   1) y(x)=0 for x <= 0
*   2) y(x)=1 for x >= 1
*   3) y(x) is continuous with its derivatives up to order NSMORD
*           for any x
*
* In the interval 0 <= x <= 1, y(x) is defined as the minimum
* degree polynomial P(x) satisfying the conditions:
*   1) P(0) = 0
*   2) P(1) = 1
*   3) d^n P/dx^n = 0 for x=0 and x=1 (n=1,NSMORD)
*
* Since these are 2*NSMORD+2 independent equations, they can be
* satisfied by a polynomial of degree 2*NSMORD+1
*
      SUBROUTINE smoocn(x,y,y1,y2,nsmord)
      IMPLICIT NONE

      INTEGER nsmord
      DOUBLE PRECISION x,y,y1,y2

      INCLUDE 'parsmo.h'

      INTEGER i,k,ng,nd1,nd2,ider,ising
      INTEGER ic(nd1x),id(nd1x)
      DOUBLE PRECISION a(nd1x,nd2x),det,xn,xn2
      DOUBLE PRECISION cp(nd1x,nd1x),cp1(nd1x,nd1x),cp2(nd1x,nd1x)
      LOGICAL first,cpcmp(nd1x)
      SAVE first,cpcmp,cp,cp1,cp2
      DATA first/.true./

c     SAVE first,cpcmp,cp,cp1,cp2

      IF(nsmord.LT.0) STOP '**** smoocn: internal error (01) ****'

      IF(first) THEN
          DO 10 k=1,nd1x
          cpcmp(k)=.true.
 10       CONTINUE
          first=.false.
      END IF

* Polynomial degree
      ng=2*nsmord+1
      nd1=nsmord+1
      nd2=nsmord+2
      IF(nd1.GT.nd1x) STOP '**** smoocn: nd1 > nd1x ****'

      IF(cpcmp(nd1)) THEN

          DO 1 k=1,nd1
          i=2*k-1
          ic(k)=1
          id(k)=i
          a(1,k)=ic(k)
          a(1,nd2)=0.5d0
 1        CONTINUE

          DO 3 ider=1,nsmord
          DO 2 k=1,nd1
          i=2*k-1
          IF(id(k).GT.0) THEN
              ic(k)=ic(k)*id(k)
          ELSE
              ic(k)=0
          END IF
          id(k)=id(k)-1
          a(ider+1,k)=ic(k)
 2        CONTINUE
          a(ider+1,nd2)=0.d0
 3        CONTINUE

          CALL matin(a,det,nd1,1,nd1x,ising,0)
          IF(ising.NE.0) STOP '**** smoocn: internal error (02) ****'

          DO 4 k=1,nd1
          i=2*k-1
          ic(k)=1
          id(k)=i
 4        cp(nd1,k)=a(k,nd2)

          DO 5 k=1,nd1
          i=2*k-1
          IF(id(k).GT.0) THEN
              ic(k)=ic(k)*id(k)
          ELSE
              ic(k)=0
          END IF
          id(k)=id(k)-1
 5        cp1(nd1,k)=cp(nd1,k)*ic(k)

          DO 6 k=1,nd1
          i=2*k-1
          IF(id(k).GT.0) THEN
              ic(k)=ic(k)*id(k)
          ELSE
              ic(k)=0
          END IF
 6        cp2(nd1,k)=cp(nd1,k)*ic(k)

          cpcmp(nd1)=.false.
      END IF

      xn=2*x-1
      y=0.d0
      y1=0.d0
      y2=0.d0

      IF(x.LE.0.d0) RETURN
      IF(x.GE.1.d0) THEN
          y=1.d0
          RETURN
      END IF

      xn2=xn**2
      DO 11 k=nd1,1,-1
      y=y*xn2+cp(nd1,k)
      y1=y1*xn2+cp1(nd1,k)
 11   CONTINUE
      y=0.5d0+y*xn
      y1=2*y1
      DO 12 k=nd1,2,-1
      y2=y2*xn2+cp2(nd1,k)
 12   CONTINUE
      y2=4*y2*xn

      END
