* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         L G N I N T                           *
*  *                                                               *
*  *    Least squares interpolation using Chebychev polynomials    *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    X(i)      -  (i=1,NPT) Dependent variable
*           T(i)      -  (i=1,NPT) Independent variable (NORMALIZED)
*           NPT       -  Number of data points
*           NGR       -  Polynomial degree
*
* OUTPUT:   COEF(i)   -  (i=0,NGR) Coefficients of Legendre polynomials
*           SIGMA     -  Fit RMS error
*
      SUBROUTINE lgnint(x,t,npt,ngr,coef,sigma)
      IMPLICIT NONE

      INTEGER npt,ngr
      DOUBLE PRECISION x(npt),t(npt),coef(0:ngr),sigma

      INCLUDE 'parlgi.h'

      INTEGER i,j,k,indp
      DOUBLE PRECISION atx(0:lgintx),covc(0:lgintx,0:lgintx)
      DOUBLE PRECISION poly(0:lgintx),v(0:lgintx),xk,xt,resk,cx

      IF(ngr.GT.lgintx) STOP '**** lgnint: ngr > lgintx ****'

      DO 1 i=0,ngr
      atx(i)=0.d0
      DO 1 j=i,ngr
      covc(i,j)=0.d0
 1    CONTINUE

* Computation of the scalar products with Legendre polynomials
      DO 2 k=1,npt
      xk=x(k)
      CALL chebym(t(k),ngr,poly)
      DO 2 i=0,ngr
      atx(i)=atx(i)+poly(i)*xk
      DO 2 j=i,ngr
      covc(i,j)=covc(i,j)+poly(i)*poly(j)
 2    CONTINUE

* Least squares fit
      CALL tchol2(covc,lgintx+1,ngr+1,indp)
      IF(indp.NE.0) THEN
          WRITE(6,100) indp
          STOP '**** lgnint: abnormal end ****'
      END IF
 100  FORMAT(' **** lgnint: INDP =',i4,' ****')
      CALL inver2(covc,v,lgintx+1,ngr+1)

* Polynomial coefficients
      DO 4 i=0,ngr
      cx=0.d0
      DO 3 j=0,ngr
      cx=cx+covc(i,j)*atx(j)
 3    CONTINUE
      coef(i)=cx
 4    CONTINUE

* RMS error
      sigma=0.d0
      DO 6 k=1,npt
      CALL chebym(t(k),ngr,poly)
      xt=0.d0
      DO 5 j=0,ngr
      xt=xt+coef(j)*poly(j)
 5    CONTINUE
      resk=x(k)-xt
      sigma=sigma+resk**2
 6    CONTINUE
      sigma=sqrt(sigma/npt)

      END
