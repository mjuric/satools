c
c  ***************************************************************
c  *                                                             *
c  *                          e v d i s                          *
c  *                                                             *
c  *    evaluation of (squared) distance and its derivative      *
c  *                                                             *
c  ***************************************************************
c
c
c input:    t         -  time (normalized between -1 and +1)
c           coef(k,i) -  coefficients of legendre polynomials of degree
c                          (k=0,ngr; i=1,3)
c           ngr       -  degree of polynomial interpolation
c           ngx       -  first dimension of coef array - 1
c           p,pd      -  working areas (dimension >= ngx+1)
c
c output:   d2        -  squared distance
c           d2d       -  time derivative of squared distance
c
      subroutine evdis(t,coef,ngr,ngx,p,pd,d2,d2d)
      implicit double precision (a-h,o-z)
      dimension coef(0:ngx,3),p(0:ngx),pd(0:ngx)
      dimension r(3),v(3)
      call plegd1(t,ngr,p,pd)
      do 2 i=1,3
      sr=0.d0
      sv=0.d0
      do 1 j=0,ngr
      sr=sr+coef(j,i)*p(j)
 1    sv=sv+coef(j,i)*pd(j)
      r(i)=sr
 2    v(i)=sv
      d2=prscal(r,r)
      d2d=prscal(r,v)
      return
      end
