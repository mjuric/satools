c Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
c Version: February 24, 1997
c
c  ***************************************************************
c  *                                                             *
c  *                         p l e g d 1                         *
c  *                                                             *
c  *        recursive computation of legendre polynomials        *
c  *             with derivatives up to first order              *
c  *                                                             *
c  ***************************************************************
c
c
c input:    x         -  independent variable
c           n         -  degree
c
c output:   p(i)      -  (i=0,n) legendre polynomials of degree i
c           pd(i)     -  (i=0,n) first derivatives
c
      subroutine plegd1(x,n,p,pd)
      implicit double precision (a-h,o-z)
      dimension p(0:n),pd(0:n)
c legendre polynomials
      p(0)=1.d0
      if(n.eq.0)goto 2
      p(1)=x
      if(n.eq.1)goto 2
      do 1 j=2,n
 1    p(j)=((2*j-1)*x*p(j-1)-(j-1)*p(j-2))/j
c first derivatives
 2    pd(0)=0.d0
      if(n.eq.0)return
      pd(1)=1.d0
      if(n.eq.1)return
      do 3 j=2,n
 3    pd(j)=x*pd(j-1)+j*p(j-1)
      return
      end
