c Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
c Version: February 24, 1997
c
c  ***************************************************************
c  *                                                             *
c  *                          p l e g m                          *
c  *                                                             *
c  *        recursive computation of legendre polynomials        *
c  *                                                             *
c  ***************************************************************
c
c
c input:    x         -  independent variable
c           n         -  degree
c
c output:   pol(i)    -  (i=0,n) legendre polynomials of degree i
c
      subroutine plegm(x,n,pol)
      implicit double precision (a-h,o-z)
      dimension pol(0:n)
      pol(0)=1.d0
      if(n.eq.0)return
      pol(1)=x
      if(n.eq.1)return
      do 1 j=2,n
 1    pol(j)=((2*j-1)*x*pol(j-1)-(j-1)*pol(j-2))/j
      return
      end
