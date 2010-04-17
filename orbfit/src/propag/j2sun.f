c ***************************************************************
c J2SUN
c ***************************************************************
c This subroutine computes the J_2 acceleration on an asteroid
c due to the Sun; J_2(Sun) = 2 x 10^-7 according to JPL DE405.
c Neglects tilt of the solar spin axis to the ecliptic.
c ***************************************************************
      subroutine j2sun(x,accj2)
      implicit none
c planetary masses and other model parameters
      double precision solj2,radsun
      parameter (solj2=2.d-7,radsun=4.6527174d-3)
      include 'parbep.h'
      include 'masses.h'
c scalars
      double precision xsun2,xsun5,ratio,brac1,brac2
c vectors
      double precision x(3),accj2(3)
c function
      double precision prscal
c ---------------------------------------------------------------
      xsun2=prscal(x,x)
      xsun5=xsun2*xsun2*dsqrt(xsun2)
      ratio=1.5d0*gm0*solj2*(radsun*radsun/xsun2)/xsun5
      brac1=5.d0*x(3)*x(3)-xsun2
      brac2=5.d0*x(3)*x(3)-3.d0*xsun2
      accj2(1)=ratio*x(1)*brac1
      accj2(2)=ratio*x(2)*brac1
      accj2(3)=ratio*x(3)*brac2
c      write(99,*)accj2(1),accj2(2)
      return
      end
