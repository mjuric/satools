c=======================================================
c   {\bf equkep}: coordinate change from equinoctal elements
c   to keplerian
c
c     eq: equinoctal elements, see equcar/carequ
c
c     el: keplerian elements, see kepequ
c
c     eps=control on eccentricity/inclination; for smaller values,
c     the angles eq(4), eq(5) are set to arbitrary value 0
c
c   bugs: no test for hyperbolic/parabolic case; element a
c   is copied whatever its meaning
c=======================================================
      subroutine equkep(eq,eps,el)
      implicit double precision (a-h,o-z)
      dimension el(6),eq(6)
      el(1)=eq(1)
c  test on eccentricity
      ecc=sqrt(eq(2)**2+eq(3)**2)
      if(ecc.lt.eps)then
         dig=0.d0
      else
         dig=atan2(eq(2),eq(3))
      endif
      el(2)=ecc
c   test on tangent of half inclination
      tgi2=sqrt(eq(4)**2+eq(5)**2)
      if(tgi2.lt.eps)then
         el(4)=0.d0
      else
         el(4)=atan2(eq(4),eq(5))
      endif
      el(3)=2.d0*atan(tgi2)
c   angular variables
      el(5)=dig-el(4)
      el(6)=eq(6)-dig
      el(4)=princ(el(4))
      el(5)=princ(el(5))
      el(6)=princ(el(6))
      return
      end
