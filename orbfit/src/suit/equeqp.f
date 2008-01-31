c=======================================================
c   {\bf equeqp}: coordinate change from equinoctal elements
c   to equinoctal polar
c
c     eq: equinoctal elements, see equcar/carequ
c
c     el: equinoctal polar elements, see eqpequ
c
c     eps=control on inclination; for smaller values,
c       the computation stops 
c   limitation: no test for hyperbolic/parabolic case; element a
c   is copied whatever its meaning
c=======================================================
      subroutine equeqp(eq,eps,el)
      implicit double precision (a-h,o-z)
      dimension el(6),eq(6)
      el(1)=eq(1)
c  test on inclination
      tgi2=sqrt(eq(4)**2+eq(5)**2)
      if(tgi2.lt.eps)then
         write(*,*)' inclination zero cannot be handled in eqp'
         stop
      else
         omnod=atan2(eq(4),eq(5))
      endif
      co=cos(omnod)
      so=sin(omnod)
      el(2)=eq(2)*co-eq(3)*so
      el(3)=eq(3)*co+eq(2)*so
      el(4)=eq(4)
      el(5)=eq(5)
      el(6)=princ(eq(6)-omnod)
      return
      end
