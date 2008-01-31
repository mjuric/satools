c=======================================================
c   {\bf eqpequ}: coordinate change from equinoctal 
c   polar to equinoctal elements
c     eq: equinoctal elements, see carequ
c
c     el(1)=a
c
c     el(2)=e sin (omega)
c
c     el(3)=e cos (omega)
c
c     el(4)=tg (I/2) sin (Omega)
c
c     el(5)=tg (I/2) cos (Omega)
c
c     el(6)=mean argument of latitude (=mean anom+ omega)
c
c       omega= argument of pericentre
c       Omega= longitude of asc. node
c
c   limitations: no test for hyperbolic/parabolic case; element a
c   is copied whatever its meaning
c=======================================================
      subroutine eqpequ(el,eq)
      implicit double precision (a-h,o-z)
      dimension el(6),eq(6)
      eq(1)=el(1)
      omnod=atan2(el(4),el(5))
      co=cos(omnod)
      so=sin(omnod)
      eq(2)=el(2)*so+el(3)*co
      eq(3)=el(3)*co-el(2)*so
      eq(4)=el(4)
      eq(5)=el(5)
      eq(6)=princ(el(6)+omnod)
      return
      end
