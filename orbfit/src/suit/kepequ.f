c=======================================================
c   {\bf kepequ}: coordinate change from keplerian to
c   equinoctal elements
c     eq: equinoctal elements, see carequ
c
c     el(1)=a
c
c     el(2)=ecc
c
c     el(3)=inclination (radians)
c
c     el(4)=longitude asc. node (radians)
c
c     el(5)=argument of pericentre (radians)
c
c     el(6)=mean anomaly (radians)
c
c   bugs: no test for hyperbolic/parabolic case; element a
c   is copied whatever its meaning
c=======================================================
      subroutine kepequ(el,eq)
      implicit double precision (a-h,o-z)
      dimension el(6),eq(6)
      eq(1)=el(1)
      dig=el(4)+el(5)
      ecc=el(2)
      eq(2)=ecc*sin(dig)
      eq(3)=ecc*cos(dig)
      tgim=tan(el(3)/2.d0)
      eq(4)=tgim*dsin(el(4))
      eq(5)=tgim*dcos(el(4))
      eq(6)=dig+el(6)
      eq(6)=princ(eq(6))
      return
      end
