c ======================================================
c   {\bf carequ}: coordinate change from cartesian to
c   equinoctal elements
c
c     x= position and velocity, cartesian coordinates
c
c     gm= $G \times Mass$
c
c         eq(1)= a
c
c         eq(2)=ecc*sin(dig)
c
c         eq(3)=ecc*cos(dig) ;  dig=longitude pericentre
c
c         eq(4)=tgim*cos(omega) ; tgim=tg(i/2)
c
c         eq(5)=tgim*sin(omega) ; omega=longitude ascending node
c
c         eq(6)=mean longitude
c
c         enne= mean motion
c
c  if the energy is not negative, eq(6) is set equal to true longitude,
c  eq(1)=negative a and enne=0
c
c  bugs: zero angular momentum and/or energy are handled in such
c  a way that zero divide are avoided, but overflows can occur
c
c  this definition of elements is singular only for a planar
c  retrograde orbit and parabolic orbits
c=======================================================
      subroutine carequ(x,gm,eq,enne)
      implicit double precision (a-h,o-z)
      dimension x(6),eq(6)
      dimension ang(3),f(3),g(3),vlenz(3)
c  radius and velocity squared
      vel2=prscal(x(4),x(4))
      r=vsize(x)
c  angular momentum
      call prvec(x(1),x(4),ang)
c   zero divide occurs for zero angular momentum
      gei=vsize(ang)
      if(gei.eq.0.d0)stop '****** carequ: zero angular momentum ******'
c  angular momentum unit vector, Lenz vector
      call prvec(x(4),ang,vlenz)
      do 1 i=1,3
        vlenz(i)=vlenz(i)/gm-x(i)/r
 1      ang(i)=ang(i)/gei
c   zero divide occurs for inclination of 180 degrees
      d=1.d0+ang(3)
      if(d.eq.0.d0)stop '****** carequ: 180 deg. inclination ******'
c  unit vectors of the equinoctal reference system (Broucke and
c  Cefola 1972, CM 5, 303--310) are f, g, ang
      f(1)=1.d0-ang(1)**2/d
      f(2)=-ang(1)*ang(2)/d
      f(3)=-ang(1)
      call prvec(ang,f,g)
c  elements related to eccentricity and inclination
      eq(2)=prscal(vlenz,g)
      eq(3)=prscal(vlenz,f)
      eq(4)=ang(1)/d
      eq(5)=-ang(2)/d
c     tgim1=dsqrt(eq(4)**2+eq(5)**2)
c  test on energy
      ainv=2/r-vel2/gm
      if(ainv.eq.0.d0)then
c eq(1) is q
         eq(1)=(r+prscal(vlenz,x))/(1+vsize(vlenz))
         enne=0
         cosf=prscal(x,f)
         sinf=prscal(x,g)
         eq(6)=atan2(sinf,cosf)
         eq(6)=princ(eq(6))
         return
c         stop '****** carequ: parabolic orbit ******'
      elseif(ainv.lt.0.d0)then
         eq(1)=1.d0/ainv
         enne=0
         cosf=prscal(x,f)
         sinf=prscal(x,g)
         eq(6)=atan2(sinf,cosf)
         eq(6)=princ(eq(6))
         return
      endif
c   semimajor axis and mean motion
      eq(1)=1.d0/ainv
      enne=dsqrt(gm/eq(1)**3)
c   mean longitude from non--singular Kepler equation
      ecc2=eq(2)**2+eq(3)**2
      rad=dsqrt(1.d0-ecc2)
      beta=1.d0/(1.d0+rad)
      chk=eq(2)*eq(3)*beta
      ch=1.d0-eq(2)**2*beta
      ck=1.d0-eq(3)**2*beta
      x2=prscal(x,f)
      y2=prscal(x,g)
      cosf=eq(3)+(ck*x2-chk*y2)/(eq(1)*rad)
      sinf=eq(2)+(ch*y2-chk*x2)/(eq(1)*rad)
c   eccentric longitude
      fe=datan2(sinf,cosf)
      eq(6)=fe+eq(2)*cosf-eq(3)*sinf
c   reduction to principal value
      eq(6)=princ(eq(6))
      return
      end
