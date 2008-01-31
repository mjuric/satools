c ======================================================
c   {\bf equcar}: coordinate change from
c   equinoctal elements to cartesian
c
c     x= position and veloctiy, cartesian coordinates
c
c     gm= $G \times Mass$
c
c     eps= convergence control for non--singular Kepler equation
c
c     eq: equinoctal elements, see carequ
c
c         enne= mean motion
c
c  bugs: if  eq(1) is negative, the hyperbolic case is not handled in
c  this version
c
c  this definition of elements is singular only for a planar
c  retrograde orbit and parabolic orbits
c=======================================================
      subroutine equcar(eq,gm,eps,x)
c  ====================================================================
      implicit double precision (a-h,o-z)
      dimension x(6),eq(6),f(3),g(3)
      include 'trig.h'
c   test for hyperbolic orbit
      if(eq(1).le.0.d0)then
         stop'****** equcar: hyperbolic/parabolic orbit ******'
      endif
c  non--singular intermediate variables
      ecc2=eq(2)**2+eq(3)**2
      rad=dsqrt(1.d0-ecc2)
      beta=1.d0/(1.d0+rad)
      chk=eq(2)*eq(3)*beta
      ch=1.d0-eq(2)**2*beta
      ck=1.d0-eq(3)**2*beta
      tgim2=eq(4)**2+eq(5)**2
c     tgim=dsqrt(tgim2)
      opwz=1.d0+tgim2
c   mean motion
      enne=dsqrt(gm/eq(1)**3)
c  unit vectors of the equinoctal reference system (Broucke and
c  Cefola 1972, CM 5, 303--310) are f, g and the angular momentum
c  unit vector
      f(1)=(1.d0-eq(4)**2+eq(5)**2)/opwz
      f(2)=2*eq(4)*eq(5)/opwz
      f(3)=-2*eq(4)/opwz
      g(1)=2*eq(4)*eq(5)/opwz
      g(2)=(1.d0+eq(4)**2-eq(5)**2)/opwz
      g(3)=2*eq(5)/opwz
c  Non singular Kepler equation
      ecc=dsqrt(ecc2)
      if(ecc.lt.eps)then
c  for negligible eccentricity, the eccentric longitude is
c  set equal to the mean longitude
         fe=eq(6)
         cosf=cos(fe)
         sinf=sin(fe)
      else
c  mean longitude is reduced to the interval (dig, dig+2*pig)
c  with dig=longitude of pericentre
         tlong=eq(6)
         dig=atan2(eq(2),eq(3))
         tlong=princ(tlong-dig)+dig
c  initial condition for Newton's method is always
c  the longitude of apocentre; this ensures the right
c  convexity for secure convergence
         fe=dig+pig
         do 16 i=1,100
           cosf=cos(fe)
           sinf=sin(fe)
           df=(fe-tlong+eq(2)*cosf-eq(3)*sinf)/
     +        (1.d0-eq(2)*sinf-eq(3)*cosf)
           if(dabs(df).lt.eps)goto 17
 16        fe=fe-df
c  convergence problems -- this should happen only for
c  extremely high eccentricity
         stop'****** equcar: 100 iterations of Newton ******'
      endif
c  cartesian coordinates in the equinoctal frame x2, y2, 0
 17   x2=eq(1)*(ch*cosf+chk*sinf-eq(3))
      y2=eq(1)*(ck*sinf+chk*cosf-eq(2))
      do 18 i=1,3
 18     x(i)=x2*f(i)+y2*g(i)
c  cartesian velocities in the equinoctal frame xp2, yp2, 0
      de=enne*eq(1)**2/dsqrt(x2**2+y2**2)
      xp2=de*(chk*cosf-ch*sinf)
      yp2=de*(ck*cosf-chk*sinf)
      do 19 i=1,3
 19     x(i+3)=xp2*f(i)+yp2*g(i)
      return
      end
