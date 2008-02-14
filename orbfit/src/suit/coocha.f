c ===================================================================
c COOCHA-COODER
c ===================================================================
c   general purpose coordinate to/from
c   elements change
c
c  specifically designed to handle correctly low
c  eccentricity and low inclination orbits;
c  polar orbits are supported without derivatives
c
c  limitations: only elliptic orbits are handled in this
c  version; singularity for 180 degrees inclination
c
c  coordinate types implememted:
c    'CAR' : cartesian positions and velocities in a vector
c            of length 6
c    'EQU' : equinoctal elements, see carequ, in a vector of
c            length 6; the mean motion enne is also provided
c    'EQP' : equinoctal elements for highly inclined orbit
c    'KEP' : classical keplerian elements,  see kepequ, in a
c             vector of length 6; singular for
c             0 eccentricity, 0 and 180 degrees inclination;
c
c    INPUT:  x(6), coox (string defining the coord. type of x)
c            gm=G x Mass of the Sun (anyway the central mass, e.g.
c                 mass of Sun = mass of planet for massive palnet)
c            cooy (string defining the coord. type)
c
c    OUTPUT: y(6)
c            enne=mean motion; WARNING: enne is not computed
c            for identical transformation
c ====================================================================
c COOCHA
c ====================================================================
c in this simpler version the elements are computed without derivatives
c ===============INTERFACE========================================
      subroutine coocha(x,coox,gm,y,cooy,enne)
      implicit none
c ================= input/output ================
      double precision x(6),y(6),gm,enne
      character*3 coox,cooy
c =============END INTERFACE====================
c ================ workspace ============
      double precision z(6)
c ================ loop indexes ==============
      integer j
c ================ rounding off problem ===========
      double precision roff,rouoff,eps
      integer lflag,nb
**********************************
*  static memory only for:
      save lflag,rouoff
**********************************
c  machine rounding off is computed to decide accuracy
c  required in Kepler equation and controls for singular cases
      data lflag/0/
      if(lflag.eq.0)then
         rouoff=roff(nb)
         lflag=1
      endif
c  dummy case
      if(coox.eq.cooy)then
         do 1 j=1,6
 1         y(j)=x(j)
         return
      endif
c  input is interpreted according to input type coox,
c  and anyway transformed to equinoctal elements z; enne is
c  also computed in any case apart from identical transformation
      if(coox.eq.'EQU')then
         do 2 j=1,6
 2         z(j)=x(j)
         if(x(1).gt.0.d0)then
            enne=sqrt(gm/x(1)**3)
         else
            enne=0.d0
         endif
      elseif(coox.eq.'KEP')then
         call kepequ(x,z)
         if(x(1).gt.0.d0)then
            enne=sqrt(gm/x(1)**3)
         else
            enne=0.d0
         endif
      elseif(coox.eq.'CAR')then
         call carequ(x,gm,z,enne)
      elseif(coox.eq.'EQP')then
         call eqpequ(x,z)
         if(x(1).gt.0.d0)then
            enne=sqrt(gm/x(1)**3)
         else
            enne=0.d0
         endif
      else
         write(0,*)'**** coocha: in coord.type ',coox,' unknown ****'
         stop
      endif
c  transformation to the output type cooy
      if(cooy.eq.'EQU')then
         do 3 j=1,6
 3         y(j)=z(j)
      elseif(cooy.eq.'KEP')then
c  this is a potentially singular case; the control for negligible
c  eccentricity and inclination is set to $100 \times$ rounding off
         eps=1.d2*rouoff
         call equkep(z,eps,y)
      elseif(cooy.eq.'EQP')then
c  this case is singular only for zero inclination
         eps=1.d2*rouoff
         call equeqp(z,eps,y)
      elseif(cooy.eq.'CAR')then
c  control for convergence in kepler equation is set to $100 \times$
c  rounding off
         eps=1.d2*rouoff
         call equcar(z,gm,eps,y)
      else
         write(0,*)'**** coocha: out coord.type ',cooy,' unknown ****'
         stop
      endif
      return
      end
c ======================================================
c COODER
c ======================================================
c   general purpose coordinate to/from
c   elements change: version with partial derivatives
c   definitions and input as for coocha (but no equinoctal polar)
c   in output, derpar(6,6) = matrix of partial derivatives
c ==============INTERFACE============================
      subroutine cooder(x,coox,gm,y,cooy,enne,derpar)
      implicit none
c ================= input/output ================
      double precision x(6),y(6),gm,enne,derpar(6,6)
      character*3 coox,cooy
c =============END INTERFACE====================
c ================ workspace ============
      double precision z(6),derws(6,6),derws2(6,6),w(6),ddxde(3,6,6)
c =============== scalars ===============
      double precision t0,det
c ================ loop indexes ==============
      integer j
c ================ derivatives, inversion control =======
      integer ider,ising
c ================ rounding off problem ===========
      double precision roff,rouoff,eps
      integer lflag,nb
**********************************
*  static memory only for:
      save lflag,rouoff
**********************************
c  machine rounding off is computed to decide accuracy
c  required in Kepler equation and controls for singular cases
      data lflag/0/
      if(lflag.eq.0)then
         rouoff=roff(nb)
         lflag=1
      endif
c  dummy case
      if(coox.eq.cooy)then
         do 1 j=1,6
 1         y(j)=x(j)
         call ident(6,derpar)
         return
      endif
c  input is interpreted according to input type coox,
c  and anyway transformed to equinoctal elements z; enne is
c  also computed in any case apart from identical transformation
      if(coox.eq.'EQU')then
         do 2 j=1,6
 2         z(j)=x(j)
         if(x(1).gt.0.d0)then
            enne=sqrt(gm/x(1)**3)
         else
            enne=0.d0
         endif
         call ident(6,derws)
      elseif(coox.eq.'KEP')then
         call ekensd(x,z,derws)
         if(x(1).gt.0.d0)then
            enne=sqrt(gm/x(1)**3)
         else
            enne=0.d0
         endif
      elseif(coox.eq.'CAR')then
         call carequ(x,gm,z,enne)
         t0=0.d0
         ider=1
         call prop2b(t0,z,t0,w,gm,ider,derws,ddxde)
         call matin(derws,det,6,0,6,ising,1)
      elseif(coox.eq.'EQP')then
         write(0,*)' partial derivatives for EQP not implemented'
         stop
      else
         write(0,*)'**** coocha: in coord.type ',coox,' unknown ****'
         stop
      endif
c  transformation to the output type cooy
      if(cooy.eq.'EQU')then
         do 3 j=1,6
 3         y(j)=z(j)
         call ident(6,derws2)
      elseif(cooy.eq.'KEP')then
c  this is a potentially singular case; the control for negligible
c  eccentricity and inclination is set to $100 \times$ rounding off
         eps=1.d2*rouoff
         call equkep(z,eps,y)
         call ekensd(y,w,derws2)
* ***    write(0,*)(w(ii)-z(ii),ii=1,6)
         call matin(derws2,det,6,0,6,ising,1)
      elseif(cooy.eq.'EQP')then
         write(0,*)' partial derivatives for EQP not implemented'
         stop
      elseif(cooy.eq.'CAR')then
c  control for convergence in kepler equation is set to $100 \times$
c  rounding off
         t0=0.d0
         ider=1
         call prop2b(t0,z,t0,y,gm,ider,derws2,ddxde)
         eps=1.d2*rouoff
         call equcar(z,gm,eps,w)
* ***    write(0,*)(w(ii)-y(ii),ii=1,6)
      else
         write(0,*)'**** coocha: out coord.type ',cooy,' unknown ****'
         stop
      endif
c  multiplication of jacobian matrices to get the jacobian of the composite
      call mulmat(derws,6,6,derws2,6,6,derpar)
      return
      end
