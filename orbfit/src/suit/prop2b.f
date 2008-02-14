c
c  PROP2B
c
c  Task:       Solves the two body problem in equinoctal coordinates
c
c  Features:   the Newton's method is exploited in the interval
c              [W, 2PI + W] where W is the peri-planet longitude:
c              W = w_p + RAAN
c
c  Input arguments:
c              E         =     equinoctal orbital elements 
c                               e=(a,h,k,p,q,lambda)
c              T0        =     epoch time
c              T1        =     prediction time
c              GM        =     gravitational constant of the system GM
c              IDER       =     flag for derivatives option :
c                                0 = only position and velocites
c                                1 = first derivatives
c                                2 = first and second partial derivatives 
c  Output arguments:
c              X         =     position and velocity components
c                               in absolute cartesian coordinates
c              DXDE      =     first derivatives of position vector x
c                               with respect to elements
c              DDXDE     =     second derivatives of position vector x   
c                               with respect to elements 
c
c****************
c   static memory not required
c****************
      subroutine prop2b(t0,e,t1,x,gm,ider,dxde,ddxde)
      implicit none
      double precision e(6),x(6)
      double precision f(3),g(3),w(3)
      double precision dxde(6,6),ddxde(3,6,6)
c scalars
      double precision t0,t1,gm
      integer ider
c iteration maximum
      integer iter
      parameter (iter=25)
c      parameter (iter=1000)
c scalar temporaries
      double precision enne, pml,ecc2,errm,roff,eps,pol,princ
      double precision sinel,cosel,rf,rdf,del,el,beta,r,upq,
     +  xe,ye,coe,xpe,ype,tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,
     +  tmp9,tmp10,tmp11,tmp12,dx1de2,dx2de2,dx1de3,dx2de3,dx4de2,
     +  dx5de2,dx4de3,dx5de3
      integer j,n
c  Trgonometric constants 
      include 'trig.h'
c  Mean motion 
      enne=sqrt(gm/e(1)**3)
c  Mean longitude PML at time T1
      pml=e(6)+enne*(t1-t0)
c  Absolute peri-planet longitude POL at epoch T0
      ecc2=e(2)**2+e(3)**2
      errm=roff(n)
      eps=errm*1.d2
      if(ecc2.lt.eps)then
         pol=0.d0
      elseif(ecc2.ge.1.d0)then
         write(0,*)' dcc.ge.1, ecc**2=',ecc2
         write(0,*)e
         stop
      else
         pol=atan2(e(2),e(3))
         pol=princ(pol)
      endif
c  Mean longitude restriction to [POL, POL + 2*PIGR]
c  (Newton's method continuity conditions)
      pml = princ(pml)
      if (pml.lt.pol)then
         pml = pml + dpig
      endif
c  Newton's method for motion equation solution:
c  R(F,lambda) = F - ksinF + hcosF - lambda = 0
c  search for F for a given lambda within [POL, POL + 2PIGR]
      el = pig+pol
      do 70 j=1,iter
             sinel=sin(el)
             cosel=cos(el)
             rf = el - e(3)*sinel + e(2)*cosel - pml
             rdf = 1.d0 - e(3)*cosel - e(2)*sinel
             del = - rf/rdf
             el = el + del
             if (abs(del).lt.eps) goto 100
 70   continue
      write(0,*)' Too many iter. in newton - prop2b, iter=',iter,' del=',del
      write(0,*) ' eq,eps ',e, eps
      goto 100
      stop
c  Computation of position and velocity on the orbit plane
c  in equinoctal cartesian coordinates (f,g,w)
 100  beta=1.d0/(1.d0+sqrt(1.d0-ecc2))
      xe=e(1)*((1.d0-beta*e(2)**2)*cosel+e(2)*e(3)*beta*sinel-e(3))
      ye=e(1)*((1.d0-beta*e(3)**2)*sinel+e(2)*e(3)*beta*cosel-e(2))
c  Equinoctal reference frame
      upq=1.d0+e(4)**2+e(5)**2
      f(1)=(1.d0-e(4)**2+e(5)**2)/upq
      f(2)=2.d0*e(4)*e(5)/upq
      f(3)=-2.d0*e(4)/upq
      g(1)=2.d0*e(4)*e(5)/upq
      g(2)=(1.d0+e(4)**2-e(5)**2)/upq
      g(3)=2.d0*e(5)/upq
c  Conversion from equinoctal to absolute coordinates
      call lincom(f,xe,g,ye,x)
c  Computation of velocities 
      coe=enne*e(1)**2/sqrt(xe**2+ye**2)
      xpe=coe*(e(2)*e(3)*beta*cosel-(1.d0-beta*e(2)**2)*sinel)
      ype=coe*((1.d0-beta*e(3)**2)*cosel-e(2)*e(3)*beta*sinel)
      call lincom(f,xpe,g,ype,x(4))
c  Computation of partials if required
      if(ider.lt.1)return
c  Equinoctal reference frame, third vector 
      w(1)=2.d0*e(4)/upq
      w(2)=-2.d0*e(5)/upq
      w(3)=(1.d0-e(4)**2-e(5)**2)/upq
c  Computation of same temporary variables
      r=sqrt(xe**2+ye**2)
      tmp1=pml-el
      tmp2=beta+e(2)**2*beta**3/(1.d0-beta)
      tmp3=e(2)*e(3)*beta**3/(1.d0-beta)
      tmp4=beta*e(2)-sinel
      tmp5=beta*e(3)-cosel
      tmp6=beta+e(3)**2*beta**3/(1.d0-beta)
      tmp7=1.d0-r/e(1)
      tmp8=sinel-e(2)
      tmp9=cosel-e(3)
      tmp10=e(1)*cosel/r
      tmp11=e(1)*sinel/r
      tmp12=enne*e(1)**2/r
c  Computation of derivatives of position vector w. r. to the elements 
      dxde(1,1)=(x(1)-3.d0*x(4)*(t1-t0)/2.d0)/e(1)
      dxde(2,1)=(x(2)-3.d0*x(5)*(t1-t0)/2.d0)/e(1)
      dxde(3,1)=(x(3)-3.d0*x(6)*(t1-t0)/2.d0)/e(1)
      dx1de2=-e(1)*(tmp1*tmp2+e(1)*cosel*tmp4/r)          
      dx2de2=e(1)*(tmp1*tmp3-1.d0+e(1)*cosel*tmp5/r)
      call lincom(f,dx1de2,g,dx2de2,dxde(1,2)) 
      dx1de3=-e(1)*(tmp1*tmp3+1.d0-e(1)*sinel*tmp4/r)
      dx2de3=e(1)*(tmp1*tmp6-e(1)*sinel*tmp5/r)
      call lincom(f,dx1de3,g,dx2de3,dxde(1,3))
      dxde(1,4)=2.d0*(e(5)*(ye*f(1)-xe*g(1))-xe*w(1))/upq
      dxde(2,4)=2.d0*(e(5)*(ye*f(2)-xe*g(2))-xe*w(2))/upq
      dxde(3,4)=2.d0*(e(5)*(ye*f(3)-xe*g(3))-xe*w(3))/upq
      dxde(1,5)=2.d0*(e(4)*(-ye*f(1)+xe*g(1))+ye*w(1))/upq          
      dxde(2,5)=2.d0*(e(4)*(-ye*f(2)+xe*g(2))+ye*w(2))/upq
      dxde(3,5)=2.d0*(e(4)*(-ye*f(3)+xe*g(3))+ye*w(3))/upq
      dxde(1,6)=x(4)/enne
      dxde(2,6)=x(5)/enne
      dxde(3,6)=x(6)/enne
c  Computation of derivatives of velocity vector w. r. to the elements
      dxde(4,1)=-(x(4)-3.d0*gm*x(1)*(t1-t0)/r**3)/(2.d0*e(1))
      dxde(5,1)=-(x(5)-3.d0*gm*x(2)*(t1-t0)/r**3)/(2.d0*e(1))
      dxde(6,1)=-(x(6)-3.d0*gm*x(3)*(t1-t0)/r**3)/(2.d0*e(1))
      dx4de2=tmp12*(tmp7*tmp2+e(1)**2*tmp8*tmp4/r**2+tmp10*cosel)          
      dx5de2=-tmp12*(tmp7*tmp3+e(1)**2*tmp8*tmp5/r**2-tmp10*sinel)
      call lincom(f,dx4de2,g,dx5de2,dxde(4,2))
      dx4de3=tmp12*(tmp7*tmp3+e(1)**2*tmp9*tmp4/r**2-tmp11*cosel)
      dx5de3=-tmp12*(tmp7*tmp6+e(1)**2*tmp9*tmp5/r**2+tmp11*sinel)
      call lincom(f,dx4de3,g,dx5de3,dxde(4,3))
      dxde(4,4)=2.d0*(e(5)*(ype*f(1)-xpe*g(1))-xpe*w(1))/upq
      dxde(5,4)=2.d0*(e(5)*(ype*f(2)-xpe*g(2))-xpe*w(2))/upq
      dxde(6,4)=2.d0*(e(5)*(ype*f(3)-xpe*g(3))-xpe*w(3))/upq
      dxde(4,5)=2.d0*(e(4)*(-ype*f(1)+xpe*g(1))+ype*w(1))/upq          
      dxde(5,5)=2.d0*(e(4)*(-ype*f(2)+xpe*g(2))+ype*w(2))/upq
      dxde(6,5)=2.d0*(e(4)*(-ype*f(3)+xpe*g(3))+ype*w(3))/upq
      dxde(4,6)=-enne*e(1)**3*x(1)/r**3
      dxde(5,6)=-enne*e(1)**3*x(2)/r**3
      dxde(6,6)=-enne*e(1)**3*x(3)/r**3
c  Computation of second derivatives if required
      if(ider.lt.2)return
      write(0,*) 'second derivatives not supported in this version'
      stop
      end
