c ******************************************************************
      subroutine yarkdi(xast,a,iparti)
c ******************************************************************
c
c This subroutine computes the heliocentric components of the
c Yarkovsky thermal acceleration -- the diurnal variant only.
c If the flag (iparti) in the common block is set to 1, one gets at
c the output also partials wrt to some parameters of the thermal
c model (if these are desired to be adjusted).
c
c Input parameters:
c -----------------
c
c - via header   xast(3) ... heliocentric coordinates of the body (in AU)
c - via common   yarkp(1-3) ... sx, sy, sz (unit vector of the
c                               body's spin axis orientation)
c                yarkp(4-5) ... k_0 and k_1 parameters of the
c                               surface thermal conductivity
c                               [K(T) = k_0 + k_1 T_av^3]
c                yarkp(6) ... density of the surface layer
c                yarkp(7) ... radius of the body
c                yarkp(8) ... rotation frequency
c                yarkp(9) ... surface absorptivity
c                iparti   ... partials (yes=1/no=0)
c                             [presently only partials listed below,
c                              a(4) - a(21) are available]
c
c
c Output parameters: a(1-3) ... diurnal acceleration
c ------------------ a(4-6) ... partials wrt the radius of the body
c                    a(7-9) ... partials wrt the thermal conductivity
c                               parameter k_0
c                    a(10-12) ... partials wrt the thermal conductivity
c                                 parameter k_1
c                    a(13-15) ... partials wrt the x-component of the
c                                 spin axis unit vector
c                    a(16-18) ... partials wrt the y-component of the
c                                 spin axis unit vector
c                    a(19-21) ... partials wrt the z-component of the
c                                 spin axis unit vector
c
c SI units are assumed internally in the subroutine, but the results
c (e.g. accelerations) are given in AU and days.
c
c Written by: D. Vokrouhlicky, Oct 99
c (queries to vokrouhl@mbox.cesnet.cz)
c ..................................................................
      implicit double precision (a-h,o-z)
c here we specify two more parameters that eventually might be changed:
c -- the average bulk density of the body (densityb) which is now set
c    to 2 g/cm^3
c -- the heat capacity of the surface layer (capacity) which is now set
c    to 680 J/kg/K
      parameter (densityb=2.d3,capacity=680.d0,solcon=1371.d0)
      parameter (emiss=0.9d0,stefboltz=5.66962d-8,clight3=8.99377374d8)
      parameter (dsqrt2=1.414213562373d0,dsqrt23=1414.213562373d0)
      parameter (aceuni=0.049900176d0)
c input: asteroid position, flag for partials 
      double precision xast(3)
      integer iparti
c output: acceleration and partials
      double precision a(21)
c internal variables
      double precision vprod1(3),vprod2(3)
c physical data on the current asteroid
      INCLUDE 'yarkov.h'    
c -----------------------------------------------------------------------
      rau2=xast(1)*xast(1)+xast(2)*xast(2)+xast(3)*xast(3)
      rau=dsqrt(rau2)
      xn=xast(1)/rau
      yn=xast(2)/rau
      zn=xast(3)/rau
c initializations & constants
      radflu=solcon/rau2
c - subsolar temperature
      tstar=(yarkp(9)*radflu/emiss/stefboltz)**0.25d0
      tav1000=tstar/dsqrt23
c - surface conductivity
      surcon=yarkp(4)+yarkp(5)*(tav1000**3)
c - thermal inertia & diurnal thermal parameter
      bgama=dsqrt(surcon*yarkp(6)*capacity)
      theta=bgama*dsqrt(yarkp(8))/emiss/stefboltz/(tstar**3)
      diudepth=dsqrt(surcon/yarkp(6)/capacity/yarkp(8))
c - radius of the body scaled by the depth of the diurnal wave
      rp=yarkp(7)/diudepth
      al=dsqrt2*rp
      tau=theta/al
      tau1=1.d0+tau
c - the auxiliary functions A-D, a,b
      cal=dcos(al)
      sal=dsin(al)
      if (al.lt.90.d0) then
       ealm=dexp(-al)
      else
       ealm=0.d0
      endif
      af=3.d0*(al+2.d0)*ealm+(3.d0*(al-2.d0)*cal+al*(al-3.d0)*sal)
      bf=al*(al+3.d0)*ealm+(-al*(al-3.d0)*cal+3.d0*(al-2.d0)*sal)
      caf=-(al+2.d0)*ealm+(-(al-2.d0)*cal+al*sal)
      cbf=-al*ealm-(al*cal+(al-2.d0)*sal)
      ccf=caf+tau*af/tau1
      cdf=cbf+tau*bf/tau1
c - G exp(i delta) & amplitude computed
      facp=aceuni*yarkp(9)*radflu/yarkp(7)/densityb/clight3
      deno=ccf*ccf+cdf*cdf
      deno1=deno*tau1
      gcosd=(caf*ccf+cbf*cdf)/deno1
      gsind=(cbf*ccf-caf*cdf)/deno1
c geometric products
c - r x s
      vprod1(1)=yn*yarkp(3)-zn*yarkp(2)
      vprod1(2)=zn*yarkp(1)-xn*yarkp(3)
      vprod1(3)=xn*yarkp(2)-yn*yarkp(1)
c - s x (r x s) = r - (r.s) s
      scalar=xn*yarkp(1)+yn*yarkp(2)+zn*yarkp(3)
      vprod2(1)=xn-scalar*yarkp(1)
      vprod2(2)=yn-scalar*yarkp(2)
      vprod2(3)=zn-scalar*yarkp(3)
c diurnal acceleration
      a(1)=facp*(gsind*vprod1(1)+gcosd*vprod2(1))
      a(2)=facp*(gsind*vprod1(2)+gcosd*vprod2(2))
      a(3)=facp*(gsind*vprod1(3)+gcosd*vprod2(3))
c Partials?
      if (iparti.eq.0) return
c - general
      cafp=-ealm+cal+(2.d0*al-1.d0)*sal
      cbfp=-ealm-(2.d0*al-1.d0)*cal+sal
      afp=3.d0*ealm+(al*al-3.d0)*cal+(al*(al-4.d0)+3.d0)*sal
      bfp=(2.d0*al+3.d0)*ealm-(al*(al-4.d0)+3.d0)*cal
     .     +(al*al-3.d0)*sal
c - thermal conductivity parameters (k_0,k_1)
      xi1r=caf*ccf-cbf*cdf
      xi1i=cbf*ccf+caf*cdf
      xi2r=cafp*af-cbfp*bf
      xi2i=cbfp*af+cafp*bf
      xi2r=xi2r-caf*afp+cbf*bfp
      xi2i=xi2i-cbf*afp-caf*bfp
      deno=xi1r*xi1r+xi1i*xi1i
      facr=1.d0+0.5d0*al*(xi2r*xi1r+xi2i*xi1i)/deno
      faci=     0.5d0*al*(xi2i*xi1r-xi2r*xi1i)/deno
      derikr=-tau*(gcosd*facr-gsind*faci)/tau1
      deriki=-tau*(gsind*facr+gcosd*faci)/tau1
      a(7)=facp*(deriki*vprod1(1)+derikr*vprod2(1))
      a(8)=facp*(deriki*vprod1(2)+derikr*vprod2(2))
      a(9)=facp*(deriki*vprod1(3)+derikr*vprod2(3))
      a(10)=a(7)*(tav1000**3)
      a(11)=a(8)*(tav1000**3)
      a(12)=a(9)*(tav1000**3)
c - radius of the body
      rfac=(tau+tau1)/tau1
      a(4)=-a(1)*rfac-2.d0*a(7)
      a(5)=-a(2)*rfac-2.d0*a(8)
      a(6)=-a(3)*rfac-2.d0*a(9)
c - partials d_K (a), d_R (a) ...
      a(4)=a(4)/yarkp(7)
      a(5)=a(5)/yarkp(7)
      a(6)=a(6)/yarkp(7)
      a(7)=a(7)/surcon
      a(8)=a(8)/surcon
      a(9)=a(9)/surcon
      a(10)=a(10)/surcon
      a(11)=a(11)/surcon
      a(12)=a(12)/surcon
c - spin axis components
c ... sx
      a(13)=-facp*gcosd*(xn*yarkp(1)+scalar)
      a(14)=facp*(gsind*zn-gcosd*xn*yarkp(2))
      a(15)=-facp*(gsind*yn+gcosd*xn*yarkp(3))
c ... sy
      a(16)=-facp*(gsind*zn+gcosd*yn*yarkp(1))
      a(17)=-facp*gcosd*(yn*yarkp(2)+scalar)
      a(18)=facp*(gsind*xn-gcosd*yn*yarkp(3))
c ... sz
      a(19)=facp*(gsind*yn-gcosd*zn*yarkp(1))
      a(20)=-facp*(gsind*xn+gcosd*zn*yarkp(2))
      a(21)=-facp*gcosd*(zn*yarkp(3)+scalar)
      return
      end

