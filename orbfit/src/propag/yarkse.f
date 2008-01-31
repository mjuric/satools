c ******************************************************************
      subroutine yarkse(xast,vast,a,iparti)
c ******************************************************************
c
c This subroutine computes the heliocentric components of the
c Yarkovsky thermal acceleration -- the seasonal variant only.
c If the flag (iparti) is set to 1, one gets at the output also
c partials wrt to some parameters of the thermal model (if these
c are desired to be adjusted).
c
c Input parameters:
c -----------------
c
c - via header   iparti  ... partials (yes=1/no=0)
c                (xast,vast) ... state vector of the asteroid
c - via common   yarkp(1-3) ... sx, sy, sz (unit vector of the
c                               body's spin axis orientation)
c                yarkp(4-5) ... k_0 and k_1 parameters of the
c                               surface thermal conductivity
c                               [K(T) = k_0 + k_1 T_av^3]
c                yarkp(6) ... density of the surface layer
c                yarkp(7) ... radius of the body
c                yarkp(8) ... rotation frequency
c                yarkp(9) ...  surface absorptivity
c                + some more precomputed useful variables
c
c Output parameters: a(1-3) ... seasonal acceleration
c ------------------ a(4-6) ... partials wrt the radius of the body
c                    a(7-9) ... partials wrt the thermal conductivity
c
c REM. PARTIALS ARE DISABLED AT THIS MOMENT
c
c SI units are assumed throughout the subroutine, but the results
c (e.g. accelerations) are given in AU and days.
c
c Written by: D. Vokrouhlicky, Oct 99
c (queries to vokrouhl@mbox.cesnet.cz)
c ..................................................................
      implicit double precision (a-h,o-z)
      parameter (napprox=7)
      parameter (densityb=2.d3,capacity=680.d0,dsqrt2=1.414213562373d0)
      parameter (emiss=0.9d0,clight3=8.99377374d8,aceuni=0.049900176d0)
      dimension xast(3),vast(3)
      dimension brac(7),bras(7),gcosd(7),gsind(7),a(21)
      INCLUDE 'sunmass.h'
      INCLUDE 'trig.h'
      INCLUDE 'yarkov.h'
c -----------------------------------------------------------------------
c - thermal inertia & seasonal thermal parameter
       bgama=dsqrt(yarkp(4)*yarkp(6)*capacity)
       theta=bgama*thfacya/emiss
       seadepth=dsqrt(yarkp(4)/yarkp(6)/capacity/fmeaya)
c - radius of the body scaled by the depth of the seasonal wave
       rp=yarkp(7)/seadepth
       rp2=dsqrt2*rp
       tau=theta*etaya75/rp2
       tau1=1.d0+tau
c - amplitude of the effect
       fac=aceuni*yarkp(9)*radfluya/yarkp(7)/densityb/clight3/tau1
c - G_k cos(d_k) & G_K sin(d_k) functions computed
       do 10 k=1,napprox
        fk=dfloat(k)
        alk=dsqrt(fk)*rp2
c - the auxiliary functions A-D, a,b
        cal=dcos(alk)
        sal=dsin(alk)
        if (alk.lt.90.d0) then
         ealm=dexp(-alk)
        else
         ealm=0.d0
        endif
        af=3.d0*(alk+2.d0)*ealm+(3.d0*(alk-2.d0)*cal+alk*(alk-3.d0)*sal)
        bf=alk*(alk+3.d0)*ealm+(-alk*(alk-3.d0)*cal+3.d0*(alk-2.d0)*sal)
        caf=-(alk+2.d0)*ealm+(-(alk-2.d0)*cal+alk*sal)
        cbf=-alk*ealm-(alk*cal+(alk-2.d0)*sal)
        ccf=caf+tau*af/tau1
        cdf=cbf+tau*bf/tau1
c - G exp(i delta)
        deno=ccf*ccf+cdf*cdf
        gcosd(k)=(caf*ccf+cbf*cdf)/deno
        gsind(k)=(cbf*ccf-caf*cdf)/deno
c compute cos- & sin-related brackets
        brac(k)=spya*alya(k)*gcosd(k)+sqya*beya(k)*gsind(k)
        bras(k)=sqya*beya(k)*gcosd(k)-spya*alya(k)*gsind(k)
10     continue 
c mean anomaly detremined
c - approximated by a linear term only
c      anomaly=ele0(6)+(fmea*t)
c - computed from the state vector
      r2=xast(1)*xast(1)+xast(2)*xast(2)+xast(3)*xast(3)
      v2=vast(1)*vast(1)+vast(2)*vast(2)+vast(3)*vast(3)
      rdot=xast(1)*vast(1)+xast(2)*vast(2)+xast(3)*vast(3)
      r=dsqrt(r2)
      aaxi=1.d0/(2.d0/r-(v2/gms))
      esinu=rdot/dsqrt(aaxi*gms)
      ecosu=(r*v2/gms)-1.d0
      uano=datan2(esinu,ecosu)
      anomaly=uano-esinu
      if (anomaly.lt.0.d0) anomaly=anomaly+dpig
c compute the sum...
      fact=0.d0
      do 100 k=napprox,1,-1
       fk=dfloat(k)
       canomaly=dcos(fk*anomaly)
       sanomaly=dsin(fk*anomaly)
       fact=fact+(brac(k)*canomaly+bras(k)*sanomaly)
100   continue
      fact=fact*fac
c seasonal acceleration (~ factor * {\bf s})
      a(1)=fact*yarkp(1)
      a(2)=fact*yarkp(2)
      a(3)=fact*yarkp(3)
c Partials? -- DISABLED AT THE MOMENT
c      if (iparti.eq.0) return
c - general
c      cafp=-ealm+cal+(2.d0*al-1.d0)*sal
c      cbfp=-ealm-(2.d0*al-1.d0)*cal+sal
c      afp=3.d0*ealm+(al*al-3.d0)*cal+(al*(al-4.d0)+3.d0)*sal
c      bfp=(2.d0*al+3.d0)*ealm-(al*(al-4.d0)+3.d0)*cal
c     .     +(al*al-3.d0)*sal
c - thermal conductivity parameters (k_0,k_1)
c      xi1r=caf*ccf-cbf*cdf
c      xi1i=cbf*ccf+caf*cdf
c      xi2r=cafp*af-cbfp*bf
c      xi2i=cbfp*af+cafp*bf
c      xi2r=xi2r-caf*afp+cbf*bfp
c      xi2i=xi2i-cbf*afp-caf*bfp
c      deno=xi1r*xi1r+xi1i*xi1i
c      facr=1.d0+0.5d0*al*(xi2r*xi1r+xi2i*xi1i)/deno
c      faci=     0.5d0*al*(xi2i*xi1r-xi2r*xi1i)/deno
c      derikr=-tau*(gcosd*facr-gsind*faci)/tau1
c      deriki=-tau*(gsind*facr+gcosd*faci)/tau1
c      a(7)=fac*(deriki*vprod1(1)+derikr*vprod2(1))
c      a(8)=fac*(deriki*vprod1(2)+derikr*vprod2(2))
c      a(9)=fac*(deriki*vprod1(3)+derikr*vprod2(3))
c      a(10)=a(7)*(tav1000**3)
c      a(11)=a(8)*(tav1000**3)
c      a(12)=a(9)*(tav1000**3)
c - radius of the body
c      rfac=(tau+tau1)/tau1
c      a(4)=-a(1)*rfac-2.d0*a(7)
c      a(5)=-a(2)*rfac-2.d0*a(8)
c      a(6)=-a(3)*rfac-2.d0*a(9)
c - partials d_K (a), d_R (a) ...
c      a(4)=a(4)/yarkp(7)
c      a(5)=a(5)/yarkp(7)
c      a(6)=a(6)/yarkp(7)
c      a(7)=a(7)/surcon!!!!!! --> yarkp(4)
c      a(8)=a(8)/surcon
c      a(9)=a(9)/surcon
c      a(10)=a(10)/surcon
c      a(11)=a(11)/surcon
c      a(12)=a(12)/surcon
c - spin axis components
      return
      end

 

