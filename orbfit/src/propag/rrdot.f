c ==================================
c R R D O T
c
c radar observations
      subroutine rrdot (east,iobs,t0,tr,ioc,r,v,drde,dvde,ider,twobo)
      implicit none
c =============INPUT====================
c asteroid equinoctal elements 
      double precision east(6)
c observation type (used for surface bounce correction)
      integer iobs
c times: epoch time for asteroid elements, observation time (MJD)
      double precision t0,tr
c observatory code
      integer ioc
c flag to control computation of derivatives
      integer ider
c flag to control two-body approximation: if .false., full n-body
      logical twobo
c ============OUTPUT====================
c observations: range and range rate in AU, AU/day
      double precision r,v
c partial derivatives of range and range w.r. to asteroid coordinates
      double precision drde(6),dvde(6)
c =============END INTERFACE=========================================
c cartesian coordinates of the Earth, of the asteroid, id. at receive time
      double precision xea(6),xast(6),xastr(6)
c first partial derivatives of r, rdot, w.r. to ast. coordinates, vel.
      double precision drdx(6),dvdx(6)
c asteroid radius (for surface bounce correction)
      double precision rb
c station codes
      INTEGER iotr,iore
c station positions: geocentric, heliocentric
      double precision xre(3),yre(3),xtr(3),ytr(3)
      double precision rre(3),vre(3),rtr(3),vtr(3)
c difference vector, distance, velocity difference, size
      double precision rhorv(3),rhor,rhordv(3),rhord
      double precision rhotv(3),rhot,rhotdv(3),rhotd
      double precision vsize,prscal,prscag
c solar system barycentric velocities (for 1/c^2 Doppler level)
      double precision vressb(3),vastssb(3),vtrssb(3)
      double precision vressb2,vtrssb2
c down leg time, bounce time
      double precision taud,tb,tbold
c up leg time, transmit time
      double precision tauu,tt,ttold
c delta tau correction function
      double precision deltau
c speed of light
      INCLUDE 'vlight.h'
c radius of asteroid
      include 'radius.h'
c iteration index, conrol and flag for the position/velocity subroutine
      INTEGER i,itmax,ifla
      PARAMETER (itmax=10)
c control on convergence set at 0.05 microseconds (Yeomans et al. AJ 1992)
      DOUBLE PRECISION ept
      PARAMETER (ept=6.d-13)
c time scale
      double precision tretdb,temp,tdiffr,tdifft
      INCLUDE 'sunmass.h'
c for AU
      INCLUDE 'jplhdr.h'
c first derivatives of cartesian coordinates with respect to elements
      double precision dxde(6,6)
c second derivatives (dummy),planet coords (dummy)
      double precision ddxde(3,6,6),xdummy(6)
c auxiliary scalars for Doppler GR & tropospheric corrections
      double precision rgeo,rgeod,deldoptr,deldopre,rast,rastd
      double precision rsta,rstad,scal1,scal2,scal3,levelc
c =================================================
c two body approx must not be used
      IF(twobo)STOP'radar and two body approximations incompatible'
c deal with surface vs. mass center return:
      if(iobs-2000.ge.100)then
         rb=radius
         if(rb.le.0)then
           write(0,*)  '**** rrdot: internal error', radius
           stop
         endif
      else
c no correction is needed
         rb=0
      endif
c ======================================
c Displacement of the stations with respect to the center of the Earth
c find codes of two observatories
      iotr=ioc/10000
      iore=ioc-iotr*10000
      ifla=1
c compute position of receiver at time tr
      call pvobs2(tr,iore,xre,yre)
c =======================================
c get receive time in TDB
      call times(tr+2400000.5,temp,tdiffr)
      tretdb=tr+tdiffr/86400.d0
c Compute down leg time
c Orbit propagation at time tr
      call propag(t0,east,tretdb,xastr,xea,ider,dxde,ddxde)
c ... new initial conditions are xastr
c receive station position at time tr 
      CALL vsumg(3,xea,xre,rre)
      CALL vsumg(3,xea(4),yre,vre)
c initial guess is bounce time(position) equal to receive time(position)
      tb=tretdb
      do i=1,6
         xast(i)=xastr(i)
      enddo
c Loop on down leg
      i=0
 11   CONTINUE
         i=i+1
         CALL vdiff(xast,rre,rhorv)
         rhor=vsize(rhorv)      
         taud=(rhor-rb)/vlight + deltau(xast,xre,rhorv,rre)
c convergence control
         tbold=tb
         tb=tretdb-taud
         IF(abs(tb-tbold).lt.ept) GOTO 9
c Orbit propagation at time tb
         CALL rkg(tretdb,xastr,xastr(4),-taud,xast,xast(4),xdummy)
         if(i.lt.itmax)GOTO 11
c too many iterations
      write(99,*)' slow convergence on down leg time ',tb-tbold
c compute relative velocity between asteroid and receiver
 9    CONTINUE
      CALL vdiff(xast(4),vre,rhordv)
      rhord=prscal(rhorv,rhordv)/rhor
c solar velocity at the receive and bounce epochs to get the asteroid
c barycentric velocity (vastssb) and the receiver barycentric velocity 
c (vressb)
      ifla=2
c - receive
      CALL earcar(tretdb,xea,ifla)
      CALL vsumg(3,vre,xea(4),vressb)
      vressb2=vressb(1)*vressb(1)+vressb(2)*vressb(2)+
     +        vressb(3)*vressb(3)
c - bounce
      CALL earcar(tb,xea,ifla)
      CALL vsumg(3,xast(4),xea(4),vastssb)
c =======================================
c compute upleg time
c fist guess is up leg time = down leg time
      ifla=1
      tt=tb-taud
c Loop on upleg time
      i=0
 21   CONTINUE
         i=i+1
c compute transmitter position at estimated transmit time tt
c call the Earth-rotation model in TDT
         call times(tt+2400000.5,temp,tdifft)
         CALL earcar(tt-(tdifft/86400.d0),xea,ifla)
         CALL pvobs2(tt,iotr,xtr,ytr)
         CALL vsumg(3,xea,xtr,rtr)
c upleg time
         CALL vdiff(xast,rtr,rhotv)
         rhot=vsize(rhotv)
         tauu=(rhot-rb)/vlight + deltau(xast,xtr,rhotv,rtr)
c convergence control
         ttold=tt
         tt=tb-tauu
         IF(abs(tt-ttold).lt.ept) GOTO 19
      if(i.lt.itmax)GOTO 21
c too many iterations
      write(99,*)' slow convergence on up leg time ',tt-ttold
 19   CONTINUE
c compute relative velocity between asteroid and transmitter
      CALL vsumg(3,xea(4),ytr,vtr)
      CALL vdiff(xast(4),vtr,rhotdv)
      rhotd=prscal(rhotv,rhotdv)/rhot
c ==========================================================
c compute distance
      r=0.5d0*(tauu+taud+(tdifft-tdiffr)/86400.d0)*vlight
c compute relative frequency shift (up to 1/c^3 level);
c solar velocity at the bounce epochs and the asteroid barycentric
c velocity (vastssb)
      ifla=2
      CALL earcar(tt,xea,ifla)
      CALL vsumg(3,vtr,xea(4),vtrssb)
      vtrssb2=vtrssb(1)*vtrssb(1)+vtrssb(2)*vtrssb(2)+
     +        vtrssb(3)*vtrssb(3)
      levelc=rhotd+rhord
      scal1=prscal(rhotv,vtrssb)/rhot/vlight
      scal2=prscal(rhorv,vastssb)/rhor/vlight
      scal3=0.5d0*(vtrssb2-vressb2)+
     +      gms*((1.d0/vsize(rtr))-(1.d0/vsize(rre)))
      v=0.5d0*(levelc+rhotd*scal1*(1.d0+scal1)
     +               -rhord*scal2*(1.d0-scal2)
     +               -(rhotd*rhord*(1.d0+scal1-scal2)
     +               -scal3*(1.d0-(levelc/vlight)))/vlight)
c - get GR and tropospheric corrections to Doppler
c -- GR stuff
      rast=dsqrt(xast(1)*xast(1)+xast(2)*xast(2)+xast(3)*xast(3))
      rastd=(xast(1)*xast(4)+xast(2)*xast(5)+xast(3)*xast(6))/rast
c a) upleg piece
      rsta=dsqrt(rtr(1)*rtr(1)+rtr(2)*rtr(2)+rtr(3)*rtr(3))
      rstad=(rtr(1)*vtr(1)+rtr(2)*vtr(2)+rtr(3)*vtr(3))/rsta
      call deldop1(rast,rastd,rhot,rhotd,rsta,rstad,deldoptr)
c b) downleg piece
      rsta=dsqrt(rre(1)*rre(1)+rre(2)*rre(2)+rre(3)*rre(3))
      rstad=(rre(1)*vre(1)+rre(2)*vre(2)+rre(3)*vre(3))/rsta
      call deldop1(rast,rastd,rhor,rhord,rsta,rstad,deldopre)
      v=v-0.5d0*(deldoptr+deldopre)
c -- troposheric stuff
c a) at transmit passage -->
      rgeo=vsize(xtr)
      rgeod=prscal(xtr,ytr)/rgeo
      call deldop2(xast,xtr,ytr,rgeo,rgeod,rhotv,rhotdv,rhot,rhotd,
     . deldoptr)
c b) at receive passage <--
      rgeo=vsize(xre)
      rgeod=prscal(xre,yre)/rgeo
      call deldop2(xast,xre,yre,rgeo,rgeod,rhorv,rhordv,rhor,rhord,
     . deldopre)
      v=v-0.5d0*(deldoptr+deldopre)
c rem interplanetary environment effects (e^- plasma) neglected
c ==========================================================
c Derivatives
      IF(ider.eq.0)RETURN
c derivs of r,rdot wrt cartesian
      do i=1,3
c d(range)/d(r)
         drdx(i)=(rhotv(i)/rhot +rhorv(i)/rhor)/2.d0
c d(range)/d(v) = 0
         drdx(i+3)=0
c d(range-rate)/d(r)
         dvdx(i)=-((rhotd*rhotv(i)/rhot-rhotdv(i))/rhot + 
     +             (rhord*rhorv(i)/rhor-rhordv(i))/rhor)/2.d0
c d(range-rate)/d(v)= c * d(range)/d(r)
         dvdx(i+3)=drdx(i)
      enddo
c derivs of cartesian with respect to equinoctal elements
      do i=1,6
        drde(i)=prscag(6,drdx,dxde(1,i))
        dvde(i)=prscag(6,dvdx,dxde(1,i))
      enddo
      RETURN
      END

c ======================================================================
c DELTAU - "small" corrections to radar time of flight
c ======================================================================
c     xast - asteroid position, heliocentric
c     xsta - station position, relative to Earth center
c     rho - asteroid position, relative to station
c     r - station position, heliocentric
      double precision function deltau(xast,xsta,rho,r)
      implicit none
      double precision xast(3),xsta(3),rho(3),r(3)
      include 'sunmass.h'
      include 'vlight.h'
      double precision vsize,prscag
      double precision rsta,e,p,q,cosz,cotz,deltau1,deltau2
c     double precision sinha,sin2ha,fghz,ampli,finte1,fun1,fun2
c     double precision aprim,bprim,deltau3,omeg1,alpha
c ================================
c Relativistic delay
      e=vsize(r)
      p=vsize(xast)
      q=vsize(rho)
      deltau1=2d0*gms*log(abs((e+p+q)/(e+p-q)))/vlight**3
c Earth ionospheric/tropospheric delay
c ref. EM Standish, A&A 233, 252 (1990)
      rsta=vsize(xsta)
      if(rsta.lt.1d-12)then
         write(99,*)'deltau: radar station at geocenter!'
         cosz=1d0-1d-8
      else
         cosz=prscag(3,xsta,rho)/rsta/q
      endif
      if(cosz.eq.1d0)cosz=cosz-1d-8
      cotz=cosz/sqrt(1d0-cosz**2)
      deltau2=(7d-9/86400d0)/(cosz+1.4d-3/(4.5d-2+cotz))
c Interplanetary medium propagation effect
c ref. EM Standish, A&A 233, 252 (1990) with constants of DE118
c rem. a more precise model might be needed here; e.g.
c      Muhleman & Anderson, ApJ 247, 1093 (1981) or newer
c      alpha=q/e
c      cosha=(r(1)*rho(1)+r(2)*rho(2)+r(3)*rho(3))/e/q
c      sin2ha=1.d0-cosha*cosha
c      sinha=dsqrt(sin2ha)
c X- or S-band;
cc !!! Information about the frequency is not passed here at the
cc     moment; it should be decided manually !!!
cc      fghz=2.38d0
c      fghz=8.51d0
c      ampli=(2.01094d-8)/fghz/fghz/e/86400.d0
c      aprim=1.237265d-6
c      bprim=9.524021d0
c      finte1=(datan((alpha+cosha)/sinha)-datan(cosha/sinha))/sinha
c      fun1=bprim*finte1
c      omeg1=1.d0+alpha*(2.d0*cosh+alpha)
c      fun2=aprim*(0.25d0*((alpha+cosha)*((1.d0/omeg1)+(1.5d0/sin2ha))
c     .           /omeg1-cosha*(1.d0+(1.5d0/sin2ha)))/sin2h
c     .           +0.375d0*finte1/sin2ha/sin2ha)/(e**4)
c      deltau3=ampli*(fun1+fun2)
c Add 'em up
cc      deltau=deltau1+deltau2+deltau3
      deltau=deltau1+deltau2
      return
      end

c ======================================================================
c DELDOP1 - "small" corrections to radar-rate measurements
c ======================================================================
      subroutine deldop1(p,pdot,q,qdot,e,edot,deldop)
      implicit none
      double precision p,pdot,q,qdot,e,edot,deldop
      include 'sunmass.h'
      include 'vlight.h'
      double precision brac1,brac2
c ================================
c relativistic range-rate correction
      brac1=q*(edot+pdot)+qdot*(e+p)
      brac2=((e+p)**2)-(q**2)
      deldop=4.d0*gms*brac1/brac2/(vlight**2)
      return
      end

c ======================================================================
c DELDOP2 - "small" corrections to radar-rate measurements
c ======================================================================
c     xast(6) - asteroid r & v heliocentric
c     xtr(3),ytr(3) - station r & v relative to Earth center
c     rsta,drsta - |xtr| & d|xtr|/dt
c     rhov(3),drhov(3) - asteroid r & v relative to station
c     rho,drho - |rhov| & d|rhov|/dt
      subroutine deldop2(xast,xsta,vsta,rsta,drsta,rhov,drhov,rho,drho,
     . deldop)
      implicit none
      double precision xast(6),xsta(3),vsta(3),rhov(3),drhov(3)
      double precision rsta,drsta,rho,drho,deldop
      include 'vlight.h'
      double precision cosz,sinz,cotz,dcoszdt,phiz
      double precision brac,brac1,brac2,scal1,scal2
c ================================
c rate of change of the Earth ionospheric/tropospheric ~ Doppler shift
      if(rsta.lt.1d-12)then
         write(99,*)'deltau: radar station at geocenter!'
         cosz=1d0-1d-8
      else
         cosz=(rhov(1)*xsta(1)+rhov(2)*xsta(2)+rhov(3)*xsta(3))/rsta/rho
      endif
      sinz=sqrt(1.d0-cosz**2)
      cotz=cosz/sinz
      brac=0.045d0+cotz
      brac1=1.d0-(0.0014d0/brac/brac/(sinz**3))
      brac2=cosz+(0.0014d0/brac)
      phiz=-vlight*(7.d-9/86400.d0)*brac1/(brac2**2)
      scal1=drhov(1)*xsta(1)+drhov(2)*xsta(2)+drhov(3)*xsta(3)
      scal2=rhov(1)*vsta(1)+rhov(2)*vsta(2)+rhov(3)*vsta(3)
      dcoszdt=((scal1+scal2)/rho/rsta)-cosz*((drho/rho)+(drsta/rsta))
      deldop=phiz*dcoszdt
      return
      end







