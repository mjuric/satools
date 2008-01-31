c =====================================================================
c PREOBN- version with confidence region, nonlinear theory
c =====================================================================
c  input:  coo   = coordinate type EQU, KEP, CAR
c          t0    = epoch time (MJD)
c          idsta = station code
c          t1    = prediction time (MJD)
c          east0 = orbital elements vector at time t0
c          h     = absolute magnitude
c          g     = opposition effect coefficient
c          gamm0 = covariance at time t0 for elements east0
c          c0    = normal matrix at time t0 (should be the inverse of gamm0)
c          sigma = level of the confidence boundary in RMS values
c          npo   = number of points in the boundary
c          ibv   = Type of depiction
c                       =0 for automatic selection
c                       =1 for confidence boundary
c                       =2 for line of maximum variation
c          inl   = handling of nonlinearity
c                       =0 for automatic selection
c                       =1 for linear ellipse
c                       =2 for 2-Body nonlinear propagation of covariance
c                       =3 for n-body nonlinear propagation of covariance
c          iob1  = observation type:
c                        1000's = RA/DEC, 
c                        2000's=R/RDOT, 
c
c  output: alpha = right ascension (equatorial J2000), radians
c          delta = declination (equatorial J2000), radians
c          hmagn = apparent magnitude, as predicted, from h and g given
c          gamad = covariance matrix of observations alpha, delta
c          sig   = sqrt(eigenvalues) of gamad
c          axes  = the eigenvectors of gamad are the columns of this matrix
c          npo1  = number of output dta points 9could be less than npo)
c          al(npo1),de(npo1) points on the confidence boundary  
c                   (difference with respect to best prediciton, radians)
c          elm(npo1) alternate elements for observation time
c
c  In the linear approximation, the ellipse of confidence has principal axes
c          along axes; the semiaxes lenghts are sig
c  In the nonlinear approximation, the boundary is a map of the
c          confidence ellipse in the elements space 
c
c  WARNING: the magnitudes are often very poorly predictable
c ============INTERFACE===================================================
      SUBROUTINE preobn(coo,t0,idsta,t1,east0,h,g,gamm0,
     +    c0,sigma,npo,ibv,inl,iob1,al,de,hmagv,elm,
     +    alpha,delta,hmagn,gamad,sig,axes,npo1)
      IMPLICIT NONE      
c ============= input ====================================================
c elements and epoch times, covariance and normal matrices at t0,
c sigmas for the boundary
      character*3 coo
      DOUBLE PRECISION east0(6),t0,t1,gamm0(6,6),c0(6,6),sigma
c number of points, flag for confidence bd/line of variation, nonlinearity
      INTEGER npo,ibv,inl,iob1
c magnitude
      DOUBLE PRECISION h,g,hmagn
c station code
      INTEGER idsta
c ============= output ===================================================
c points on the confidence boundary (difference w.r. to alpha,delta)
c WARNING! the output number of points is npo1.le.npo; 
c this beacuse hyperbolic points are discarded
      INCLUDE 'npoint.h'
      INCLUDE 'jplhdr.h'
      INTEGER npo1
      DOUBLE PRECISION al(npo),de(npo),hmagv(npo),allin,delin
c line of elements
      DOUBLE PRECISION elm(6,npo)
c best fit observations
      DOUBLE PRECISION alpha,delta
c covariance
      DOUBLE PRECISION gamad(2,2),axes(2,2),sig(2)
c ============END INTERFACE===============================================
c asteroid equinoctal elements, mean motion, covariance, normal matrices
      DOUBLE PRECISION eq(6),enne,gameq(6,6),ceq(6,6)  
c jacobian matrices of partial derivatives
      DOUBLE PRECISION dedeq(6,6),dedeqt(6,6),deqde(6,6),deqdet(6,6)
c return elements, workspace
      DOUBLE PRECISION east00(6),tmp(6,6)
c partial derivatives of alpha, delta, w.r. to elements (by columns)
      DOUBLE PRECISION daddet(6,2),dummy(6)
c second derivatives of alpha, delta, w.r. to elements (not used)
      DOUBLE PRECISION ddade(6,6),dddde(6,6)
c ===================================================================
c orthonormal basis, matrix defining the plane of the ellipse
      DOUBLE PRECISION v(6,6),ceicel(4,2)
c transformation matrix between the two planes
      DOUBLE PRECISION b(2,2)
c number of full revolutions around the sky
      INTEGER ng,nrev
c functions
      DOUBLE PRECISION appmag,prscag
c elongation,distance to Earth, distance to Sun (to compute magnitude)
      INCLUDE 'phase.h'
      DOUBLE PRECISION adot0,ddot0,pha0,dis0,dsun0,elov0,galla0
c ===================================================================
c constant of gravitation, trigonometric constants 
      INCLUDE 'sunmass.h'
      INCLUDE 'trig.h'
c temporaries, indexes
      DOUBLE PRECISION dal,ddl,maxsig,minsig
      INTEGER n
c flag for 2-body approximation; must be .false. for full n-body computation
      LOGICAL twobo
      twobo=.false.
c****************
c   static memory not required
c****************
c =====================================================================
      if(iob1/1000.eq.2.and.inl.eq.2)then
         WRITE(*,*)' preobn: mixing of radar and two-body '//
     +        'approximation not permitted'
         RETURN      
      endif
c coordinate change
      CALL cooder(east0,coo,gms,eq,'EQU',enne,deqde) 
c and inverse
      CALL cooder(eq,'EQU',gms,east00,coo,enne,dedeq)
c compute normal and covariance matrix for equinoctal elements
      CALL mulmat(deqde,6,6,gamm0,6,6,tmp)
      CALL transp(deqde,6,6,deqdet)
      CALL mulmat(tmp,6,6,deqdet,6,6,gameq)
      CALL transp(dedeq,6,6,dedeqt)
      CALL mulmat(dedeqt,6,6,c0,6,6,tmp)
      CALL mulmat(tmp,6,6,dedeq,6,6,ceq)
c =====================================================================
c compute observation; derivatives (of order 1) required            
c     write(*,*)'preobn calls alfdel', t0,t1
      if(iob1/1000.eq.1)then
         CALL alfdel (eq,t0,t1,idsta,alpha,delta,daddet(1,1),
     +        daddet(1,2),1,twobo,ddade,dddde)
      elseif(iob1/1000.eq.2)then
         CALL rrdot (eq,iob1,t0,t1,idsta,alpha,delta,daddet(1,1),
     +        daddet(1,2),1,twobo)
      ELSE
         WRITE(*,*)' preobn: this observation type not supported ',iob1
         RETURN           
      ENDIF
c store true apparent motion, etc.
      adot0=adot
      ddot0=ddot
      pha0=pha
      dis0=dis
      dsun0=dsun
      elov0=elo
      galla0=gallat
c compute apparent magnitude at time of observation
      hmagn=appmag(h,g,dsun,dis,pha)
c =====================================================================
c compute ellipse of covariance of alpha,delta
      CALL ellips(daddet,gameq,sig,axes,gamad)
c If inl=0 then use automatic selection method
      IF(inl.eq.0)THEN
         maxsig=max(sig(1),sig(2))*degrad
         if(maxsig.le.1.0d0)then
            inl=1
c Is it safe to use two body if we may have close approach?
c         elseif(maxsig .le. 5.d0)then
c            inl=2
         else
            inl=3
         endif
      endif            
c If inl=0 then use automatic selection method
      IF(ibv.eq.0)THEN
         maxsig=max(sig(1),sig(2))
         minsig=min(sig(1),sig(2))
         if(maxsig/minsig.le.200.d0)then
            ibv=1
         else
            ibv=2
         endif
      endif    
      if(inl.eq.2)then
c 2-body aproximation for the central point
         CALL alfdel (eq,t0,t1,idsta,allin,delin,dummy,dummy,
     +        0,.true.,ddade,dddde)
      endif
c =====================================================================
c compute ellipse in the elements space 
      CALL slinel(daddet,gameq,ceq,ceicel,b,v)
c ===========================================================
c compute line of orbital elements
      CALL linobs(ibv,npo,eq,axes,sig,b,v,sigma,ceicel,elm,npo1)
c ===========================================================
      ng=0
      DO 7 n=1,npo1
c chose method to handle nonlinearity
        IF(inl.eq.1)THEN
c linear map from ellipse
           dal=prscag(6,elm(1,n),daddet(1,1))
           ddl=prscag(6,elm(1,n),daddet(1,2))
           al(n)=dal
           de(n)=ddl
           CALL vsumg(6,eq,elm(1,n),elm(1,n))
c apparent magnitude is the one of the nominal orbit
           hmagv(n)=hmagn
        ELSEIF(inl.eq.2)THEN
c 2-body propagation from ellipse
           CALL vsumg(6,eq,elm(1,n),elm(1,n))
           CALL alfdel (elm(1,n),t0,t1,idsta,al(n),de(n),
     +          dummy,dummy,0,.true.,ddade,dddde)
c compute apparent magnitude at time of observation
           hmagv(n)=appmag(h,g,dsun,dis,pha)
c difference is with respect to 2-body approx., used w.r. to true orbit
           al(n)=al(n)-allin
           de(n)=de(n)-delin
        ELSEIF(inl.eq.3)THEN
c full n-body propagation from ellipse 
           CALL vsumg(6,eq,elm(1,n),elm(1,n))
           CALL proele('EQU',t0,elm(1,n),t1,elm(1,n))
           if(iob1/1000.eq.1)then
              CALL alfdel (elm(1,n),t1,t1,idsta,al(n),de(n),
     +             dummy,dummy,0,twobo,ddade,dddde)
           elseif(iob1/1000.eq.2)then
              CALL rrdot (elm(1,n),iob1,t1,t1,idsta,al(n),de(n),
     +             dummy,dummy,0,twobo)
           ELSE
              stop'preobn: internal error'
           ENDIF
           al(n)=al(n)-alpha
           de(n)=de(n)-delta
c other prediction data stored in common
           phav(n)=pha
           disv(n)=dis
           dsunv(n)=dsun
           elov(n)=elo
           gallav(n)=gallat
           adotv(n)=adot
           ddotv(n)=ddot
c compute apparent magnitude at time of observation
           hmagv(n)=appmag(h,g,dsun,dis,pha)
        ELSE
           WRITE(*,*)' preobn: this we have not invented yet ', inl
           RETURN           
        ENDIF
c keep count of lost revolutions
        IF(n.eq.1)THEN
           IF(al(n).gt.pig)al(n)=al(n)-dpig
        ELSE
           CALL angupd(al(n),al(n-1),ng)
        ENDIF
c temporary output
        if(iob1/1000.eq.1)then
           write(*,*)n,', RA/DEC (deg)',al(n)*degrad,de(n)*degrad,ng
        elseif(iob1/1000.eq.2)then
           write(*,*)n,', R/RDOT (km,km/day)',al(n)*au,de(n)*au,ng
        endif
 7    continue
c =====================================================================
c ensure that LOV is consistent with nominal point
c first find midpoint of LOV, assume npo is even
      if(ibv.eq.2)then
         nrev=nint((al(npo/2)+al(npo/2+1))/2.d0/dpig)
         write(*,*)'debug: nrev:',nrev
         if(nrev.ne.0)then
            do n=1,npo1
               al(n)=al(n)-nrev*dpig
            enddo
         endif
      endif
c restore original apparent motion
      adot=adot0
      ddot=ddot0
      pha=pha0
      dis=dis0
      dsun=dsun0
      elo=elov0
      gallat=galla0
      RETURN
      END
