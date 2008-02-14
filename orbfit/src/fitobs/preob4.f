c =====================================================================
c PREOB4- virtual impactor negative observation skyhole
c ============INTERFACE===================================================
      SUBROUTINE preob4(t0,idsta,t1,eq,h,g,gameq,
     +    ceq,v6,sigma,npo,ibv,inl,al,de,hmagv,elm,
     +    alpha,delta,hmagn,gamad,sig,axes,npo1)
      IMPLICIT NONE      
c ============= input ====================================================
c elements and epoch times, covariance and normal matrices at t0,
c sigmas for the boundary
      DOUBLE PRECISION eq(6),t0,t1,gameq(6,6),ceq(6,6),sigma
c number of points, flag for confidence bd/line of variation, nonlinearity
      INTEGER npo,ibv,inl
c magnitude
      DOUBLE PRECISION h,g,hmagn
c station code
      INTEGER idsta
c 6x6 rotation matrix giving the reference system adapted to the target plane
      DOUBLE PRECISION v6(6,6)
c ============= output ===================================================
c points on the confidence boundary (difference w.r. to alpha,delta)
c WARNING! the output number of points is npo1.le.npo; 
c this beacuse hyperbolic points are discarded
      INCLUDE 'npoint.h'
      INTEGER npo1
      DOUBLE PRECISION al(npo),de(npo),hmagv(npo)
c line of elements
      DOUBLE PRECISION elm(6,npo)
c best fit observations
      DOUBLE PRECISION alpha,delta
c covariance
      DOUBLE PRECISION gamad(2,2),axes(2,2),sig(2)
c ============END INTERFACE===============================================
c workspace
      DOUBLE PRECISION tmp(6,6),daddelt(6,2),v6t(6,6)
c partials in new reference system
      DOUBLE PRECISION daddelt4(4,2),g4(4,4),gamv(6,6),c4(4,4),cv(6,6)
      DOUBLE PRECISION ws(4)
      INTEGER ii,jj,indp
c partial derivatives of alpha, delta, w.r. to elements (by columns)
      DOUBLE PRECISION daddet(6,2),dummy(6)
c second derivatives of alpha, delta, w.r. to elements (not used)
      DOUBLE PRECISION ddade(6,6),dddde(6,6)
c ===================================================================
c orthonormal basis, matrix defining the plane of the ellipse
      DOUBLE PRECISION v(4,4),ceicel(2,2)
c transformation matrix between the two planes
      DOUBLE PRECISION b(2,2)
c number of full revolutions around the sky
      INTEGER ng,nrev
c functions
      DOUBLE PRECISION appmag,prscag
c elongation,distance to Earth, distance to Sun (to compute magnitude)
      INCLUDE 'phase.h'
      DOUBLE PRECISION adot0,ddot0 
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
c compute observation; derivatives (of order 1) required            
      CALL alfdel (eq,t0,t1,idsta,alpha,delta,daddet(1,1),daddet(1,2),
     +        1,twobo,ddade,dddde)
c store proper motion
      adot0=adot
      ddot0=ddot
c compute derivatives in the reference system adapted to the target plane
      CALL transp(v6,6,6,v6t)
      CALL mulmat(v6t,6,6,daddet,6,2,daddelt)
c     WRITE(1,*)'daddet ', daddet
c     WRITE(1,*)'daddelt ',daddelt
c     WRITE(1,*)'v6 ',v6
c compute covariance matrix in the new reference system
      CALL mulmat(v6t,6,6,gameq,6,6,tmp)
      CALL mulmat(tmp,6,6,v6,6,6,gamv)
      CALL mulmat(v6t,6,6,ceq,6,6,tmp)
      CALL mulmat(tmp,6,6,v6,6,6,cv)
c reduce all to 4-d
      DO jj=1,4
        DO ii=1,4
c          g4(jj,ii)=gamv(jj+2,ii+2)
          c4(jj,ii)=cv(jj+2,ii+2)
        ENDDO
        DO ii=1,2
          daddelt4(jj,ii)=daddelt(jj+2,ii)
        ENDDO
      ENDDO
      CALL tchinv(c4,4,g4,ws,indp)
c     WRITE(1,*)'daddelt4 ',daddelt4
c     WRITE(1,*)'c4 ',c4
c     WRITE(1,*)'g4 ',g4
c compute ellipse of covariance of alpha,delta
      CALL ellip4(daddelt4,g4,sig,axes,gamad)
c     write(1,*)' sig,axes,gamad ',sig,axes,gamad
c use nonlinear method
      IF(ibv.eq.0)THEN
         maxsig=max(sig(1),sig(2))
         minsig=min(sig(1),sig(2))
         if(maxsig/minsig.le.200.d0)then
            ibv=1
         else
            ibv=2
         endif
      endif    
c =====================================================================
c compute ellipse in the elements space 
      CALL slinel4(daddelt4,g4,c4,ceicel,b,v)
c     WRITE(1,*)'ceicel,b,v4 ',ceicel,b,v
c ===========================================================
c compute line of orbital elements
      CALL linobs4(ibv,npo,eq,axes,sig,b,v,sigma,ceicel,elm,v6,npo1)
c     WRITE(1,*)' npo1',npo1
c     DO jj=1,npo1
c        WRITE(1,*)'elm,jj=',jj,(elm(ii,jj),ii=1,6)
c     ENDDO
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
           write(0,*)' inl=2 is forbidden'
           npo1=0
           RETURN
        ELSEIF(inl.eq.3)THEN
c full n-body propagation from ellipse 
           CALL vsumg(6,eq,elm(1,n),elm(1,n))
           CALL alfdel (elm(1,n),t0,t1,idsta,al(n),de(n),
     +          dummy,dummy,0,twobo,ddade,dddde)
           al(n)=al(n)-alpha
           de(n)=de(n)-delta
c          write(0,*)n,al(n),de(n)
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
           WRITE(0,*)' preobn: this we have not invented yet ', inl
           RETURN           
        ENDIF
c keep count of lost revolutions
        IF(n.eq.1)THEN
           IF(al(n).gt.pig)al(n)=al(n)-dpig
        ELSE
           CALL angupd(al(n),al(n-1),ng)
        ENDIF
c temporary output
c       write(0,*)'Solution ',n,', RA/DEC (deg)',
c    +       al(n)*degrad,de(n)*degrad,ng
 7    continue
c =====================================================================
c ensure that LOV is consistent with nominal point
c first find midpoint of LOV, assume npo is even
      if(ibv.eq.2)then
         nrev=nint((al(npo/2)+al(npo/2+1))/2.d0/dpig)
c        write(0,*)'debug: nrev:',nrev
         if(nrev.ne.0)then
            do n=1,npo1
               al(n)=al(n)-nrev*dpig
            enddo
         endif
      endif
c restore original apparent motion
      adot=adot0
      ddot=ddot0
      RETURN
      END







