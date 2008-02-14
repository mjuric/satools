c Copyright 1998, The Orbfit Consortium 
c Modified Jan 25 199 by A. Milani to estimate magnitudes
c To correct for new calls to: sortoss,unsort,fitwgt
c ===================================================================
c DIFVIN  linearly constrained differential corrector (single step)
c ===================================================================
c version 1.4.12, A. Milani, November 24, 1997
c        works on equinoctal elements: 
c        a,h=e sin(long.peri),k=e cos(long.peri),
c        p=tg I/2 sin(long.node),q=tg I/2 cos(long.node), lambda=mean long.
c Input: m observations number
c        w weights (only diagonal matrix)
c        sel selection flag
c        tau observations time vector
c        iobs observation type
c        ioco station codes vector
c        t0 epoch time for asteroid elements
c        eq0 asteroid equinoctal elements at time t0 (first guess)
c        al,de real observations vectors
c        peq vector to which deq must be orthogonal   
c        gmag G magnitude par for magnitude estimate     
c        iunf = unit file for output; if <0, no residuals output
c Output eq corrected orbital elements at time t0
c        gamma covariance matrix
c        gtwg inverse of covariance matrix
c           warning: if only some elements are corrected, only the 
c              corresponding entries in gamma and gtwg are nonzero 
c        csinor residuals norm
c        delnor differential corrections norm
c        csir residuals (in radians) 
c                r.a. first, then declination, for each obs
c        succ success flag; the solution is meaningful if succ=.true.
c ============= REMARK ===============================================
c The weights are constant in this routine
c =============INTERFACE===== =========================================
      subroutine difvin(m,w,sel,iobs,tau,ioco,t0,eq0,al,de,peq,gmag,
     +        iunf,eq,gamma,gtwg,csinor,delnor,csir,nused,succ)
c =====================================================================
      IMPLICIT NONE
c include files
      INCLUDE 'parobx.h'
      INCLUDE 'trig.h'
      INCLUDE 'restart.h'
c elongation,distance to Earth, distance to Sun (to compute magnitude)
      INCLUDE 'phase.h'
c magnitude data
      INCLUDE 'mag.h'
      DOUBLE PRECISION appmag
c ================input data==========================
c no. observations, observatory codes, obs. type, selection flags
      INTEGER m, ioco(m), iobs(m),sel(m)
c  times, alpha, delta, 
      DOUBLE PRECISION tau(m),al(m),de(m)
c weights
      DOUBLE PRECISION w(2*m)
c unit file to write output
      INTEGER iunf
c epoch time, initial equinoctal elements, orthogonal vector, G 
      DOUBLE PRECISION t0, eq0(6),peq(6),gmag
c ================output ==========================
c corrected equinoctal elements 
      DOUBLE PRECISION eq(6)
c normal and covar. matrix
      DOUBLE PRECISION gtwg(6,6),gamma(6,6)
c  corr. and residuals norm
      DOUBLE PRECISION delnor,csinor
c no obs, used 
      INTEGER nused
c residuals
      DOUBLE PRECISION csir(nob2x)
c success flag
      LOGICAL succ
c =============END INTERFACE============================================
c input data sorted, in scalar form 
      DOUBLE PRECISION tsort(nobx),als(nobx),des(nobx)
      integer iocos(nobx),iposs(nobx),iocj,no
      DOUBLE PRECISION tauj,alj,dej
c weights
      DOUBLE PRECISION ws(nob2x),wsec(nob2x)
c for sorting routine 
      INTEGER selsrt(nobx),iobsrt(nobx)
c residuals, condition number,  scalar residuals
      DOUBLE PRECISION csi(nob2x),cond,ra,rd
c ====================================================================
c first and second derivatives of alpha, delta w.r. to elements
      DOUBLE PRECISION dade(6),ddde(6),ddade(6,6),dddde(6,6)
c first and second derivatives of observations
      DOUBLE PRECISION g(nob2x,6),h(nob2x,6,6),gr(nob2x,6)
c  corr. and residuals norm, their controls
      integer inew,ider,icor(6),icor6(6),nsolv,itmax,itgmax
      DOUBLE PRECISION delcr,divrat,csino1,mu,rescov,gam(6,6),c(6,6)
c differential correction, eccentricity
      DOUBLE PRECISION deq(6),ecc
c iteration indexes
      integer it,itg
c matrices for coordinate change
      DOUBLE PRECISION v(6,6),deqv(6)
c loop indexes: j=1,m; i,k=1,6
      integer j,k,i
c DOUBLE PRECISION functions
      DOUBLE PRECISION snorm,snormd
c control of two body approximation (must be false)
      logical twobo
c function for control of bizarre orbit, deserving to give up
      LOGICAL bizarre
c unit for output
      integer iun
c ====================================================================
c number of solve-for variables
      icor(1)=0
      do  i=2,6
        icor(i)=1
      enddo
      do  i=1,6
        icor6(i)=1
      enddo
c ====================================================================
c assign ider depending from inew
      inew=2
      ider=1
c full n-body
      twobo=.false.
c sort of times and reordering of obs. and weights
      call srtoss(t0,iobs,tau,al,de,ioco,sel,w,m,iobsrt,tsort,
     +   iposs,als,des,iocos,selsrt,ws)
c Initialisation with starting value for elements
      do  k=1,6
        eq(k)=eq0(k)
      enddo
c ====================================================================
      iun=abs(iunf)
      if(iunf.gt.0)then
         write(0,220) eq
         write(iun,220) eq
 220     format(' starting values'/6f13.7)
      endif
c ================== main loop ==============================
c iteration control parameters 
      itmax=80
      itgmax=75
      delcr=1.d-3
      divrat=0.99d0 
c Loop on iterations of NEWTON's METHOD
      itg=0
      do 60 it=1,itmax
c ================== iteration ==============================
c Compute observations and derivatives
         restar=.true.
         do 61 j=1,m
            tauj=tsort(j)
            iocj=iocos(j)
            twobo=.false.
            IF(iobsrt(j)/1000.eq.1)THEN
               call alfdel(eq,t0,tauj,iocj,alj,dej,dade,ddde,ider,twobo,
     +              ddade,dddde)
c  compute magnitude difference (apparent minus absolute)
               dmagns(j)=appmag(0.d0,gmag,dsun,dis,pha)
            ELSEIF(iobsrt(j)/1000.eq.2)THEN
               CALL rrdot(eq,iobsrt(j),t0,tauj,iocj,alj,dej,dade,ddde,
     +              ider,twobo)
            ELSE
               WRITE(0,*)'difvin: iobs= ',iobsrt(j), ' not known'
               STOP
            ENDIF
            restar=.false.
c Compute residuals, form matrix g=-d(csi)/d(eq)
            csi(2*j-1)=als(j)-alj
            csi(2*j)=des(j)-dej
            DO i=1,6
               g(2*j-1,i)=dade(i)
               g(2*j,i)=ddde(i)
            ENDDO
 61      continue 
         restar=.true.
c ===========linear constraint============================
c find orthonormal basis with peq as first vector
         call graha1(peq,6,v)
c convert derivatives
         call mulmat(g,nob2x,6,v,6,6,gr)
c ===========one differential corrections step=================
c Compute solution of linear least squares
         no=2*m
         call minsol(csi,no,ws,gr,h,inew,icor,iun,
     +        c,deqv,gam,cond)
c convert elements
         call mulmav(v,6,6,deqv,6,deq)
c Update solution (only solve-for variables) 
         do  k=1,6
            if(icor(k).ne.0)then
               eq(k)=eq(k)+deq(k)
            endif
         enddo
c ================ end iteration =========================
c Convergence control: norm of the residuals
         csinor=snormd(csi,ws,no,nused)
c reordering the residuals for output
         call unsort(iposs,m,no,csi,csir,selsrt,sel,tsort,tau)   
c unsort dmagn
      do i=1,m
         j=iposs(i)
         dmagn(j)=dmagns(i)
      enddo
c norm of the correction: the zeros do not matter!
         delnor=snorm(deqv,c,6,6)
         if(iunf.gt.0)then
            write(0,200)it,csinor,delnor,eq
            write(iun,200)it,csinor,delnor,eq
 200        format(' *** iteration ',i3,' RMS residuals =',1p,d12.4,
     +        '   norm corr =',d12.4,'  new elem values:'/0p,6f13.7/)
         endif
c control against hyperbolic and bizarre orbits
         IF(bizarre(eq))THEN
            ecc=sqrt(eq(2)**2+eq(3)**2)
            write(0,*)' bizarre; e=',ecc,' a=',eq(1)
            write(iun,*)' bizarre; e=',ecc,' a=',eq(1)
            succ=.false.
            return
         endif
c Check if we need another iteration
         if(delnor.lt.delcr)then
            if(iunf.gt.0)then
               write(0,*)' convergence corrections small'
               write(iun,*)' convergence corrections small'
            endif
            succ=.true.
            goto 70
         endif
         if(it.gt.1)then
            if(csinor.gt.csino1*1.1d0)then
               itg=itg+1
               if(iunf.gt.0)then
                  write(0,*)' target function increasing '
                  write(iun,*)' target function increasing '
               endif
               succ=.false.
            elseif(csinor.gt.csino1*divrat)then
               succ=.true.
               if(iunf.gt.0)then
                  write(0,*)' target function paralyzed '
                  write(iun,*)' target function paralyzed '
               endif
               itg=itg+1
            endif
            if(itg.gt.itgmax)then
                goto 70
            endif
         endif
         csino1=csinor
 60   continue
      succ=.false.
 70   continue
      IF(.not.succ) WRITE(0,*)' non convergent ',it,itg
c ====================== stop iterations ======================
c Output: residuals at convergence
c     if(iunf.gt.0)then
c        write(iun,*) 'Res. in arcsec:'
c        write(iun,112)
c112  format('      MJD            alpha      w_alpha',
c    + '         delta      w_delta')
c        do 88 j=1,m
c          ra=csir(2*j-1)/radsec
c          rd=csir(2*j)/radsec
c          wsec(2*j-1)=w(2*j-1)*(radsec**2)
c          wsec(2*j)=w(2*j)*(radsec**2)
c          write(iun,111)tau(j),ra,wsec(2*j-1),rd,wsec(2*j) 
c111       format(2x,f11.5,1x,f14.8,f12.8,1x,f14.8,f12.8)
c88      continue
c     endif
c =====================compute covariance matrix for last iteration
c Compute solution of linear least squares
      no=2*m
      call minsol(csi,no,ws,g,h,inew,icor6,iun,
     +                gtwg,deqv,gamma,cond)
c covariance (and normal matrix) rescaling
      nsolv=6
      mu=rescov(nsolv,nused,csinor)
c apply scaling to both covariance and normal matrix
      DO  i=1,6
         DO  j=1,6
            gamma(i,j)=gamma(i,j)*mu**2
            gtwg(i,j)=gtwg(i,j)/mu**2
         ENDDO
      ENDDO
c Convergence control: norm of the residuals
      write(0,201)it,csinor,delnor
      write(iun,201)it,csinor,delnor
 201  format(' iterations ',i3,'  RMS=',1p,d12.4,' last corr. =',d12.4)
c =====================
      return
      end











