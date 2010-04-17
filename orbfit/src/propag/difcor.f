* Copyright (C) 1998 by OrbFit Consortium
c ===================================================================
c DIFCOR  differential corrector
c ===================================================================
c version 1.8 Steven Chesley, Dec. 15, 1998
c        works on equinoctal elements: 
c        a,h=e sin(long.peri),k=e cos(long.peri),
c        p=tg I/2 sin(long.node),q=tg I/2 cos(long.node), lambda=mean long.
c Input: m observations number
c        w weights (only diagonal matrix)
c        sel selection flags
c        t0 epoch time for asteroid elements
c        iobs type of observations 1=alpha,delta 2=r,rdot
c        tau observations time vector
c        ioco station codes vector
c        eq0 asteroid equinoctal elements at time t0 (first guess)
c        al,de real observations vectors
c        icor(6) flags .ne.0 to correct this element, 0 to leave it as in eq0 
c        inew =2 for pseudo-Newton (use this), =1 for Newton (not well tested)
c        iunf = unit file for output; if <0, no residuals output
c        itmax=max. no iterations, if =0 then only calculate covariance...
c        itgmax= max no iterations with target function paralysed/increasing
c        delcr=control for stop due to small corrections
c        divrat=control for paralyzed target function
c Output eq corrected orbital elements at time t0
c        gamma covariance matrix
c        gtwg inverse of covariance matrix
c           warning: if only some elements are corrected, only the 
c              corresponding entries in gamma and gtwg are nonzero 
c        csinor residuals norm
c        delnor differential corrections norm
c        csir residuals (in radians) 
c                r.a. first, then declination, for each obs
c        x2 chi**2 value for each observation
c        succ logical success flag
c =============INTERFACE===== =========================================
      SUBROUTINE difcor(m,w,sel,t0,iobs,tau,ioco,eq0,al,de,icor,inew,
     +     iunf,delcr,eq,gamma,gtwg,csinor,delnor,csir,x2,succ)
c =====================================================================
      IMPLICIT NONE
c include files
      INCLUDE 'parobx.h'
      INCLUDE 'codode.h'
      INCLUDE 'trig.h'
      INCLUDE 'comdif.h'
      INCLUDE 'comrej.h'
c ================input data==========================
c no. observations, observatory codes, selection flag, obs. type 
      INTEGER m, ioco(m), sel(m), iobs(m)
c  times, alpha, delta, 
      DOUBLE PRECISION tau(m),al(m),de(m)
c weights
      DOUBLE PRECISION w(2*m)
c unit file to write output
      INTEGER iunf
c controls 
      INTEGER inew,icor(6)
c  corr. and residuals  controls
      DOUBLE PRECISION delcr
c epoch time, initial equinoctal elements 
      DOUBLE PRECISION t0, eq0(6)
c ================output ==========================
c corrected equinoctal elements 
      DOUBLE PRECISION eq(6)
c normal and covar. matrix
      DOUBLE PRECISION gtwg(6,6),gamma(6,6)
c success flag
      LOGICAL succ
c  corr. and residuals norm
      DOUBLE PRECISION delnor,csinor
c residuals
      DOUBLE PRECISION csir(nob2x)
c =============END INTERFACE============================================
c input data sorted, in scalar form 
      DOUBLE PRECISION tsort(nobx),als(nobx),des(nobx)
      INTEGER iocos(nobx),iposs(nobx),sels(nobx),iobsrt(nobx)
c weights,no observations used (w.ne.0)
      DOUBLE PRECISION ws(nob2x),wsrej(nob2x),wsec(nob2x)
c chi**2 rms for each obs
      DOUBLE PRECISION x2s(nobx),x2(nobx)
c residuals, condition number,  scalar residuals
      DOUBLE PRECISION csi(nob2x),ra,rd
c residuals norm: previous and first (zeroth) iterations
      DOUBLE PRECISION csino1,csinor0
c differential correction, eccentricity,scaling
      INTEGER nused,nsolv
      DOUBLE PRECISION ecc,mu,rescov
c loop variables: it iteration number, itg=iter. with increase of q,
c ittodo= itmax, but forced to 1 for itmax=0 (no correction)
      INTEGER it,itg,itrej
c loop indexes: j=1,m; i,k=1,6
      INTEGER j,k,i
c Flag for only computing covariance, not changing orbit.
      LOGICAL matonly
c unit for output
      INTEGER iun
c outlier rejection
      INTEGER nmod
c magnitude data
      include 'mag.h'
c function for control of bizarre orbit, deserving to give up
      LOGICAL bizarre
c ====================================================================
c ================== INITIALIZATION ==================================
c ====================================================================
c Are commons loaded?
      IF(iicdif.ne. 36) STOP 'difcor: internal error(1)'
      IF(iicrej.ne. 36) STOP 'difcor: internal error(2)'
c sort of times and reordering of obs. and weights
      CALL srtoss(t0,iobs,tau,al,de,ioco,sel,w,m,iobsrt,tsort,iposs,
     +   als,des,iocos,sels,ws)
c count number of solve-for variables
      nsolv=0
      DO  j=1,6
        IF(icor(j).ne.0)nsolv=nsolv+1
      ENDDO
c create weight vector with zeros for rejected obs
      DO i=1,m
         IF(sels(i).eq.0)THEN
             wsrej(2*i-1)=0
             wsrej(2*i)=0
         ELSE
             wsrej(2*i-1)=ws(2*i-1)
             wsrej(2*i)=ws(2*i)
         ENDIF
      ENDDO
c Initialisation with starting value for elements
      DO  k=1,6
        eq(k)=eq0(k)
      ENDDO
      iun=abs(iunf)
      IF(iunf.gt.0)THEN
         write(99,220) eq
         write(iun,220) eq
 220     format(' starting values'/6f13.7)
      ENDIF
c norm of corrections for "zeroth" iteration
      csino1=1.0d25
c control for covariance only pass
      matonly=.false.
      IF(itmax.eq.0)THEN
         itmax=1
         matonly=.true.
      ENDIF
c ====================================================================
c ================== BEGIN OUTLIER LOOP ==============================
c ====================================================================
      DO itrej=1,itmaxr
c counter for increasing/paralyzed iterations
         itg=0
c +++++++++++++++++++++ Begin Inner Loop +++++++++++++++++++++++++++++
         DO it=1,itmax
c ================== single iteration ==============================
            CALL sincor(m,iobsrt,tsort,als,des,iocos,t0,icor,inew,
     +           wsrej,iun,matonly,eq,delnor,csi,csinor,gamma,gtwg)
c save first pass (sorted) data
c These will be passed out in case of hyperbolicity
            IF(it.eq.1 .and. itrej.eq.1)THEN
               CALL unsort(iposs,m,2*m,csi,csir,sel,sels,tsort,tau)
               csinor0=csinor
c cleanup the chi2
               DO j=1,m
                  x2(j)=0.d0
               ENDDO
            ENDIF
c for computation of covariane without correction, there is no rejection
c and no failure possible
            IF(matonly)THEN
               succ=.true.
               IF(iunf.gt.0)THEN
                  write(99,*)'One pass only. No corrections.'
                  write(iun,*)
     +                 'One pass only. No corrections applied.'
               ENDIF
               GOTO 70
            ENDIF
c control against hyperbolic and bizarre orbits
            IF(bizarre(eq))THEN
               ecc=sqrt(eq(2)**2+eq(3)**2)
               write(99,*)' bizarre; e=',ecc,' a=',eq(1)
               write(iun,*)' bizarre; e=',ecc,' a=',eq(1)
               succ=.false.
c     return saved rms
               csinor=csinor0
               return
            endif
c norm of the correction: the zeros DO not matter!
            IF(iunf.gt.0)THEN
               write(99,200)it,csinor,delnor,eq
               write(iun,200)it,csinor,delnor,eq
 200          format(' *** iteration ',i3/,'  RMS residuals =',1p,d12.4,
     +           '   norm corr =',d12.4/,'  new elem values'/0p,6f13.7/)
            ENDIF
c ====== DECISION #1 ===================================================
c Check if we need another iteration
c Small corrections to elements?
            IF(delnor.lt.delrej)THEN
               succ=.true. 
               GOTO 77
            ENDIF
c Target function increasing/paralyzed?
            IF(csinor.gt.csino1*1.1d0)THEN
               itg=itg+1
               IF(iunf.gt.0)write(99,*)' target function increasing '
               IF(iunf.gt.0)write(iun,*)' target function increasing '
               write(99,*)
               write(iun,*)
               succ=.false.
            ELSEIF(csinor.gt.csino1*divrat)THEN
               itg=itg+1
               IF(iunf.gt.0)THEN
                  write(99,*)' target function paralyzed '
                  write(iun,*)' target function paralyzed '
                  write(99,*)
                  write(iun,*)
               ENDIF
               succ=.true.
            ELSE
               itg=0
            ENDIF
            IF(itg.gt.itgmax)THEN
c              go to outlier rejection if paralyzed
               IF(succ) GOTO 77
c              otherwise (increasing) exit with failure
               GOTO 70
            ENDIF
            csino1=csinor
c +++++++++++++++++++++ End Inner Loop +++++++++++++++++++++++++++++
         ENDDO
         succ=.false.
         IF(iunf.gt.0)THEN
            write(99,*)' too many iterations'
            write(iun,*)' too many iterations'
         ENDIF
c           Must get computed orbit and selection flags to agree:
         if(autrej) call sincor(m,iobsrt,tsort,als,des,iocos,t0
     +        ,icor,inew,
     +        wsrej,iun,matonly,eq,delnor,csi,csinor,gamma,gtwg)
         GOTO 70
c ================== AUTOMATIC OUTLIER REJECTION =====================
 77      CONTINUE
         IF(autrej)THEN
         IF(iunf.gt.0)write(99,*)'REJECTING...'
c call with -iun to avoid logging rejection stats
            CALL reject(iunf,csinor,sels,csi,tsort,x2s,ws,m,
     +        gamma,icor,nmod)
c update weight vector with zeros for rejected obs
            DO i=1,m
                IF(sels(i).eq.0)THEN
                    wsrej(2*i-1)=0
                    wsrej(2*i)=0
                ELSE
                    wsrej(2*i-1)=ws(2*i-1)
                    wsrej(2*i)=ws(2*i)
                ENDIF
            ENDDO
            IF(iunf.gt.0)THEN
               write(99,201)itrej,it,nmod
               write(iun,201)itrej,it,nmod
 201           format('Outlier rejection pass ',i3,
     +         ' after ',i3,' single differential correction passes.'/
     +              'There were ',i3,' changes.')
            ENDIF
         ELSE
            DO i=1,m
               x2s(i)=0.d0
            ENDDO
         ENDIF
c go to next loop if no modifications or if not rejecting anyway
         IF(nmod.eq.0 .or. .not.autrej)THEN
            GOTO 75
         ENDIF
c ================== END OF OUTLIER LOOP ==============================
      ENDDO
c ====================================================================
c ================== BEGIN FINAL LOOP ================================
c ====================================================================
 75   CONTINUE
c      write(99,*)delnor,delcr,succ
      IF(delnor.gt.delcr)THEN
         IF(iunf.gt.0)THEN
            write(99,*)'Final convergence loop after ',
     +           itrej,' outlier rejection loops.'
            write(iun,*)'Entering final convergence loop after ',
     +           itrej,' outlier rejection loops.'
         ENDIF
c counter for increasing/paralyzed iterations
         itg=0
         DO it=1,itmax
c ================== single iteration ==================================
            CALL sincor(m,iobsrt,tsort,als,des,iocos,t0,icor,inew,wsrej,
     +           iun,matonly,eq,delnor,csi,csinor,gamma,gtwg)
c control against hyperbolic and bizarre orbits
            IF(bizarre(eq))THEN
               ecc=sqrt(eq(2)**2+eq(3)**2)
               write(99,*)' bizarre; e=',ecc,' a=',eq(1)
               write(iun,*)' bizarre; e=',ecc,' a=',eq(1)
               succ=.false.
c cleanup the chi2
               DO j=1,m
                  x2(j)=0.d0
               ENDDO
c the residuals of the first iteration will be passed up
               RETURN
            ENDIF
c norm of the correction: the zeros DO not matter!
            write(99,200)it,csinor,delnor,eq
            write(iun,200)it,csinor,delnor,eq
c ====== DECISION # 2 ===============================================
c Check if we need another iteration
c Small corrections to elements?
            IF(delnor.lt.delcr)THEN
               IF(iunf.gt.0)THEN
                  write(99,*)  
     +                 'Done. Convergence corrections small after ',
     +                 it,' passes.'
               write(iun,*)'Done. Convergence corrections small after ',
     +              it,' passes.'
                  write(99,*)
                  write(iun,*)
               ENDIF
               succ=.true.
               goto 70
            ENDIF
c     Target function increasing/paralyzed?
            IF(csinor.gt.csino1*1.1d0)THEN
               itg=itg+1
               IF(iunf.gt.0)THEN
                  write(99,*)' target function increasing '
                  write(iun,*)' target function increasing '
                  write(99,*)
                  write(iun,*)
               ENDIF
               succ=.false.
            ELSEIF(csinor.gt.csino1*divrat)THEN
               itg=itg+1
               IF(iunf.gt.0)THEN
                  write(99,*)' target function paralyzed '
                  write(iun,*)' target function paralyzed '
                  write(99,*)
                  write(iun,*)
               ENDIF
               succ=.true.
            ELSE
               itg=0
            ENDIF
            IF(itg.gt.itgmax)THEN
               IF(iunf.gt.0)write(99,*) 
     +              'Done. Target funct. not decreasing after ',
     +              it,' passes.'
               write(iun,*)'Done. Target funct. not decreasing after ',
     +              it,' passes.'
               goto 70
            ENDIF
            csino1=csinor
c ====================== END FINAL LOOP =============================
         ENDDO
         succ=.false.
         IF(iunf.gt.0)write(99,*)' too many iterations'
         write(iun,*)' too many iterations'
      ENDIF
c ====================== CLEAN UP AND RETURN ========================
 70   CONTINUE
c covariance (and normal matrix) rescaling
      nused=0
      DO i=1,2*m
         IF(ws(i).GT.0.d0) nused=nused+1
      ENDDO
      IF(nsolv .gt. nused)  STOP 'difcor: internal error, nsolv>nused'
c To be done better .... maybe done
      mu=rescov(nsolv,nused,csinor)
      DO  i=1,6
         DO  j=1,6
            gamma(i,j)=gamma(i,j)*mu**2
            gtwg(i,j)=gtwg(i,j)/mu**2
         ENDDO
      ENDDO
c reordering the residuals for output
      CALL unsort(iposs,m,2*m,csi,csir,sel,sels,tsort,tau)
c unsort x2 and dmagn, dsunv,disv,phav,adotv,ddotv
c (data for magnitude compoutation)
      do i=1,m
         j=iposs(i)
         x2(j)=x2s(i)
         dsuna(j)=dsunas(i)
         disa(j)=disas(i)
         phaa(j)=phaas(i)
         dmagn(j)=dmagns(i)
         adotmv(j)=adots(i)
         ddotmv(j)=ddots(i)
      enddo
c Output: residuals at convergence
c     IF(iunf.gt.0)THEN
c        write(iunf,*) 'Res. in arcsec:'
c        write(iunf,112)
c112     format('      MJD         alpha    w_alpha',
c    +        '      delta    w_delta    chi     obs   sel')
c        DO 88 j=1,m
c           ra=csir(2*j-1)/radsec
c           rd=csir(2*j)/radsec
c           wsec(2*j-1)=w(2*j-1)*(radsec**2)
c           wsec(2*j)=w(2*j)*(radsec**2)
c           write(iunf,111)tau(j),ra,wsec(2*j-1),rd,wsec(2*j),
c    +           sqrt(x2(j)),ioco(j),sel(j) 
c111        format(2x,f11.5,1x,f12.6,f8.4,1x,f12.6,f8.4,
c    +           f10.4,2x,i4,2x,i2)
c88      continue
c     ENDIF
c=====================
      return
      end
