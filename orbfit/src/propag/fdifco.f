* Copyright (C) 1998 by OrbFit Consortium
* Version: December 15, 1997 Steven Chesley
c =========================================
c    FDIFCO
c =========================================
c differential corrections interface routine for FITOBS
c =========INTERFACE=============================
      SUBROUTINE fdifco(iarc,obs0,ini0,ok,cov0,
     +     t0,eq0,m,objid,iobs,tau,tutm,idsta,aln,den,rmsa,rmsd,
     +     rmsmag,smag,h0,sel,nobs,
     +     rwofi0,iun20,iun8,eqc,g0,c0,csino0,delno0,succ)
      IMPLICIT NONE
c =========INPUT==============================
c arc: 1,2 arcs, 3 together
      INTEGER iarc
c data availability
      LOGICAL obs0,ini0
c initial conditions (first guess)
      DOUBLE PRECISION t0,eq0(6)
c ======observations====
c number of observations, station codes, obs. type, object id
      INTEGER m,idsta(m),iobs(m)
      CHARACTER*(*) objid(m)
c observation times (ET, UT), alpha, delta, a priori rms
      DOUBLE PRECISION tau(m),tutm(m),aln(m),den(m),rmsa(m),rmsd(m)
c magnitudes (string), a priori rms'
      CHARACTER*6 smag(m)
      DOUBLE PRECISION rmsmag(m)
c output units 
      CHARACTER*60 rwofi0
      INTEGER iun20,iun8
c ==========INPUT/OUTPUT=============================
c selection flags, no. observations used and weights
      INTEGER sel(m),nobs
      INCLUDE 'parobx.h'
c magnitude (a priori, estimate)
      DOUBLE PRECISION h0
c ========= OUTLIER REJECTION ===========
      INCLUDE 'comrej.h'
      INCLUDE 'comdif.h'
c =========OUTPUT=============================
c data available, differential correction convergent,matrices available
      LOGICAL ok,succ,cov0
c corrected orbital elements
c WARNING: if accepted, they are also copied in eq0
      DOUBLE PRECISION eqc(6)
c normal and covariance matrix (warning: not full if ncor.lt.6)
      DOUBLE PRECISION c0(6,6),g0(6,6)
c norm of residuals, of last correction
      DOUBLE PRECISION csino0,delno0
c =========END INTERFACE======================
c no. solve for variables, list, pseudoNewton/Newton
      INTEGER ncor,icor(6),inew,inter
c rms of residuals
      DOUBLE PRECISION rms,sigma
c controls for iterations to be passed to difcor
      DOUBLE PRECISION delcr
c residuals
      DOUBLE PRECISION csir(nob2x),resa(nobx),resd(nobx),x2(nobx)
      DOUBLE PRECISION w(nob2x),ratio
      PARAMETER (ratio=0.6666666d0)
      INTEGER itsav
c fit residuals, rms
      DOUBLE PRECISION resmag(nobx),rmsh
      LOGICAL radar
c data to be used to output radar reports
      INCLUDE 'iunrad.h'
c discarded observations?
      LOGICAL disc
c loop indexes, counters, modes, units
      INTEGER i,nobnew,idif,iunf
c scalar temporaries
      DOUBLE PRECISION ra,rd,recov,disca
c characters for menu 
      CHARACTER*20 menunam
      CHARACTER*70 s5,s6,s7,s8,s9,s10
c trigonometric constatnts
      INCLUDE 'trig.h'
c =====================================================================
c check availability of observations and initial condition
      CALL cheobs(obs0,ini0,ok)
      IF(.not.ok) RETURN
c =====================================================================
c check availability of JPL ephemerides and ET-UT table
      CALL chetim(tau(1),tau(m),ok)
      IF(.not.ok) RETURN
c ok, go on with arc
      IF(.not.batch)THEN
         IF(iarc.eq.1)THEN
            CALL tee(iun20,' FIRST ARC=')
         ELSEIF(iarc.eq.2)THEN
            CALL tee(iun20,' SECOND ARC=')
         ELSEIF(iarc.eq.4)THEN
            CALL tee(iun20,' BOTH ARCS TOGETHER=')
         ELSEIF(iarc.eq.0)THEN
            CONTINUE
         ELSE
            WRITE(0,*)'FDIFCO: this should not happen, iarc=',iarc
         ENDIF
      ENDIF
c =====================================================================
c choose method, how many elements to correct (interactively???)
      IF(batch)THEN
c leave autoreject control autrej as it is set 
c in options file or difini.def
c
c solve for all elements anyway
         inter=0
         itsav=itmax
         CALL whicor(inter,icor,ncor,inew)
      ELSE
         menunam='difcomod'
         CALL menu(idif,menunam,4,' select correction and reject mode=',
     +      'correct all, autoreject=',
     +      'correct all, manual reject=',
     +      'correct only some elements (reject is manual)=',
     +      'compute residuals and covariance w/o correcting=',
     +      s5,s6,s7,s8,s9,s10)
         itsav=itmax
         if(idif.eq.0)then
            ok=.false.
            return
         endif
         if(idif.eq.4)itmax=0
c auto/manual reject
         IF(idif.eq.1)THEN
            autrej=.true.
         ELSE
            autrej=.false.
         ENDIF
c which elements to correct
         IF(idif.ne.3)THEN
            inter=0
         ELSE
            inter=1
         ENDIF 
         CALL whicor(inter,icor,ncor,inew)
      ENDIF
c default iteration control parameters
      delcr=1.d-3
c weights (non-zero!)
      call fitwgt(rmsa,rmsd,den,sel,iobs,w,m,.false.)
c differential corrections
      IF(batch)THEN
         iunf=-iun20
      ELSE
         iunf=iun20
      ENDIF
      CALL difcor(m,w,sel,t0,iobs,tau,idsta,eq0,aln,den,icor,inew,
     +         iunf,delcr,eqc,g0,c0,csino0,delno0,csir,x2,succ)
      itmax=itsav
c estimate magnitude here
      IF(succ)THEN
         CALL magest(smag,rmsmag,sel,m,h0,resmag,rmsh)
      ELSE
         WRITE(0,*)' magnitude cannot be estimated w/o new orbit'
         rmsh=-1.d0
         DO i=1,m
            resmag(i)=1.d9
         ENDDO
      ENDIF
c residuals and weights written on .rwo file
      DO i=1,m
         resa(i)=csir(2*i-1)
         resd(i)=csir(2*i)
      ENDDO
c output of residuals, weights and observatiosn file
c warning: when the orbit is hyperbolic, these are the residuals of
c iteration one.
c when the divergence is mild (e.g. target function continues to increase,
c too many iterations) should be (TO BE FIXED)
      CALL wrirwo(rwofi0,objid,iobs,tutm,idsta,
     +     aln,rmsa,resa,den,rmsd,resd,smag,rmsmag,resmag,rmsh,
     +     sel,x2,m,csino0)
c Output radar residuals
      IF(.not.batch)THEN
         iunrad=iun20
         iunrare=iun20
c ELSE iunrad,iunrare is set in the main
      ENDIF
      CALL radres(iobs,tutm,resa,rmsa,resd,rmsd,sel,m,radar)
c is covariance available?
      IF(.not.succ)THEN
         cov0=.false.
      ELSE
c covariance matrix
         IF(ncor.eq.6)THEN
            cov0=.true.
         ELSE
            cov0=.false.
         ENDIF
c output covariance in .fga file, not in batch
         IF(.not.batch)THEN 
            IF(iarc.eq.1)then
               WRITE(iun8,*) 'COVARIANCE MATRIX FOR FIRST ARC'
            ELSEIF(iarc.eq.2)then
               WRITE(iun8,*) 'COVARIANCE MATRIX FOR SECOND ARC'
            ELSEIF(iarc.eq.4)THEN
               WRITE(iun8,*)'COVARIANCE MATRIX FOR BOTH ARCS'
            ENDIF
            CALL outcov(iun8,icor,g0,c0)
         ENDIF
c improved orbital elements are accepted
         CALL vcopy(6,eqc,eq0)
c observations used
         nobs=0
         DO i=1,m
           IF(sel(i).gt.0)nobs=nobs+1
         ENDDO
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c Manually discard big residuals
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         IF(.not.autrej .and. .not.batch)THEN
         rms=csino0
         WRITE(0,*)' RMS of weighed residuals is ',rms
         WRITE(0,*)' Discard residuals bigger than sigma*rms'
         WRITE(0,*)' give sigma; sigma.le.0 not to discard'
         WRITE(0,*)
         READ(*,*)sigma
         IF(sigma.gt.0.d0)THEN
            recov=sigma*rms*ratio
            disca=sigma*rms
            disc=.false.
            DO i=1,m
               ra=abs(resa(i))*sqrt(w(2*i-1))
               rd=abs(resd(i))*sqrt(w(2*i))
               IF(ra.gt.disca.or.rd.gt.disca)THEN
c discard observation
                  IF(sel(i).ne.0)THEN
                     WRITE(0,330)tau(i),idsta(i),resa(i)*secrad,
     +                    resd(i)*secrad,sel(i)
                     disc=.true.
                  ENDIF
                  WRITE(iun20,330)tau(i),idsta(i),resa(i)*secrad,
     +                 resd(i)*secrad,sel(i)
 330              FORMAT('OBS. at',f11.4,' from ',i3,
     +                 ' residuals ',2f10.3,' TO BE DISCARDED',i2) 
                  sel(i)=0
               ELSEIF(ra.lt.recov.and.rd.lt.recov)THEN
c recover previously discarded observation
                  IF(sel(i).eq.0)THEN
                     WRITE(0,340)tau(i),idsta(i),resa(i)*secrad,
     +                    resd(i)*secrad,sel(i)
                     WRITE(iun20,340)tau(i),idsta(i),resa(i)*secrad,
     +                    resd(i)*secrad,sel(i)
 340                 FORMAT('OBSERVATION at',f11.4,' from ',i3,
     +                    ' residuals ',2f13.4,' RECOVERED',i2)
                     sel(i)=1
                     disc=.true.
                  ENDIF
               ENDIF
            ENDDO
            IF(.not.disc)THEN
               WRITE(0,*)'NOTHING TO BE DISCARDED/RECOVERED'
               WRITE(0,*)' at sigma level=',sigma
            ENDIF
            nobnew=0
            DO i=1,m
               IF(sel(i).gt.0)nobnew=nobnew+1
            ENDDO
            WRITE(0,*)'OBSERVATIONS USED=',nobs,' TO BE USED=',nobnew
         ENDIF
         ENDIF
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c End Manual discard
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ENDIF
      RETURN
      END
c ==========================================================
c Output radar residuals
      SUBROUTINE radres(iobs,tutm,resa,rmsa,resd,rmsd,
     +     sel,m,radar)
      IMPLICIT NONE
c number of observations, obs. type,
      INTEGER m,iobs(m),sel(m)
c observation times, residuals, a priori rms
      DOUBLE PRECISION tutm(m),resa(m),resd(m),rmsa(m),rmsd(m)
c radar data flag
      LOGICAL radar
c scalar temporaries, indexes
c radar residuals
      DOUBLE PRECISION rsum,rrsum,rwsum,rrwsum,hour
      INTEGER ii,rcnt,rrcnt,iday,imonth,iyear
c unit for output; if positive, include standard output
      INTEGER iunp
      LOGICAL radrange,radrate
c needed to use au
      INCLUDE 'jplhdr.h'
c data to be used to output radar reports
      INCLUDE 'iunrad.h'
c ===========================================================
      rsum=0d0
      rrsum=0d0
      rwsum=0d0
      rrwsum=0d0
      rcnt=0
      rrcnt=0
      radar=.false.
      radrange=.false.
      radrate=.false.
      do ii=1,m
c     if radar...
         if(iobs(ii)/1000.eq.2)then
            radar=.true.
c convert time
               CALL mjddat(tutm(ii),iday,imonth,iyear,hour)
c     range:               
            if(mod(iobs(ii),100).ne.3)then
c write residual in range
               radrange=.true.
               WRITE(iunrare,100)namast,iyear,imonth,iday,hour,
     +              resa(ii)*au,rmsa(ii)*au,sel(ii)
 100  FORMAT(a9,1x,i4,1x,i2,1x,i2,1x,'1',1x,f5.2,1x,f11.5,1x,f9.5,1x,i1)
               rsum=rsum+(resa(ii)/rmsa(ii))**2
               rwsum=rwsum+(1d0/rmsa(ii))**2
               rcnt=rcnt+1
            endif
c     range rate:              
            if(mod(iobs(ii),100).ne.2)then
               radrate=.true.
c write residual in range rate
               WRITE(iunrare,101)namast,iyear,imonth,iday,hour,
     +              resd(ii)*au,rmsd(ii)*au,sel(ii)
 101  FORMAT(a9,1x,i4,1x,i2,1x,i2,1x,'2',1x,f5.2,1x,f11.5,1x,f9.5,1x,i1)
               rrsum=rsum+(resd(ii)/rmsd(ii))**2
               rrwsum=rrwsum+(1d0/rmsd(ii))**2
               rrcnt=rrcnt+1
            endif
         endif
      enddo
      iunp=abs(iunrad)
      IF(radrange)THEN 
         IF(iunrad.gt.0)THEN
            WRITE (*,456)
     +           'range  RMS      =',sqrt(rsum/rwsum)*au,' km',
     +           'range weight =',sqrt(1d0/rwsum*rcnt)*au,' km'
         ENDIF
         WRITE (iunp,456)
     +        'range        =',sqrt(rsum/rwsum)*au,' km',
     +        'range weight =',sqrt(1d0/rwsum*rcnt)*au,' km'
 456     FORMAT(a,f10.5,a/a,f10.5,a)
      ENDIF
      IF(radrate)THEN
         IF(iunrad.gt.0)THEN
         WRITE(0,457)
     +           'range rate RMS   =',sqrt(rrsum/rrwsum)*au,' km/day',
     +           'range rate wt=',sqrt(1d0/rrwsum*rrcnt)*au,' km/day'
         ENDIF
         WRITE (iunp,456) 
     +        'range rate   =',sqrt(rrsum/rrwsum)*au,' km/day',
     +        'range rate wt=',sqrt(1d0/rrwsum*rrcnt)*au,' km/day'
 457     FORMAT(a,f10.5,a/a,f10.5,a/)
      ENDIF
      RETURN
      END



