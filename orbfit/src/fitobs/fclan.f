c =======================================
c  FCLAN
c vers. 2.0.1, A. Milani, November 27, 1999
c =======================================
c close approach analysis
c ===============INTERFACE========================
      SUBROUTINE fclan(batchcl,t1,eq0,t0,gc,cc,csinor,delnor,iun20,ok,
     +     hmag,gmag,obs0,m,objid,iobs,tau,tutm,idsta,aln,den,
     +     rmsa,rmsd,sel,astna0)
      IMPLICIT NONE
c ================INPUT===========================
c control for batch mode
      LOGICAL batchcl
c time to monitor up to, passed only in batch mode
      DOUBLE PRECISION t1
c magnitude
      DOUBLE PRECISION hmag,gmag
c asteroid names (18 CHARACTERs)
      CHARACTER*18 astna0
c ======observations====
c data availability
      LOGICAL obs0
c number of observations, station codes, obs. type, object id
      INTEGER m,idsta(m),iobs(m)
      CHARACTER*(9) objid(m)
c selection flags
      INTEGER sel(m)
c observation times (ET, UT), alpha, delta, a priori rms
      DOUBLE PRECISION tau(m),tutm(m),aln(m),den(m),rmsa(m),rmsd(m)
c =======orbit==============
c nominal initial conditions: epoch, elements
      DOUBLE PRECISION t0,eq0(6)
c normal and covariance matrices
      DOUBLE PRECISION gc(6,6),cc(6,6)
c norms of residuals, of last correction
      DOUBLE PRECISION csinor,delnor
c output unit
      INTEGER iun20
c ===============OUTPUT=========================
c success flag
      LOGICAL ok
c ==============END INTERFACE=====================
c final elements on output
      DOUBLE PRECISION eq1(6),tcl
c arrays of close approach data and controls
      INCLUDE 'cloapp.h'
      INCLUDE 'closapl.h'
c ======== time spans for JPL data etc. =========
      INCLUDE 'timespan.h'
c ======== close approach data =======================
c nominal close approach time
      DOUBLE PRECISION tcl0
c time interval including the close approach under study
      DOUBLE PRECISION tcl1,tcl2
c workspace for changed orbital elements
      DOUBLE PRECISION eqc(6),tc
c to continue 
      DOUBLE PRECISION t1old
      INTEGER icont
      DOUBLE PRECISION de1de0(6,6)
c dirty trick to force restart anyway
c      include 'restart.h'
c control of derivatives
      INTEGER ide
      COMMON/deriv/ide
c ======== basis adapted to MTP =================
      DOUBLE PRECISION vt3(3,3),vsize
c relative positions at close approach
      DOUBLE PRECISION xc0(3),vc0(3)
c target plane coordinates: cartesian, derivatives (transposed)
      DOUBLE PRECISION tpc(2),dtpdet(6,2)
c computation of target ellipse
      DOUBLE PRECISION axes(2,2),sig(2)
c correspondent ellipse in orbital lements space
      DOUBLE PRECISION b(2,2),ceicel(4,2),v(6,6)
c ====== multiple target plane intersections============
c string of virtual asteroids
      INTEGER npox
      PARAMETER (npox=4000)
      DOUBLE PRECISION elm(6,npox)
c confidence boundary, line of max variation
      INTEGER ibv,inl,npo,npo1
      DOUBLE PRECISION sigma,maxsig,minsig
c ======================================================
c **************************************************
c temporary... to be modified when Newton's method is added
c target plane type
      INTEGER iclan
c logical controls
      LOGICAL error
c close approach parameters
      DOUBLE PRECISION dmin0,vmin0,tcros
c newton controls
      DOUBLE PRECISION dminew,tpr
c **************************************************
c newton method controls
      INTEGER itnew,itnewma,ikey
      DOUBLE PRECISION newcont
c menu empty items
      CHARACTER*20 menunam
      CHARACTER*70 s3,s4,s5,s6,s7,s8,s9,s10
c initialisation
      INTEGER lflag
c  moid routine
      DOUBLE PRECISION moid,dnp,dnm
      INTEGER iconv
c memory model
      SAVE
      DATA lflag /0/
      IF(lflag.eq.0)THEN
         ipla0=0
         lflag=1
      ENDIF
c not a continuation, but ready to continue target plane
c analysis (not for Newton's method)
      icont=0
      CALL vcopy(6,eq0,eqc)
      tc=t0
      CALL ident(6,de1de0)
c ================================================================
c menu/batch control
      IF(batchcl)THEN
         iclan=1
      ELSE
         menunam='fclan'
         CALL menu(iclan,menunam,2,'Target plane selection=',
     +        'modified target plane (MTP)=',
c     +        'Opik impact plane (b-plane)=',
     +        'old MTP, stored previously=',
c     +        'old b-plane, stored previously=',
     +        s3,s4,s5,s6,s7,s8,s9,s10)
         IF(iclan.eq.0)RETURN
      ENDIF
c ================================================================
c search for close approaches: final time
 55   CONTINUE
      IF(icont.eq.1)THEN
        t1old=t1
      ENDIF
c if new target plane has to be found, exploratory propagation
c      IF(iclan.eq.1.or.iclan.eq.2)THEN
       IF(iclan.eq.1)THEN
         IF(batchcl)THEN
c t1 has been passed as dummy variable, nothing to do
         ELSE
c select final time
 56         write(99,*)' search for close approaches until time (MJD)?'
            READ(*,*) t1
            IF(icont.eq.1.and.(t1-tc)/(t1old-tc).le.1.d0)THEN
               write(99,*)' change in direction not allowed '
               write(99,*)' epoch, old target time, new target time' 
               write(99,*) tc,t1old,t1,' must be in sequence'
               GOTO 56
            ENDIF
         ENDIF 
c initialize close approach count
         eprdot=1.d-10
         njc=0
c no target plane so far
         njt=0
         mtpon=.false.
         epsmtp=1.d-10
c check availability of JPL ephemerides
         IF(t1.lt.tejpl1.or.t1.gt.tejpl2)THEN
            write(99,*)' JPL ephemerides not available for t1=',t1
            write(99,*)' but only for interval ',tejpl1,tejpl2
            ok=.false.
            RETURN
         ENDIF
c explorative propagation
         iplam=0
         CALL proele('EQU',tc,eqc,t1,eq1)
c select modified target plane
         CALL mtpsel(batchcl,tc,t1,xc0,vc0,tcl0,tcl1,tcl2,error)
         IF(error)RETURN
         dmin0=vsize(xc0)
         vmin0=vsize(vc0)
         tcros=tcl0
         WRITE(iun20,*)' close app. at t=',tcl0
         WRITE(iun20,*)' distance=',dmin0,' velocity=',vmin0
c     ELSEIF(iclan.eq.3.or.iclan.eq.4)THEN
      ELSEIF(iclan.eq.2)THEN
c for previously selected target plane: what to do? nothing
         write(99,*)' using previously selected target plane'
         WRITE(iun20,*)' using previously selected target plane'
         write(99,*)' close app. at t=',tcl0
         write(99,*)' distance=',dmin0,' velocity=',vmin0
      ELSE
         write(99,*)' fclan: option iclan=',iclan,' not available'
         RETURN
      ENDIF
c =============================================================
c call close approach driver routine clan
c select target plane type not implemented yet
c      IF(iclan.eq.1.or.iclan.eq.3)THEN
c iclan 1,3 means Modified Target Plane 
         CALL clan(batchcl,iun20,iclan,tc,eqc,gc,tcl0,xc0,vc0,
     +        tpc,dtpdet,vt3,sig,axes,tcl,eq1,de1de0)
c      ELSEIF(iclan.eq.2.or.iclan.eq.4)THEN
c iclan 2,4 means Opik target plane
c        CALL opclan(batchcl,iun20,iclan,tc,eqc,gc,tcl0,xc0,vc0,
c    +        tpc,dtpdet,vt3,sig,axes,tcl,eq1,de1de0)
c     ELSE
c        write(99,*)' fclan: option iclan=',iclan,' not available'
c        RETURN
c     ENDIF     
c =====================================================================
c options
c ===================================================================
c restart from menu
 58   CONTINUE
c choose handling of nonlinearity
      IF(batchcl)THEN
c batch use is for Newton's method (from clonew.f)
         inl=4
      ELSE
         menunam='closnonl'
         CALL menu(inl,menunam,4,'How to handle nonlinearity?=',
     +        'linear map=',
     +        'full n-body nonlinearity=',
     +        'Newtons method on LOV, 1 step=',
     +        'Newtons method on LOV, auto=',
     +        s5,s6,s7,s8,s9,s10)
c exit, disconnecting mtp search routine
         IF(inl.eq.0)THEN
            mtpon=.false.
c disconnected... this does not work! The changes to intiial conditions have
c to be applied at the original epoch, not at the intermediate one! Although
c the linear analysis is good, all the semilinear and newton fails
c           write(99,*)' go on with search for close app.? 1=yes 0=no'
c           READ(*,*)icont
c           IF(icont.eq.1)THEN
c              tc=tcl
c              CALL vcopy(6,eq1,eqc)
c              GOTO 55
c check against change of direction if icont=1           
c           ELSE
               RETURN
c           ENDIF
         ENDIF
      ENDIF
c ===================================================================
c operation mode depending upon inl
      IF(inl.eq.1.or.inl.eq.2)THEN
c ============target plane analysis:
c input specification of set of points
         CALL asscbd(iun20,npox,npo,sigma,ibv)
c If ibv=0 then use automatic selection method
         IF(ibv.eq.0)THEN
            maxsig=max(sig(1),sig(2))
            minsig=min(sig(1),sig(2))
            if(maxsig/minsig.le.200.d0)then
               ibv=1
            else
               ibv=2
            endif
         endif    
c compute ellipse in the elements space 
         CALL slinel(dtpdet,gc,cc,ceicel,b,v)
c compute line of orbital elements
         CALL linobs(ibv,npo,eqc,axes,sig,b,v,sigma,ceicel,elm,npo1)
c display confidence boundary
         CALL fclanout(iun20,inl,tc,eqc,elm,npo1,tpc,dtpdet,vt3,
     +        tcl1,tcl2,ibv,tcl0,astna0)
      ELSEIF(inl.eq.3.or.inl.eq.4)THEN
c ============Newton's method to find virtual impactors:
         WRITE(iun20,*)' Newton method'
         itnewma=10
         newcont=1.d-7
         itnew=1
c beginning loop on iterations
 59      CONTINUE
c one newton's method iteration
         CALL clanew(tcl0,tcl1,tcl2,axes,sig,tpc,dtpdet,t0,t1,
     +     eq0,gc,cc,csinor,delnor,
     +     m,objid,iobs,tau,tutm,idsta,aln,den,rmsa,rmsd,sel,
     +     iun20,error,dminew,tpr)
c iteration step acceptable?
         IF(error)THEN
            write(99,*)' Newton failed at iteration ',itnew
            write(iun20,*)' Newton failed at iteration ',itnew
            IF(batchcl) THEN
                ok=.false.
                RETURN
            ELSE
                GOTO 58
            ENDIF
         ENDIF
c a new iteration is required?
         IF(inl.eq.3)THEN
c no, only one requested; go back to fclan menu
            IF(batchcl) THEN
               RETURN
            ELSE
               GOTO 58
            ENDIF
         ELSE
c convergence control
            IF(abs(dminew-tpr).lt.newcont)THEN
c convergence already achieved
               write(99,*)' newton converged'
               astna0='virimp'
               CALL wromlr (iun20,astna0,eq0,'EQU',t0,gc,.true.,
     +                  cc,.true.,hmag,gmag,0.d0)
               CALL nomoid(t0,eq0,moid,iconv,dnp,dnm)
               write(iun20,198)moid,iconv,dnp,dnm
 198           format('!MOID ',f8.5,1x,i4/'!NODES ',f8.5,1x,f8.5)
c virtual impactor found
               IF(batchcl)THEN
                  RETURN
               ELSE
c ephemerides requred?
                 write(99,*)' ephemeris of virtual impactor? 1=yes 0=no'
                  read(*,*)ikey
                  IF(ikey.eq.1)THEN
                     CALL virimp(tpc,dtpdet,axes,sig,ceicel,b,v,
     +                    t0,eq0,gc,cc,hmag,gmag,iun20)
                  ENDIF
               GOTO 58
               ENDIF
            ELSE
c not yet convergent
               itnew=itnew+1
               IF(itnew-1.le.itnewma)THEN
c next iteration
                  GOTO 59
               ELSE
c too many iterations
                  write(99,*)' Newton failed to converge'
                  write(iun20,*)' Newton failed to converge'
                  IF(batchcl)THEN
                     ok=.false.
                     RETURN
                  ELSE
                     GOTO 58
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
c ===========================================================
c back to the menu
      GOTO 58
      END








