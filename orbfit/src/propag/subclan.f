c =================================================================
c  CLAN2: close approach linear analysis; MTP plane roatted to approach Opik b-plane 
c        used by both fitobs (fclan) and neofit (clolin)
c =================================================================
      SUBROUTINE clan(batchcl,iunaxe,iclan,tc,eqc,gc,tcl0,xc0,vc0,
     +   tpc,dtpdet,vt3,sig,axes,tcl,eq1,de1de0)
      IMPLICIT NONE
c logical controls
      LOGICAL batchcl
c ================INPUT===========================
c initial conditions: epoch, elements, covariance matrix
      DOUBLE PRECISION tc,eqc(6),gc(6,6)
c time of close approach, geocentric position and velocity
      DOUBLE PRECISION tcl0,xc0(3),vc0(3)
c output unit
      INTEGER iunaxe
c target plane type (so far dummy)
      INTEGER iclan
c ================OUTPUT==========================
c target ellipse
      DOUBLE PRECISION axes(2,2),sig(2)
c target plane coordinates: cartesian, derivatives transposed
      DOUBLE PRECISION tpc(2),dtpdet(6,2)
c basis adapted to MTP 
      DOUBLE PRECISION vt3(3,3)
c orbital elements near the close approach
      DOUBLE PRECISION tcl,eq1(6)
c =============INPUT/OUTPUT=========================
c partials of the propagated state w.r. to the original one
      DOUBLE PRECISION de1de0(6,6)
c ================END INTERFACE===================
c target plane coordinates: distance
      DOUBLE PRECISION tpr
c relative positions, also in reference system adapted to MTP
      DOUBLE PRECISION dx(3),dv(3),vclo,vsize
c basis adapted to MTP 
      DOUBLE PRECISION v3(3,3)
c earth, asteroid, mean motion
      DOUBLE PRECISION xea(6),xast(6),enne
c partials of coordinates with respect to elements
      DOUBLE PRECISION dxde(6,6),ddxde(6,6,6)
      DOUBLE PRECISION dxde1(6,6),de1dx1(6,6)
c derivatives control (for propag)
      INTEGER ider
c unit for matlab files
      INTEGER iun7
c arrays of close approach data and controls
      INCLUDE 'cloapp.h'
      INCLUDE 'closapl.h'
c ======== constant of gravitation ==============
      include 'sunmass.h'
c times "before" and "after"
      DOUBLE PRECISION tcl1,tcl2
c dirty trick to force restart anyway
      INCLUDE 'restart.h'
c loop indexes
      INTEGER i
c ======== call to force =======================
      INTEGER nvar2,idc
c acceleration, planetary positions
      DOUBLE PRECISION dery(3),xpla(6)
c control of derivatives
      INTEGER ide
      COMMON/deriv/ide
      SAVE
c restore tpnor for batch use
      IF(batchcl)THEN
         vclo=vsize(vc0)
         DO i=1,3
           tpnor(i)=vc0(i)/vclo
         ENDDO
      ENDIF
c select times "before" and "after" the close approach under study
      CALL aftclo(iplam,tcl0,tc,tcl0,tcl1,tcl2)
c ===============================================
c propagate to epoch "after" close approach
      tcl=tcl2
c no target plane
      mtpon=.false.
c with state transition matrix
      ider=1 
      CALL propag(tc,eqc,tcl,xast,xea,ider,dxde1,ddxde)
c use accumulated state transition matrix
      CALL mulmat(dxde1,6,6,de1de0,6,6,dxde)
c compute new elements, with partial derivatives
      CALL cooder(xast,'CAR',gms,eq1,'EQU',enne,de1dx1)
c chain rule to obtain d(east1)/d(east0)
      CALL mulmat(de1dx1,6,6,dxde,6,6,de1de0)
c now propagate from there to the exact time of close apparoach 
c (established in an exploratory integration)
      CALL propag(tcl,eq1,tcl0,xast,xea,ider,dxde1,ddxde)
c chain rule to obtain d(xast1)/d(east0)
      CALL mulmat(dxde1,6,6,de1de0,6,6,dxde)
c right hand side at target (to obtain xpla)
      nvar2=3
      ide=0
      CALL force(xast,xast(nvar2+1),tcl0,dery,nvar2,idc,xpla)
      IF(idc.ne.iplam)THEN
         write(99,*)' fclan: ???? idc=',idc,' iplam=',iplam
      ENDIF
c relative coordinates
      DO i=1,3
        dx(i)=xast(i)-xpla(i)
        dv(i)=xast(i+3)-xpla(i+3)
      ENDDO
c =================================================================
c reference system with tpnor as first axis, dx as second axis
c =================================================================
c reference system with tpnor as first axis, dx as second axis
      IF(iclan.eq.1)THEN
         CALL mtpref(tpnor,xpla(4),v3,vt3)
      ENDIF
      IF(.not.batchcl.and.iclan.eq.1)THEN
c output for matlab processing
         CALL filopn(iun7,'v3.fla','unknown')
         WRITE(iun7,127)xc0
         WRITE(iun7,127)vc0
         WRITE(iun7,127)v3
 127     FORMAT(3e20.12)
         CALL filclo(iun7,' ')
      ENDIF
c =================================================================
c rotation to v3 reference system
      CALL mtprot(batchcl,vt3,dx,dv,dxde,gc,tpc,dtpdet,sig,axes,tpr)
c output the data on the target plane ellipse
      IF(batchcl)THEN
         WRITE(iunaxe,171)sig
         WRITE(iunaxe,172)axes
      ELSE
         write(99,171)sig
         WRITE(iunaxe,171)sig
 171     FORMAT(' semiaxes of target ellipse: lengths ',1p,2d14.5)
         write(99,172)axes
         WRITE(iunaxe,172)axes
 172     FORMAT(' directions ',2f10.6/12x,2f10.6)
      ENDIF
c
      RETURN
      END
c =================================================================
c reference system with tpnor as first axis, -dx as third axis
c to be used with dx= eliocentric velocity vector of the planet
      SUBROUTINE mtpref(tpnor,dx,v3,vt3)
      IMPLICIT NONE
      DOUBLE PRECISION tpnor(3),dx(3),v3(3,3),vt3(3,3)
c end interface
      DOUBLE PRECISION vl,vsize,vv,prscal
      INTEGER i
c the first vetor is normal to the target plane
      CALL vcopy(3,tpnor,v3(1,1))
c the third vector is the projection of -dx on the plane orthogonal to tpnor       
      vv=prscal(tpnor,dx)
      DO i=1,3
         v3(i,3)=-(dx(i)-vv*tpnor(i))
      ENDDO
c reduced to unit length
      vl=vsize(v3(1,3))
      DO i=1,3
         v3(i,3)=v3(i,3)/vl
      ENDDO
c the second vector must be orthogonal to both
      CALL prvec(tpnor,v3(1,3),v3(1,2))
c paranoia check of unit length
      vl=vsize(v3(1,2))
      DO i=1,3
         v3(i,2)=v3(i,2)/vl
      ENDDO
c the inverse is the transposed matrix
      CALL transp(v3,3,3,vt3)
      RETURN
      END
c =================================================================
c MTPROT
c rotation to v3 reference system
      SUBROUTINE mtprot(batchcl,vt3,dx,dv,dxde,gc,tpc,dtpdet,
     +        sig,axes,tpr)
      IMPLICIT NONE
c input
      LOGICAL batchcl
      DOUBLE PRECISION vt3(3,3),dx(3),dv(3),dxde(6,6),gc(6,6)
c output 
      DOUBLE PRECISION tpc(2),dtpdet(6,2),sig(2),axes(2,2),tpr
c end interface
      DOUBLE PRECISION xx(3),vv(3),dxde3(3,6),dtpcde(2,6)
      DOUBLE PRECISION tpth,gtp(2,2),dxxde(3,6)
      INTEGER i,ii,jj
      CALL mulmav (vt3,3,3,dx,3,xx)
      IF(.not.batchcl)write(99,*)' distance from target plane=',xx(1)
      CALL mulmav (vt3,3,3,dv,3,vv)
      DO i=1,3
        DO ii=1,6
          dxde3(i,ii)=dxde(i,ii)
        ENDDO
      ENDDO
      CALL mulmat (vt3,3,3,dxde3,3,6,dxxde)
      DO ii=1,2
        tpc(ii)=xx(ii+1)
        DO jj=1,6
          dtpcde(ii,jj)=dxxde(ii+1,jj)-(vv(ii+1)/vv(1))*dxxde(1,jj)
        ENDDO
      ENDDO
      tpr=sqrt(tpc(1)**2+tpc(2)**2)
      tpth=atan2(tpc(2),tpc(1))
      IF(.not.batchcl)THEN
         write(99,170) tpc,tpr,tpth
 170     FORMAT('MTP coordinates ',2f12.8,' dist ',f12.8,' angle ',f8.4)
         write(99,*)' partial derivatives'
         write(99,171) (dtpcde(1,jj),jj=1,6)
         write(99,171) (dtpcde(2,jj),jj=1,6)
 171  FORMAT(6(f12.5,2x))
      ENDIF
c ========================================================
c compute target ellipse of confidence
      CALL transp(dtpcde,2,6,dtpdet)
      CALL ellips(dtpdet,gc,sig,axes,gtp)
      RETURN
      END
c ====================================================================
c MTPSEL
c display list of current close approaches, select one target plane
      SUBROUTINE mtpsel(batchcl,tc,t1,xc0,vc0,tcl0,tcl1,tcl2,error)
      IMPLICIT NONE
c input: batch control
      LOGICAL batchcl
c epoch of initial conditions, target time for propagation
      DOUBLE PRECISION tc,t1
c output: position/velocity relative to planet, time of closest appr,
c         times  "before" and "after" (all MJD)
      DOUBLE PRECISION xc0(3),vc0(3),tcl0,tcl1,tcl2
c return error flag
      LOGICAL error
c arrays of close approach data and controls
      INCLUDE 'cloapp.h'
      INCLUDE 'closapl.h'
      INTEGER j,i
      DOUBLE PRECISION rmin0
c display list of current close approaches
      error=.false.
      IF(iplam.eq.0)THEN
         write(99,*)' no close approaches found'
c error flag
         error=.true.
         RETURN
      ELSEIF(iplam.gt.0)THEN
         IF(.not.batchcl.or.njc.gt.1)THEN
            write(99,*)' close approaches to planet ',iplam
            write(99,*)' time(MJD), min.dis.(AU), normal to MTP'
            DO  j=1,njc
               write(99,*)tcla(j),rmin(j),(vmtp(i,j),i=1,3)
            ENDDO
         ENDIF
      ELSE
         write(99,*)' mtpsel: this should not happen, iplam=',iplam
         error=.true.
         RETURN
      ENDIF
c =================================================================
c select modified target plane
      IF(njc.eq.1)THEN
         IF(.not.batchcl)THEN
 60         write(99,*)' select the target plane: 1=yes 0=quit'
            READ(*,*)jtsel 
            IF(jtsel.lt.0.or.jtsel.gt.1)GOTO 60
            IF(jtsel.eq.0)THEN
                 error=.true.
                 RETURN
            ENDIF
            write(99,*)
         ELSE
            jtsel=1
         ENDIF
      ELSEIF(njc.gt.1)THEN
         IF(.not.batchcl)THEN
 61         write(99,*)' WARNING: close approach analysis might'
            write(99,*)' not work when there are multiple minima;anyway'
            write(99,*)' select one of the target planes jtsel=1,',njc
            READ(*,*)jtsel
c           IF(jtsel.eq.0)RETURN
            IF(jtsel.lt.0.or.jtsel.gt.njc)GOTO 61
         ELSE
c in batch mode multiple minima imply failure
            error=.true.
            RETURN
         ENDIF
      ELSEIF(njc.le.0)THEN
         write(99,*)' mtpsel: this should not happen; no close app'
         error=.true.
         RETURN
      ENDIF
c storage of the modified target plane, of the nominal close approach
      DO i=1,3
         tpnor(i)=vmtp(i,jtsel)
         xc0(i)=xcla(i,jtsel)
         vc0(i)=vcla(i,jtsel)
      ENDDO
      rmin0=rmin(jtsel)
      tcl0=tcla(jtsel)
c select time interval to get out of the close approach, before and after
      CALL aftclo(iplam,tcl0,tc,t1,tcl1,tcl2)
c store current planet for next time
      ipla0=iplam
      RETURN
      END
c ================================================================
c AFTCLO
c select time interval to get out of the close approach, before and after
      SUBROUTINE aftclo(iplam,tcl0,tc,t1,tcl1,tcl2)
      IMPLICIT NONE
c input: planet number, time of closest aprpoach
      INTEGER iplam
      DOUBLE PRECISION tcl0
c        epoch of initial conditions, target time of propagation
      DOUBLE PRECISION tc,t1
c output: time "before" and "after"
      DOUBLE PRECISION tcl1,tcl2
c margin
      DOUBLE PRECISION delt
      IF(iplam.le.4)THEN
c this interval is suitable for terrestrial planet encounters only!!!
c         delt=2.d1
          delt=3.d1
      ELSE
         delt=365.25d0
      ENDIF
      IF(tc.lt.t1)THEN
c analysis of future close approaches
         tcl1=tcl0-delt
         tcl2=tcl0+delt
      ELSE
c past close approaches
         tcl1=tcl0+delt
         tcl2=tcl0-delt
      ENDIF
c      write(99,*)tc,tcl0,tcl2
      RETURN
      END
