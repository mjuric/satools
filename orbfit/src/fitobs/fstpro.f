c ====================================================
c FSTPRO state propagation for FITOBS
c ====================================================
      SUBROUTINE fstpro(icov,ini0,cov0,iun20,iun8,ok,
     +         t0,eq0,h0,g0,c0,tr,eq1,h1,g1,c1)
      IMPLICIT NONE
c =================INPUT=========================================
c requirements on covariance
      INTEGER icov
c availability of initial conditions, covariance, all required
      LOGICAL ini0,cov0,ok
c output units
      INTEGER iun20,iun8
c epoch time, target time
      DOUBLE PRECISION t0,tr
c elements, magnitude, covariance and normal matrix
      DOUBLE PRECISION eq0(6),h0,g0(6,6),c0(6,6)
c ================OUTPUT=================================
c elements, magnitude, covariance and normal matrix at epoch tr
      DOUBLE PRECISION eq1(6),h1,g1(6,6),c1(6,6)
c batch control
      INCLUDE 'comdif.h'
c ================END INTERFACE==========================
c trigonometric constants
      INCLUDE 'trig.h'
c keplerian elements, mena motion, opposition effect
      DOUBLE PRECISION ekr(6),enne
c     DOUBLE PRECISION gmag
c loop indexes
      INTEGER ii,j
c ======== constant of gravitation ==============
      INCLUDE 'sunmass.h'
c ======== time spans for JPL data etc. =========
      INCLUDE 'timespan.h'
c ======== xephem controls ======================
      INCLUDE 'xephem.h'
c =====================================================================
c check availability of required data
      IF(batch)THEN
         CALL chereq(icov,ini0,cov0,t0,iun20,-1,ok)
      ELSE
         CALL chereq(icov,ini0,cov0,t0,iun20,iun8,ok)
      ENDIF
      IF(.not.ok)RETURN
c =====================================================================
c check availability of JPL ephemrides
         IF(tr.lt.tejpl1.or.tr.gt.tejpl2)THEN
            WRITE(*,*)' JPL ephemrides not available for tr=',tr
            WRITE(*,*)' but only for interval ',tejpl1,tejpl2
            ok=.false.
            RETURN
         ENDIF
c =====================================================================
c propagation to time tr
      IF(icov.eq.1)THEN
c state vector only
         CALL proele('EQU',t0,eq0,tr,eq1)
         cov0=.false.
      ELSEIF(icov.eq.2)THEN
         CALL proelc('EQU',t0,eq0,g0,c0,tr,eq1,g1,c1)
      ENDIF 
      h1=h0
c =====================================================================
c output
c =====================================================================
      IF(.not.batch)THEN
         WRITE(iun20,223) tr
         WRITE(*,223) tr
 223     FORMAT(' elements at time ',f8.1,' (MJD):')
         WRITE(*,105) eq1
         WRITE(iun20,105) eq1
         CALL coocha(eq1,'EQU',gms,ekr,'KEP',enne)
         DO  ii=3,6
            ekr(ii)=ekr(ii)*degrad
         ENDDO
         WRITE(*,105)ekr
         WRITE(iun20,105)(ekr(j),j=6,1,-1)
         WRITE(*,*)
         WRITE(iun20,*)
 105     FORMAT(6f13.7)
         IF(icov.eq.2)THEN
            WRITE(iun8,*) 'COVARIANCE MATRIX FOR NEW EPOCH'
            WRITE(iun8,223) tr
            CALL outco(iun8,g1,c1)
         ENDIF
         IF(ixeph.gt.0)THEN
c this is guess!!
c           gmag=0.15d0
c          CALL wriedb(tr,eq1,titnam,h0,gmag,1)
         ENDIF
      ENDIF
      RETURN
      END




