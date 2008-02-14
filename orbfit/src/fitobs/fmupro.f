c ====================================================
c FMUPRO multiple state propagation for FITOBS
c ====================================================
      SUBROUTINE fmupro(iun20,imim,imip,t0,eqm,hmu,gm,cm,tr,eq1,g1,c1)
      IMPLICIT NONE
c maximum number of alternate solutions
      INCLUDE 'parmul.h'
c =================INPUT=========================================
c min and max index of alternate orbits
      INTEGER imim,imip
c orbits, covariance, normal matrices, abs.magnitude
      DOUBLE PRECISION eqm(6,imip-imim+1),gm(6,6,imip-imim+1),
     +     cm(6,6,imip-imim+1),hmu(imip-imim+1)
c requirements on covariance: assumed available in gm,cm
c output units
      INTEGER iun20
c epoch time, target time
      DOUBLE PRECISION t0,tr
c ================OUTPUT=================================
c elements, covariance and normal matrix at epoch tr
      DOUBLE PRECISION eq1(6,imip),g1(6,6,imip),
     +        c1(6,6,imip)
c ================END INTERFACE==========================
c keplerian elements, mean motion, opposition effect
c     DOUBLE PRECISION ekr(6),enne,gmag
c ======== output moid =====================
      DOUBLE PRECISION moid(mulx), dnp(mulx), dnm(mulx)
      INTEGER iconv(mulx)
c loop indexes
      INTEGER j,i
c =====================================================================
c main loop
c propagation to time tr
      DO j=imim,imip
        WRITE(0,*)' orbit ',j
c       CALL proelc('EQU',t0,eqm(1,j),gm(1,1,j),cm(1,1,j),
c    +          tr,eq1(1,j),g1(1,1,j),c1(1,1,j))
        CALL proele('EQU',t0,eqm(1,j),tr,eq1(1,j))
c orbital distance
        CALL nomoid(tr,eq1(1,j),moid(j),
     +              iconv(j),dnp(j),dnm(j))
      ENDDO
c =====================================================================
c summary table
c =====================================================================
      CALL tee(iun20,'SUMMARY OF MULTIPLE SOLUTIONS=')
      WRITE(iun20,223) tr
      WRITE(0,223) tr
 223  FORMAT(' elements at time ',f8.1,' (MJD):')
      CALL tee(iun20,
     +  'no.,     a      h      k      p      q      lambda=') 
      DO i=imim,imip
        WRITE(0,144)i,(eqm(j,i),j=1,6)
 144    FORMAT(i3,6f12.8)
        WRITE(iun20,144)i,(eqm(j,i),j=1,6)
      ENDDO
      CALL tee(iun20,'no.,  magn,  MOID ,  nod+  ,  nod-=')
      DO i=imim,imip
        WRITE(0,145)i,hmu(i),moid(i),dnp(i),dnm(i),iconv(i)
        WRITE(iun20,145)i,hmu(i),moid(i),dnp(i),dnm(i),iconv(i)
 145    FORMAT(i3,2x,f5.2,1x,f8.5,1x,f8.5,1x,f8.5,1x,i2)
      ENDDO
c this has to be fixed
c     IF(ixeph.gt.0)THEN
c this is guess!!
c        gmag=0.15d0
c        CALL wriedb(tr,eq1,titnam,h0,gmag,1)
c     ENDIF
      RETURN
      END




