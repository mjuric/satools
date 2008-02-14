c =======================================
c  FMUOBS
c =======================================
c output of multiple observations 
c ===============INTERFACE========================
      SUBROUTINE fmuobs(tc,gmag,iobs,ids,t1,tut1,sigma,eqm,hmu,
     +     aobs,dobs,iff,imim,imip,imi0,titnam,filnam,iun20)
      IMPLICIT NONE
c reference orbit: epoch, G magnitude 
      DOUBLE PRECISION tc,gmag
c first last, and reference index of multiple orbits
      INTEGER imim,imip,imi0
c multiple solutions elements, magnitudes,sigma value
      DOUBLE PRECISION eqm(6,imip),hmu(imip),sigma
c actual observations
      DOUBLE PRECISION aobs,dobs
c strings with asteroid names, output unit
      CHARACTER*80 titnam
      CHARACTER*60 filnam
      INTEGER iun20
c station code, flag for use of act.obs., observation type
      INTEGER ids,iff,iobs
c target time (TDT, UTC)
      DOUBLE PRECISION t1,tut1
c =================END INTERFACE====================
      INCLUDE 'parmul.h'
      DOUBLE PRECISION alm(mulx),dem(mulx)
      INTEGER ng,i,npop
      DOUBLE PRECISION eqm1(6,mulx),amagn(mulx),eq1(6),alpha,delta
c trig constants
      INCLUDE 'trig.h'
c observation auxiliary data (e.g. distance)
      INCLUDE 'phase.h'
c multiple data for confidence boundary
      INCLUDE 'npoint.h'
c xephem interface data
      INCLUDE 'xephem.h'
c covariance (here it is dummy)
      integer icov
      double precision gamad(2,2),axes(2,2),sig(2)
c ====================================================
c this routine does not handle confidece elllipses 
      icov=1
c first compute nominal prediction
      CALL proele('EQU',tc,eqm(1,imi0),t1,eq1)
      CALL preobs('EQU',t1,ids,t1,eq1,iobs,alpha,delta,hmu(imi0),gmag
     + ,amagn(imi0))
      WRITE(0,*)' nominal solution '
      CALL outobc(iun20,iobs,ids,tut1,alpha,delta,amagn(imi0),adot,ddot,
     +     elo,dis,icov,gamad,sig,axes)
      alm(imi0)=0.d0
      dem(imi0)=0.d0
      adotv(imi0)=adot
      ddotv(imi0)=ddot
      disv(imi0)=dis
c loop on existing multiple solutions: first forward, then backward
      DO 147 i=imi0+1,imip
         CALL proele('EQU',tc,eqm(1,i),t1,eqm1(1,i))
         CALL preobs('EQU',t1,ids,t1,eqm1(1,i),iobs,alm(i),dem(i),
     +        hmu(i),gmag,amagn(i))
         IF(alm(i).gt.pig)alm(i)=alm(i)-dpig
         disv(i)=dis
         adotv(i)=adot
         ddotv(i)=ddot
         WRITE(0,*)' alternate obs.no. ',i
         CALL outobc(iun20,iobs,ids,tut1,alm(i),dem(i),amagn(i),
     +     adot,ddot,elo,dis,icov,gamad,sig,axes)
         alm(i)=alm(i)-alpha
         dem(i)=dem(i)-delta
c keep count of lost revolutions in alpha
         IF(i.eq.imi0)THEN
c initialize revolution counter
            ng=0
         ELSE
c update revolution counter
            CALL angupd(alm(i),alm(i-1),ng)
         ENDIF
 147  CONTINUE
      DO 148 i=imi0-1,imim,-1
         CALL proele('EQU',tc,eqm(1,i),t1,eqm1(1,i))
         CALL preobs('EQU',t1,ids,t1,eqm1(1,i),iobs,alm(i),dem(i),
     +        hmu(i),gmag,amagn(i))
         disv(i)=dis
         adotv(i)=adot
         ddotv(i)=ddot
         WRITE(0,*)' alternate obs.no. ',i
         CALL outobc(iun20,iobs,ids,tut1,alm(i),dem(i),amagn(i),
     +     adot,ddot,elo,dis,icov,gamad,sig,axes)
         alm(i)=alm(i)-alpha
         dem(i)=dem(i)-delta
c keep count of lost revolutions in alpha
         IF(i.eq.imi0)THEN
c initialize revolution counter
            ng=0
         ELSE
            CALL angupd(alm(i),alm(i+1),ng)
         ENDIF
 148  CONTINUE
c ===============================================
c output multiple prediction of observations
      IF(ixeph.le.0)THEN
         CALL outmul(titnam,filnam,tut1,sigma,alpha,delta,
     +        alm,dem,hmu,imim,imip,imi0,iff,aobs,dobs,iobs)
      ELSE
c to be fixed, giving variable magnitude, but requires change to wriedb
c        CALL wriedb(t1,eqm1(imim),titnam,hmu(imim),gmag,npop)
c number of good points 
         npop=imip-imim+1
         CALL wriedb(t1,eqm1(1,imim),titnam,hmu(imi0),gmag,npop)
      ENDIF
      RETURN
      END
