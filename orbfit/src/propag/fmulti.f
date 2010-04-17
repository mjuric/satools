c =======================================
c  FMULTI
c =======================================
c multiple solutions for a differential correction problem
c ===============INTERFACE========================
      SUBROUTINE fmulti(obsc,inic,ok,covc,tc,
     +     eqc,gc,cc,csinor,delnor,
     +     mc,iobs,tauc,istc,alc,dec,rmsa,rmsd,
     +     rmsmag,smag,hc,gmag,sel,
     +     iun20,eqm,gm,cm,hmu,csinom,delnom,
     +           sigma,imult,imim,imip,imi0)
      IMPLICIT NONE
c ================INPUT===========================
c ======observations====
c number of obs., time, station code, alpha, delta
      INTEGER mc,istc(mc)
      DOUBLE PRECISION tauc(mc),alc(mc),dec(mc)
c rms of alpha, delta, rms magnitudes, magnitudes (string)
      DOUBLE PRECISION rmsa(mc),rmsd(mc),rmsmag(mc)
      CHARACTER*6 smag(mc)
c selection flags, obs type
      INTEGER sel(mc),iobs(mc)
c initial conditions: epoch, elements, abs. magnitude, gmag
      DOUBLE PRECISION tc,eqc(6),hc,gmag
c normal and covariance matrices, norms of residuals, of last correction
      DOUBLE PRECISION gc(6,6),cc(6,6),csinor,delnor
c output unit
      INTEGER iun20,iun
c logical flags
      LOGICAL obsc,inic,covc
c ===============OUTPUT=========================
c number of alternate solutions, maximum
      INTEGER imult
      INCLUDE 'parmul.h'
c multiple solutions elements
      DOUBLE PRECISION eqm(6,mulx)
c normal and covariance matrices  
      DOUBLE PRECISION gm(6,6,mulx),cm(6,6,mulx)
c norms of residuals, of last correction
      DOUBLE PRECISION csinom(mulx),delnom(mulx),sigq(mulx),rescov
c magnitude for each alternate solution
      DOUBLE PRECISION hmu(mulx)
c no. of sigma along the line  
      DOUBLE PRECISION sigma
c first, last, reference solution
      INTEGER imim,imip,imi0
c success flas
      LOGICAL ok
c ==============END INTERFACE=====================
c interactive?
      INCLUDE 'comdif.h'
c weak direction, rms along it, weight for semimajor axis
      DOUBLE PRECISION wdir(6),wdir0(6),sdir,direc,prscag
c weighing to be computed
      INCLUDE 'parobx.h'
      DOUBLE PRECISION wc(nob2x)
c residuals
      DOUBLE PRECISION csirc(nob2x)
c no obs, used 
      INTEGER nused
c ======== differential correction flags and controls ======
c scalar temporaries
      DOUBLE PRECISION dn
c loop indexes 
      INTEGER i, j, imi,jj
c success flag for diff. correction
      LOGICAL succ
c rms magnitudes, residuals
      DOUBLE PRECISION rmsh,resmag(nobx)
c ======== output moid =====================
      DOUBLE PRECISION moid(mulx), dnp(mulx), dnm(mulx)
      INTEGER iconv(mulx)
c hack for catalog
      INTEGER iunctc,le
      CHARACTER*9 astna0
c control of integration method
      INTEGER imint
      DOUBLE PRECISION hh
      LOGICAL fail
c control of bizarre orbits
      LOGICAl bizarre
c =====================================================================
c check availability of observations and initial condition
      CALL chereq(2,inic,covc,tc,iun20,-1,ok)
      IF(.not.ok) RETURN
c =====================================================================
c check availability of JPL ephemerides and ET-UT table
      CALL chetim(tauc(1),tauc(mc),ok)
      IF(.not.ok) RETURN
c =====================================================================
c weights (removed set to zero!)
      call fitwgt(rmsa,rmsd,dec,sel,iobs,wc,mc,.true.)
c =====================================================================
c compute line of variations
c      aw=1.d0
      CALL weakdi(gc,wdir,sdir,iun20)
      CALL vcopy(6,wdir,wdir0)
c input parameters of segment on the variations line
      IF(batch)THEN
c sigma and imult have to be passed in the call
         WRITE(iun20,*)' sigma=',sigma,'  in ',imult,' steps'
         IF(2*imult+1.gt.mulx)THEN
            write(99,*)' too many; max is ',mulx
            STOP
         ENDIF
      ELSE
         write(99,*)' how many sigma?'
         READ(*,*) sigma
 259     write(99,*)' how many steps on each side?'
         READ(*,*) imult
         WRITE(iun20,*)' sigma=',sigma,'  in ',imult,' steps'
         IF(2*imult+1.gt.mulx)THEN
            write(99,*)' too many; max is ',mulx
            GOTO 259
         ENDIF
      ENDIF
      dn=1.d0/float(imult)
c =====================================================================
c nominal solution at the center of the list
      imi0=imult+1
      CALL vcopy(6,eqc,eqm(1,imi0))
      CALL mcopy(6,6,gc,gm(1,1,imi0))
      CALL mcopy(6,6,cc,cm(1,1,imi0))
      csinom(imi0)=csinor
      delnom(imi0)=delnor
      sigq(imi0)=0.d0
      hmu(imi0)=hc
c orbital distance
      CALL nomoid(tc,eqm(1,imi0),moid(imi0),
     +              iconv(imi0),dnp(imi0),dnm(imi0))
c =====================================================================
c main loop on number of steps (positive side)
c =====================================================================
       DO 5 i=1,imult
        imi=imult+i+1
        write(99,*)' alternate solution no. ',imi
        WRITE(iun20,*)' alternate solution no. ',imi
c use weak direction to find initial conditions 
        hh=dn*sigma
        iun=-iun20
c 1=Euler 2= RK2
        imint=2
        CALL intstep(eqm(1,imi-1),eqm(1,imi),hh,imint,
     +       mc,wc,sel,iobs,tauc,istc,tc,
     +           alc,dec,iun,wdir,sdir,fail)
c check for hyperbolic
        IF(fail.or.sqrt(eqm(2,imi)**2+eqm(3,imi)**2).ge.0.99d0
     +       .or.eqm(1,imi).le.0.d0)THEN
           write(99,*)'step ',imi,' hyperbolic'
           write(99,*)(eqm(j,imi),j=1,6)
           imi=imi-1
           GOTO 6
        ELSEIF(bizarre(eqm(1,imi)))THEN
           write(99,*)'step ',imi,' byzarre'
           write(99,*)(eqm(j,imi),j=1,6)
           imi=imi-1
           GOTO 6
        ELSE
c differential corrections: 
           iun=-iun20
c for verbose          iun=iun20
           CALL difvin(mc,wc,sel,iobs,tauc,istc,tc,eqm(1,imi),
     +           alc,dec,wdir,gmag,
     +           iun,eqm(1,imi),gm(1,1,imi),cm(1,1,imi),
     +           csinom(imi),delnom(imi),csirc,nused,succ)
c exit if not convergent
           IF(.not.succ) THEN
              imi=imi-1
              GOTO 6
           ENDIF
c estimate magnitude here
           CALL magest(smag,rmsmag,sel,mc,hmu(imi),resmag,rmsh)
c orbital distance
           CALL nomoid(tc,eqm(1,imi),moid(imi),
     +              iconv(imi),dnp(imi),dnm(imi))
c check for sigQ.le.sigma
           sigq(imi)=sqrt(abs(csinom(imi)**2-csinom(imi0)**2)*nused)/
     +             rescov(6,nused,csinor)
           IF(batch.and.sigq(imi).gt.sigma)THEN
              GOTO 6
           ENDIF
c compute line of variations
           CALL weakdi(gm(1,1,imi),wdir,sdir,iun)
           direc=prscag(6,wdir,wdir0)
           IF(direc.lt.0.d0)THEN
              DO jj=1,6
                 wdir(jj)=-wdir(jj)
              ENDDO
           ENDIF
           CALL vcopy(6,wdir,wdir0)
        ENDIF
 5    CONTINUE
c =====================================================================
c recompute line of variations
 6    imip=imi
      CALL weakdi(gc,wdir,sdir,iun20)
      direc=prscag(6,wdir,wdir0)
      IF(direc.lt.0.d0)THEN
         DO jj=1,6
            wdir(jj)=-wdir(jj)
         ENDDO
      ENDIF
      CALL vcopy(6,wdir,wdir0)
      DO 7 i=1,imult
c =====================================================================
c main loop on number of steps (negative side)
c =====================================================================
        imi=imult-i+1
        write(99,*)' alternate solution no. ',imi
        WRITE(iun20,*)' alternate solution no. ',imi
c use weak direction to find initial conditions 
        hh=-dn*sigma
        iun=-iun20
c 1=Euler 2= RK2
        imint=2
        CALL intstep(eqm(1,imi+1),eqm(1,imi),hh,imint,
     +       mc,wc,sel,iobs,tauc,istc,tc,
     +           alc,dec,iun,wdir,sdir,fail)
c check for hyperbolic
        IF(fail.or.sqrt(eqm(2,imi)**2+eqm(3,imi)**2).ge.0.99d0
     +       .or.eqm(1,imi).le.0.d0)THEN
           write(99,*)'step ',imi,' hyperbolic'
           write(99,*)(eqm(j,imi),j=1,6)
           imi=imi+1
           GOTO 8
        ELSEIF(bizarre(eqm(1,imi)))THEN
           write(99,*)'step ',imi,' byzarre'
           write(99,*)(eqm(j,imi),j=1,6)
           imi=imi+1
           GOTO 8
        ELSE
c differential corrections: 
           iun=-iun20
c for verbose        iun=iun20
           CALL difvin(mc,wc,sel,iobs,tauc,istc,tc,eqm(1,imi),
     +           alc,dec,wdir,gmag,
     +           iun,eqm(1,imi),gm(1,1,imi),cm(1,1,imi),
     +           csinom(imi),delnom(imi),csirc,nused,succ)
c exit if not convergent
           IF(.not.succ)THEN
               imi=imi+1
               GOTO 8
           ENDIF
c estimate magnitude here
           CALL magest(smag,rmsmag,sel,mc,hmu(imi),resmag,rmsh)
c orbital distance
           CALL nomoid(tc,eqm(1,imi),moid(imi),
     +              iconv(imi),dnp(imi),dnm(imi))
c check for sigQ.le.sigma
           sigq(imi)=sqrt(abs(csinom(imi)**2-csinom(imi0)**2)*nused)/
     +             rescov(6,nused,csinor)
           IF(batch.and.sigq(imi).gt.sigma)THEN
              GOTO 8
           ENDIF
c compute line of variations
           CALL weakdi(gm(1,1,imi),wdir,sdir,iun)
           direc=prscag(6,wdir,wdir0)
           IF(direc.lt.0.d0)THEN
              DO jj=1,6
                wdir(jj)=-wdir(jj)
              ENDDO
           ENDIF
           CALL vcopy(6,wdir,wdir0)
        ENDIF
 7    ENDDO
c =====================================================================
c summary table
c =====================================================================
 8    imim=imi
      IF(batch)RETURN
      CALL tee(iun20,'SUMMARY OF MULTIPLE SOLUTIONS=')
      CALL tee(iun20,
     +  'no       a      h      k      p      q      lambda=')
      CALL filopn(iunctc,'mult.ctc','unknown')
      CALL wromlh(iunctc,'ECLM','J2000')
      DO i=imim,imip
        write(99,144)i,(eqm(j,i),j=1,6)
        WRITE(iun20,144)i,(eqm(j,i),j=1,6)
 144    FORMAT(i3,6f12.8)
      ENDDO
      CALL tee(iun20,'no  RMS ,lastcor,  magn,  MOID ,nod+,nod-, sigQ=')
      DO i=imim,imip
        write(99,145)i,csinom(i),delnom(i),hmu(i)
     +                    ,moid(i),dnp(i),dnm(i),iconv(i),sigq(i)
        WRITE(iun20,145)i,csinom(i),delnom(i),hmu(i)
     +                    ,moid(i),dnp(i),dnm(i),iconv(i),sigq(i)
 145    FORMAT(i5,1x,1p,e13.5,e11.3,2x,0p,f5.2,1x,     
     +              f8.5,1x,f8.5,1x,f8.5,1x,i2,1x,f6.3)
        WRITE(astna0,111)i
 111    FORMAT('mult_',i4)
        CALL rmsp(astna0,le)
        CALL wromlr(iunctc,astna0(1:le),eqm(1,i),'EQU',tc,gm(1,1,i),
     +    .true.,cm(1,1,i),.true.,hmu(i),gmag,0.d0)
      ENDDO
      CALL filclo(iunctc,' ')
      RETURN
      END






