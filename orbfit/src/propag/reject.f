* Copyright (C) 1998 by Mario Carpino (carpino@brera.mi.astro.it)
*                    and Steve Chesley (chesley@@dm.unipi.it)
* Version: December 15, 1998 Steven Chesley
* hacked for radar: A. Milani, January 16, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R E J E C T                           *
*  *                                                               *
*  *            Rejection and recovery of outliers                 *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNILOG    -  File unit for logging rejections (<0 for no log)
*           CSINOR    -  Norm of residuals
*           SEL       -  Selection flag (0 = not selected)
*           CSI       -  O-C residuals
*           W         -  Observation weights
*           NOBS      -  Number of observations
*           GAMMA     -  Covariance matrix of orbital elements
*           ICOR      -  List of solve-for parameters
*
* OUTPUT:   SEL       -  Updated selection flags
*           X2        -  CHI**2 residual
*           NMOD      -  Number of modifications to the outlier list
*                        (additions or deletions)
*
* WARNING: in some cases the weights are zero, because a scalar observation
*          has been squeezed in a two-dimensional observation
*          This is the case for obs. types 3,4 (radar)

      SUBROUTINE reject(unilog,csinor,sel,csi,mjd,x2,w,
     +     nobs,gamma,icor,nmod)
      IMPLICIT NONE

      INCLUDE 'parobx.h'
c ===========units for err,pro,clo files ========
      INCLUDE 'proout.h'

* Output of debugging information
      LOGICAL debug,log
      PARAMETER (debug=.false.)

      INTEGER nobs,icor(6),nmod,unilog,iun
      INTEGER sel(nobs)
      DOUBLE PRECISION csinor,csi(2*nobs),gamma(6,6),w(2*nobs),x2(nobs)
      DOUBLE PRECISION mjd(nobs)

* NEEDED common blocks:
      INCLUDE 'comrej.h'
      INCLUDE 'codode.h'

      INTEGER iob,ipa,ipd,i,k,nsel,minobs,selp,nrej,nrec
      DOUBLE PRECISION sa,sd,sad,gamga(6),gamgd(6),covres(2,2),wres(2,2)
      DOUBLE PRECISION det,x2max
      DOUBLE PRECISION covr,wrr
c calendar date variables
      INTEGER iyear,imonth,iday
      DOUBLE PRECISION hour
      CHARACTER*16 date
      LOGICAL entopp
c fudge factor, stuck loops
      DOUBLE PRECISION fudge
      INTEGER nit,oldmod,oldnob

      SAVE nit,oldmod,oldnob
      DATA nit/0/

      IF(iicrej.NE.36) STOP '**** reject: internal error (01) ****'
      log=.true.
      IF(unilog.le.0) log=.false.

      nrej=0
      nrec=0
*****************************************************
* First compute chi**2 for each obs
*****************************************************
* Minimum number of observations
*     First count solved for params
      minobs=6
      DO 10 i=1,6
         IF(icor(i).NE.1) minobs=minobs-1
 10   CONTINUE
*     Next set minobs
      minobs=nint(0.5d0*minobs)
      IF(nobs.le.minobs)THEN
         WRITE(*,*)'No autorejection with only ',nobs,' observations.'
         nmod=0
         RETURN
      ENDIF

      x2max=0
      nsel=0
      iun=abs(unilog)
      WRITE(iun,300) csinor
      IF(log) WRITE(*,300) csinor
 300  FORMAT('==== OUTLIER REJECTION ======='/
     +        'Residual norm =',1P,E13.5,0P)
      DO 1 iob=1,nobs
* Pointers to RA and DEC observations
         ipa=2*iob-1
         ipd=2*iob
* Expected variance of fit residuals
         DO 2 i=1,6
            sa=0
            sd=0
            DO 3 k=1,6
               sa=sa+gamma(i,k)*g(ipa,k)
               sd=sd+gamma(i,k)*g(ipd,k)
 3          CONTINUE
            gamga(i)=sa
            gamgd(i)=sd
 2       CONTINUE
c separate case with zero weight in one of the two components
         IF(w(ipa).gt.0.d0.and.w(ipd).gt.0.d0)THEN
c both components to be used
            sa=0
            sd=0
            sad=0
            DO  i=1,6
               sa =sa +g(ipa,i)*gamga(i)
               sd =sd +g(ipd,i)*gamgd(i)
               sad=sad+g(ipa,i)*gamgd(i)
            ENDDO
            IF(sel(iob).EQ.0) THEN
               covres(1,1)=1/w(ipa)+sa
               covres(2,2)=1/w(ipd)+sd
               covres(1,2)=+sad
            ELSE
               covres(1,1)=1/w(ipa)-sa
               covres(2,2)=1/w(ipd)-sd
               covres(1,2)=-sad
            END IF
            covres(2,1)=covres(1,2)
* Chi-square value
            CALL inv22(covres,wres,det)
            IF (det.eq.0.d0)THEN
               WRITE(ierrou,*) 'WARNING: reject.f, det=0.'
               numerr=numerr+1
            ENDIF
            x2(iob)=csi(ipa)*wres(1,1)*csi(ipa)
     +           +csi(ipd)*wres(2,2)*csi(ipd)
     +           +2*csi(ipa)*wres(1,2)*csi(ipd)
            
         ELSEIF(w(ipa).eq.0.d0)THEN
c only second component to be used
            sd=0
            DO  i=1,6
               sa =sa +g(ipa,i)*gamga(i)
               sd =sd +g(ipd,i)*gamgd(i)
               sad=sad+g(ipa,i)*gamgd(i)
            ENDDO
            IF(sel(iob).EQ.0) THEN
               covr=1/w(ipd)+sd
            ELSE
               covr=1/w(ipd)-sd
            END IF
* Chi-square value
            IF (covr.eq.0.d0)THEN
               WRITE(ierrou,*) 'WARNING: reject.f, covr=0. (1)'
               numerr=numerr+1
            ENDIF
            wrr=1.d0/covr
            x2(iob)=csi(ipd)*wrr*csi(ipd)
         ELSEIF(w(ipd).eq.0.d0)THEN
c only first component to be used
            sa=0
            DO  i=1,6
               sa =sa +g(ipa,i)*gamga(i)
            ENDDO
            IF(sel(iob).EQ.0) THEN
               covr=1/w(ipa)+sa
            ELSE
               covr=1/w(ipa)-sa
            END IF
* Chi-square value
            IF (covr.eq.0.d0)THEN
               WRITE(ierrou,*) 'WARNING: reject.f, covr=0. (2)'
               numerr=numerr+1
            ENDIF
            wrr=1.d0/covr
            x2(iob)=csi(ipa)*wrr*csi(ipa)
         ELSE
            WRITE(*,*)'reject: this should not happen ',
     +           w(ipa),' ',w(ipd)
            STOP
         ENDIF
         IF (x2(iob).lt.0.d0)THEN
            WRITE(ierrou,*) 'WARNING: reject.f, x2=',x2(iob)
            WRITE(*,*) 'WARNING: reject.f, x2=',x2(iob)
            numerr=numerr+1
            x2(iob)=0.d0
         ENDIF
         IF(sel(iob).NE.0) THEN
            x2max=MAX(x2(iob),x2max)
            nsel=nsel+1
         END IF
 1    CONTINUE
*****************************************************
* Second perform tests
*****************************************************

      IF(nsel.LE.0) STOP '**** reject: internal error (02) ****'

*     For <=18 obs. or for inf. loop -> reject one at a time
      IF(nsel .le. minobs*6) THEN
         IF(log) WRITE(*,*) 'Rejecting no more than one.'
      ELSEIF(nit.ge.4) THEN
         IF(log) WRITE(*,*) 'Trying to get unstuck.'
      ELSE
         x2max=x2max*x2frac
      ENDIF
      iun=abs(unilog)
      WRITE(iun,302) x2max
      IF(log) WRITE(*,302) x2max
 302  FORMAT('Chi2 threshold =',1P,E13.5)
c This is a "fudge" factor to make it more difficult to reject when 
c there are very few observations. If there are more than ~50 than 
c this has little effect.
      fudge=400.d0 * (1.2)**(-nsel)

      DO 5 iob=1,nobs
         selp=sel(iob)
         IF(selp.EQ.0) THEN
            IF(x2(iob).LE.x2rec + 0.75*fudge) THEN
               call mjddat(mjd(iob),iday,imonth,iyear,hour)
               write(date,'(i4,a1,i2.2,a1,f8.5)')
     +              iyear,'/',imonth,'/',iday+hour/24d0
               write(*,*) 'Recover: ',date,x2(iob)
               sel(iob)=1
               nrec=nrec+1
            END IF
         ELSE
            IF(x2(iob).GE.x2max .AND. x2(iob).GT.x2rej + fudge) THEN
c check to see if this is the only remaining obs in an opposition
               call mjddat(mjd(iob),iday,imonth,iyear,hour)
               write(date,'(i4,a1,i2.2,a1,f8.5)')
     +              iyear,'/',imonth,'/',iday+hour/24d0
               write(*,*) 'Reject : ',date,x2(iob)
               if(entopp(iob,sel,mjd,nobs))then
                  if(rejopp)then
                     write(*,*)
     +                '*** WARNING: Rejecting last obs. in opp. Date:',
     +                  date
                     write(ierrou,*)
     +                    'Rejecting last obs. in opp. Date:',date
                     numerr=numerr+1
                     sel(iob)=0
                     nrej=nrej+1                     
                  else
                     write(*,*)
     +            '*** WARNING: Did not reject last obs. in opp. Date:',
     +                    date
                     write(ierrou,*)
     +                    'Did not reject last obs. in opp. Date:',date
                     numerr=numerr+1
                  endif
               else
                  sel(iob)=0
                  nrej=nrej+1
               endif
            END IF
         END IF
         IF(debug.and.log) WRITE(unilog,301) iob,x2(iob),selp,sel(iob)
 5    CONTINUE
 301  FORMAT(I5,1P,E13.5,0P,2I3)
      iun=abs(unilog)
      WRITE(iun,303) nobs,nsel,nrej,nrec
      IF(log) WRITE(*,303) nobs,nsel,nrej,nrec
 303  FORMAT('No. of observations:'/
     +       10X,'total     =',I5/
     +       10X,'selected  =',I5/
     +       10X,'rejected  =',I5/
     +       10X,'recovered =',I5)

      nmod=nrej+nrec
c     Is it the same no. of modifications with the same number of obs?
      IF(oldmod.EQ.nmod .AND. nobs.EQ.oldnob)THEN
         nit=nit+1
      ELSE
         nit=0
      ENDIF
      oldmod=nmod
      oldnob=nobs

      END
c
c decide if index is the last selected obs in an opposition
      logical function entopp(index,sel,mjd,nobs)      
      integer index,nobs,sel(nobs),i
      double precision mjd(nobs)
      entopp=.false.
      do i=1,nobs
c return if we find a different selected obs that is close enough
         if(i.ne.index.and.
     +        abs(mjd(i)-mjd(index)).le.180d0.and.
     +        sel(i).eq.1) return
      enddo
      entopp=.true.
      return
      end
