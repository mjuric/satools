* Copyright 1999 Orbfit Consortium
* Written by Milani and Chesley
* Last Modified 1/20/99 by S. Chesley
************************************************************************
* SUBROUTINE MAGEST
************************************************************************
* INPUTS:  smag -     magnitude strings e.g., '15.55V'
*          rmsmag -   a priori rms of the observation (NOTE: rmsmag will 
*                        be changed to 99.99 for outlier rejection)
*          sel -      selection flag for the astrometric observation
*          m -        total number of observations
* OUTPUTS: h0 -       absolute magnitude
*          resmag -   residual of the mag observation (NOTE: If resmag =  
*                        1.d9 then the mag obs was not used in the fit.)
*          rmsh -     weighted RMS of the residuals
************************************************************************
      SUBROUTINE magest(smag,rmsmag,sel,m,h0,resmag,rmsh)
      IMPLICIT NONE
c number of observations
      INTEGER m
c magnitudes: estimated values, observed (string), a priori rms
      DOUBLE PRECISION h0
      CHARACTER*6 smag(m)
      DOUBLE PRECISION rmsmag(m)
c fit residuals, rms
      DOUBLE PRECISION resmag(m),rmsh
c selection flags, no. observations used and weights
      INTEGER sel(m)
c ===========END INTERFACE=================
c get nobx
      INCLUDE 'parobx.h'
c get x2mrej,x2mrec
      INCLUDE 'comrej.h'
c interactive?
      INCLUDE 'comdif.h'
c observed magnitudes, mean
      DOUBLE PRECISION obsm(nobx),hmean,hsum,wsum,wrsum
      DOUBLE PRECISION tmprms(nobx),hnew,chi2
      LOGICAL avail(nobx)
      CHARACTER*1 col
      CHARACTER*5 smag5
      INTEGER numrej,numchg
c debugging:
      LOGICAL verbose
c common magnitude data
c dmagn is difference apparent minus absolute magnitude
      include 'mag.h'
c error file and number
      INCLUDE 'proout.h'
c function to compute default mag RMS
      DOUBLE PRECISION magrms
c loop indexes
      INTEGER j,icount,nrej
c ======================================================================
      verbose = .false.
c First create a vector of appmags and rms's
      DO j=1,m
         READ(smag(j),101)col
 101     FORMAT(5x,a1)
         avail(j)=smag(j).ne.'      '.and.
     +        (col.eq.'B'.or.col.eq.'V'.or.col.eq.'R'.or.
     +        col.eq.'I'.or.col.eq.' ')
         IF(.not.(col.eq.'B'.or.col.eq.'V'.or.col.eq.'R'.or.
     +        col.eq.'I'.or.col.eq.' '))THEN
            IF(ierrou.gt.0)THEN
               WRITE(ierrou,*) 'Unknown Color:',col,' Obs #',j
               numerr=numerr+1
            ELSE
               WRITE(*,*) 'Unknown Color:',col,' Obs #',j
            ENDIF
         ENDIF
         IF(avail(j))THEN
c Get the default rms (to be used for recovery)
c The last three arguments are dummies.
            tmprms(j)=magrms(smag(j),0d0,0,'0')
c Read the magnitude and make color corrections.
            smag5=smag(j)
            READ(smag5,*,ERR=3)obsm(j)
            IF(col.eq.'V')THEN
               CONTINUE
            ELSEIF(col.eq.' ')THEN
               obsm(j)=obsm(j)-0.8d0
            ELSEIF(col.eq.'B')THEN
               obsm(j)=obsm(j)-0.8d0
            ELSEIF(col.eq.'R')THEN
               obsm(j)=obsm(j)+0.4d0
            ELSEIF(col.eq.'I')THEN
               obsm(j)=obsm(j)+0.8d0
            ENDIF
         ENDIF
      ENDDO
c Start Outlier loop
      DO nrej=1,10
         hsum=0.d0
         icount=0
         wsum=0.d0
c main loop
         DO j=1,m
            IF(avail(j))THEN
               icount=icount+1
               hsum=hsum+(obsm(j)-dmagn(j))/rmsmag(j)
               wsum=wsum+1.d0/rmsmag(j)
            ENDIF
         ENDDO
c jump out if no good observations
         IF(icount.eq.0)THEN
            WRITE(*,*)' magnitude cannot be estimated w/o obs'
            rmsh=-1.d0
            RETURN
         ENDIF             
c Compute H, etc.
         hmean=hsum/wsum
         rmsh=0.d0
         wrsum=0.d0
         DO j=1,m
            IF(avail(j))THEN
               resmag(j)=obsm(j)-dmagn(j)-hmean
               rmsh=rmsh+(resmag(j)/rmsmag(j))**2
               wrsum=wrsum+(1.d0/rmsmag(j))**2
            ELSE
c              if data not used
               resmag(j)=1.d9
            ENDIF
         ENDDO
C Divide by icount or wsum here?
         rmsh=sqrt(rmsh/wrsum)
         IF(verbose) write(*,*)'H,RMS',hmean,rmsh
c Handle outliers in a separate loop
         numrej=0
         numchg=0
c Never reject if there are less than 5 obs.
         IF(icount.le.4) GOTO 10
         DO j=1,m
            IF(avail(j))THEN
               IF(rmsmag(j).gt.99.98d0)THEN
C              test for recovery
                  hnew=(hsum+(obsm(j)-dmagn(j))/tmprms(j))/
     +                 (wsum+1.d0/tmprms(j))
                  chi2=(((obsm(j)-dmagn(j))-hnew)/tmprms(j))**2
                  IF(chi2.lt.x2mrec)THEN
                     numchg=numchg+1
                     rmsmag(j)=tmprms(j)
                  ENDIF
                  IF(verbose) write(*,*)'REC:Hnew,chi2,rms,res',
     +                 hnew,chi2,rmsmag(j),resmag(j)
               ELSE
C              test for rejection
                  hnew=(hsum-(obsm(j)-dmagn(j))/rmsmag(j))/
     +                 (wsum-1.d0/rmsmag(j))
                  chi2=(((obsm(j)-dmagn(j))-hnew)/rmsmag(j))**2
                  IF(chi2.gt.x2mrej)THEN
                     numchg=numchg+1
                     rmsmag(j)=99.99d0
                  ENDIF
                  IF(verbose) write(*,*)'REJ:Hnew,chi2,rms,res',
     +                 hnew,chi2,rmsmag(j),resmag(j)
               ENDIF
C              count rejection
               IF(rmsmag(j).gt.99.98d0) numrej=numrej+1
            ENDIF
         ENDDO
         IF(numchg.eq.0) GOTO 10
      ENDDO
      WRITE(*,*)'WARNING: Did not finish rejecting photometry...'
************************************************************************
 10   IF(.not.batch)THEN
         WRITE(*,190)h0,hmean,icount,rmsh,numrej,nrej
 190     FORMAT('Absol Magnitude: Old=',f5.2,' New=',f5.2,/
     +        'no. magnitude obs.=',i4, ' with RMS ',f5.2,/
     +        'no. rejected obs.=',i4,' in ',i4,' passes.')
         WRITE(*,*)
      ENDIF
      h0=hmean
      RETURN

c parsing error
 3    WRITE(*,*)'magest: error in parsing magn. no ',j,' ',smag(j)
      STOP
      END
