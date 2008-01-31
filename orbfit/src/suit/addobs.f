c =========================================
c  A D D O B S
c
c add information from .obs and .rad
c The number of observations can increase, from m to a maximum
c which is m+mt; however, the dimensioning of the vectors
c is nlef
      SUBROUTINE addobs(     
     +           objid,iobs,tau,tut,idsta,
     +           aln,rmsa,den,rmsd,smag,rmsmag,sel,m,
     +           objidt,iobst,taut,tutt,idstat,
     +           alnt,rmsat,dent,rmsdt,smagt,rmsmagt,mt,
     +           nlef,mnew,change)
      IMPLICIT NONE
c dummy variables length
      INTEGER nlef
c ===============================================
c change flag, new obs. number
      LOGICAL change
      INTEGER mnew
c ===== observational data ===========================
c observation number
      INTEGER m
c observations: alpha, delta, time (ET and UT), station code, type
      DOUBLE PRECISION aln(nlef),den(nlef),tau(nlef),tut(nlef)
      INTEGER idsta(nlef),iobs(nlef)
c identifier, app. magnitude
      CHARACTER*9 objid(nlef)
      CHARACTER*6 smag(nlef) 
c selection flag 0=discard 1=select 2=prelim
      INTEGER sel(nlef)
c RMS of observation error, of magnitude
      DOUBLE PRECISION rmsa(nlef),rmsd(nlef),rmsmag(nlef),rmseps
c ===== observational data: temporary copy===========
c observation number
      INTEGER mt
c observations: alpha, delta, time (ET and UT), station code, type
      DOUBLE PRECISION alnt(mt),dent(mt),taut(mt),tutt(mt)
      INTEGER idstat(mt),iobst(mt)
c identifier, app. magnitude
      CHARACTER*9 objidt(mt)
      CHARACTER*6 smagt(mt) 
c RMS of observation error, of magnitude
      DOUBLE PRECISION rmsat(mt),rmsdt(mt),rmsmagt(mt)
c ===========================================
      INTEGER j,mj,findob,double
      LOGICAl chaobs
c error file, number of errors
      INCLUDE 'proout.h'
      INCLUDE 'trig.h'
      INCLUDE 'jplhdr.h'
c ==================================================
c monitor changes
      mnew=m
      change=.false. 
c scan supposedly new observations
      DO 1 j=1,mt
         mj=findob(tutt(j),tut,idstat(j),idsta,iobst(j),iobs,double,m)
         IF(mj.ne.0.and.double.eq.0)THEN
c     the observation was already there
            IF(chaobs(alnt(j),aln(mj),dent(j),den(mj),
     +           smagt(j),smag(mj),iobst(j),iobs(mj)) )THEN 
c ... but it is changed 
c               write(*,*)'change',j,mj,alnt(j),aln(mj),dent(j),den(mj),
c     +           smagt(j),smag(mj),iobst(j),iobs(mj)
               change=.true.
               aln(mj)=alnt(j)
               den(mj)=dent(j)
               smag(mj)=smagt(j)
               rmsa(mj)=rmsat(j)
               rmsd(mj)=rmsdt(j)
               rmsmag(mj)=rmsmagt(j)
               sel(mj)=1
               iobs(mj)=iobst(j)
               IF(ierrou.gt.0)THEN
                  WRITE(ierrou,*)'addobs: changed observ. at record ',mj
                  numerr=numerr+1
               ELSE
                  WRITE(*,*)'addobs: changed observation at record ',mj
               ENDIF
            ELSE
c do not give precedence to rwo weighting unless rwo weights are negative
c cutoff is 5 milliarcsec or 1 meter (radar) for new weights
               if(iobs(mj)/1000.eq.2)then
                  rmseps=0.0005/au
               elseif(iobs(mj)/1000.eq.1)then
                  rmseps=0.0055*radsec
               endif
               if(rmsa(mj).ge.0d0.and.
     +              abs(rmsa(mj)-rmsat(j)).gt.rmseps)then
                  change=.true.
c                  write(*,'(a,4g16.8)')'alf',rmseps,
c     +                 rmsa(mj)-rmsat(j),rmsa(mj),rmsat(j)
                  rmsa(mj)=rmsat(j) 
               endif  
               if(rmsd(mj).ge.0d0.and.
     +              abs(rmsd(mj)-rmsdt(j)).gt.rmseps)then
c                  write(*,'(a,4g16.8)')'del',rmseps,
c     +                 rmsd(mj)-rmsdt(j),rmsd(mj),rmsdt(j)
                   change=.true.
                  rmsd(mj)=rmsdt(j) 
               endif  
            ENDIF
         ELSEIF(mj.ne.0.and.double.ne.0)THEN
c the observation was already there, in double copy!
            IF(chaobs(alnt(j),aln(mj),dent(j),den(mj),smagt(j),smag(mj)
     +           ,iobst(j),iobs(mj)) ) THEN 
               IF(chaobs(alnt(j),aln(double),dent(j),den(double),
     +              smagt(j),smag(double),iobst(j),iobs(double)))THEN 
                  change=.true.
c double, and changed! human intervention required
                  WRITE(*,*)'addobs: double and changed'
                  WRITE(*,*)' records ',mj,' and ',double,' in .rwo'
                  WRITE(*,*)' record ',j,' in .obs'
c                STOP
               ELSE
c OK, it is the double
               ENDIF
            ELSE
c OK, it is the first one
            ENDIF  
         ELSEIF(mj.eq.0)THEN
c the observation is new: add it
           change=.true.
c           WRITE(*,*)'addobs: new observation at record ',j
           mnew=mnew+1
           aln(mnew)=alnt(j)
           den(mnew)=dent(j)
           smag(mnew)=smagt(j)
           rmsa(mnew)=rmsat(j)
           rmsd(mnew)=rmsdt(j)
           rmsmag(mnew)=rmsmagt(j)
           tau(mnew)=taut(j)
           tut(mnew)=tutt(j)
           idsta(mnew)=idstat(j)
           iobs(mnew)=iobst(j)
           sel(mnew)=1
           objid(mnew)=objidt(j)
        ENDIF
 1    ENDDO
      RETURN
      END
c =======================================================
c F I N D O B
c find an observation from a list, matching the given one
      INTEGER FUNCTION findob(tutt,tut,idstat,idsta,iobst,iobs,double,m)
      IMPLICIT NONE
c ============= INPUT =====================
c number of obs. record to be scanned, time, time to be found
      INTEGER m
      DOUBLE PRECISION tut(m),tutt
c station code vector, of the one to be found
      INTEGER idsta(m),idstat,iobs(m),iobst
c ============OUTPUT (impure function!) ===========
      INTEGER double
c error number, file
      INCLUDE 'proout.h'
c =========END INTERFACE=====================
      INTEGER j
      DOUBLE PRECISION epst
      PARAMETER (epst=1.d-8)
      findob=0
      double=0
      DO 1 j=1,m
        IF(abs(tutt-tut(j)).lt.epst.and.
     +      idsta(j).eq.idstat.and.iobs(j).eq.iobst)THEN
           IF(findob.ne.0)THEN
              IF(double.eq.0)THEN
                 double=j
                 IF(ierrou.gt.0)THEN
                 WRITE(ierrou,*)'findob: two same time',
     +                findob,j,tutt,idsta(j)
                 numerr=numerr+1
                 ELSE
                    WRITE(*,*)'findob: two same time',
     +                findob,j,tutt,idsta(j)
                 ENDIF
              ELSE
                 IF(ierrou.gt.0)THEN
                    WRITE(ierrou,*)'findob: three same time',
     +                   findob,double,j,tutt,idsta(j)
                 ELSE
                    WRITE(*,*)'findob: three same time',
     +                   findob,double,j,tutt,idsta(j)
                 ENDIF
c                STOP
              ENDIF
           ELSE
              findob=j
           ENDIF
        ENDIF
 1    ENDDO
      RETURN
      END
c =======================================================
c C H A O B S
c is an observation changed?
      LOGICAL FUNCTION chaobs(aln,alnt,dent,den,smagt,smag,iobs,iobst)
      IMPLICIT NONE
      DOUBLE PRECISION aln,alnt,den,dent
      CHARACTER*6 smag,smagt
      INTEGER iobst,iobs
      DOUBLE PRECISION epsa
      PARAMETER (epsa=1.d-9)
      chaobs=.false.
      IF(abs(aln-alnt).gt.epsa*abs(aln).or.
     +      abs(den-dent).gt.epsa*abs(den))chaobs=.true.
      IF(smagt.ne.smag.or.iobs.ne.iobst)chaobs=.true.
      RETURN
      END
c =========================================
c  A D D R W O
c
c add information from .rwo file (weights, selection flags)
c to the observations from .obs and .rad
c The number of observations cannot increase
      SUBROUTINE addrwo(
     +           objid,iobs,tut,idsta,
     +           aln,rmsa,den,rmsd,smag,rmsmag,sel,m,
     +           objidt,iobst,tutt,idstat,
     +           alnt,rmsat,dent,rmsdt,smagt,rmsmagt,selt,mt,
     +           change)
      IMPLICIT NONE
c logical change flag
      LOGICAl change
c ===== observational data ===========================
c observation number
      INTEGER m
c observations: alpha, delta, time (ET and UT), station code, type
      DOUBLE PRECISION aln(m),den(m),tut(m)
      INTEGER idsta(m),iobs(m)
c identifier, app. magnitude
      CHARACTER*9 objid(m)
      CHARACTER*6 smag(m) 
c selection flag 0=discard 1=select 2=prelim
      INTEGER sel(m)
c RMS of observation error, of magnitude
      DOUBLE PRECISION rmsa(m),rmsd(m),rmsmag(m),rmseps
c ===== observational data: temporary copy===========
c observation number
      INTEGER mt
c observations: alpha, delta, time (ET and UT), station code, type
      DOUBLE PRECISION alnt(mt),dent(mt),tutt(mt)
      INTEGER idstat(mt),iobst(mt)
c identifier, app. magnitude
      CHARACTER*9 objidt(mt)
      CHARACTER*6 smagt(mt) 
c selection flag 0=discard 1=select 2=prelim
      INTEGER selt(mt)
c RMS of observation error, of magnitude
      DOUBLE PRECISION rmsat(mt),rmsdt(mt),rmsmagt(mt)
c ===========================================
      INTEGER j,mj,findob,double
      LOGICAl chaobs
      INCLUDE 'trig.h'
      INCLUDE 'jplhdr.h'
c if all the same...
      change=.false.
c scan old weigts and selection flags, see if they match  observations
      DO 1 j=1,mt
        mj=findob(tutt(j),tut,idstat(j),idsta,iobst(j),iobs,double,m)
        IF(mj.ne.0.and.double.eq.0)THEN
c this observation is still present in .obs, .rad files
           IF(chaobs(alnt(j),aln(mj),dent(j),den(mj),smagt(j),smag(mj)
     +        ,iobst(j),iobs(mj)))THEN 
c ... but it is changed, thus leave selection flag=1 and default weight
              change=.true.
           ELSE
c do not give precedence to rwo weighting unless rwo weights are negative
c cutoff is 5 milliarcsec or 1 meter for new weights
c here we just set the change flag to accept the computed weights
              if(iobs(mj)/1000.eq.2)then
                 rmseps=0.0005/au
              elseif(iobs(mj)/1000.eq.1)then
                 rmseps=0.0055*radsec
              endif
              if(rmsat(j).ge.0d0.and.
     +             abs(rmsat(j)-rmsa(mj)).gt.rmseps)then
                 change=.true.
              endif  
              if(rmsdt(j).ge.0d0.and.
     +             abs(rmsdt(j)-rmsd(mj)).gt.rmseps)then
                 change=.true.
              endif  
c if no change to observation then preserve the 
c manually fixed weights and selection flags
              IF(rmsat(j).lt.0.d0)rmsa(mj)=rmsat(j)
              IF(rmsdt(j).lt.0.d0)rmsd(mj)=rmsdt(j)
              IF(rmsmagt(j).lt.0.d0)rmsmag(mj)=rmsmagt(j)
c selection flags are preserved anyway
              sel(mj)=selt(j)
           ENDIF
        ELSEIF(mj.ne.0.and.double.ne.0)THEN
c this observation is present in .obs, .rad files, in duplicate!
           IF(chaobs(alnt(j),aln(mj),dent(j),den(mj),smagt(j),smag(mj)
     +        ,iobst(j),iobs(mj)))
     +     THEN 
              IF(chaobs(alnt(j),aln(double),dent(j),den(double),
     +             smagt(j),smag(double),iobst(j),iobs(double)))THEN
                 change=.true.
c double, and changed! human intervention required
                 WRITE(*,*)'addobs: double and changed'
                 WRITE(*,*)' records ',mj,' and ',double,' in .obs'
                 WRITE(*,*)' record ',j,' in .rwo'
c                STOP
              ELSE
c OK, it is the duplicate
c it is the same, so preserve the weights and selection flags
                 rmsa(double)=rmsat(j)
                 rmsd(double)=rmsdt(j)
                 rmsmag(double)=rmsmagt(j)
                 sel(double)=selt(j)
              ENDIF
           ELSE
c OK, it is the first one
c it is the same, so preserve the weights and selection flags
              rmsa(mj)=rmsat(j)
              rmsd(mj)=rmsdt(j)
              rmsmag(mj)=rmsmagt(j)
              sel(mj)=selt(j)
           ENDIF
        ELSEIF(mj.eq.0)THEN
c if it is not found in .rwo, leave the default weights and selection flags
           change=.true.
        ENDIF
 1    ENDDO
c check if there are extra (added) observations in .obs file
c it might be better to loop on the .obs rather than the .rwo data
      IF (mt.lt.m) change=.true.
      RETURN
      END
