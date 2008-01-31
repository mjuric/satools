c ===========================================
c INOBS
c observation input control routine
c reads sequentially the .rwo, .obs, .rad file and combines
c the data according to the precedence rule specified by precob
c
      SUBROUTINE inobs(obsdir,astna0,precob,objid,obs0,m,iobs,tau,
     +    aln,den,tut,idsta,sel,rmsa,rmsd,rmsmag,smag,nlef,iun20,change)
      IMPLICIT NONE
c ==============INPUT==================
c input directory (all files astna0.rwo, astna0.obs, astna0.rad must be there)
      CHARACTER*60 obsdir
c asteroid name
      CHARACTER*(*) astna0
c messages unit
      INTEGER iun20
c observation numbers: maximum, space left
      INCLUDE 'parobx.h'
      INTEGER nlef
c logical flag: .true. for overwrite .rwo, .false. for update .rwo
      LOGICAL precob
c =============OUTPUT================================
c successful input flag
      LOGICAL obs0,change
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
      DOUBLE PRECISION rmsa(nlef),rmsd(nlef),rmsmag(nlef)
c ===========END INTERFACE=========================
c file names
      CHARACTER*77 file
      INTEGER lfile
      LOGICAL rwo,mpc,rad
c accuracy (?) of observational data from mpcin
      double precision acct(nobx),acca(nobx),accd(nobx)
c new obs. number
      INTEGER mnew,mr,nlefm
c ===== observational data: temporary copy===========
c observation number
      INTEGER mt
c observations: alpha, delta, time (ET and UT), station code, type
      DOUBLE PRECISION alnt(nobx),dent(nobx),taut(nobx),tutt(nobx)
      INTEGER idstat(nobx),iobst(nobx)
c identifier, app. magnitude
      CHARACTER*9 objidt(nobx)
      CHARACTER*6 smagt(nobx) 
c selection flag 0=discard 1=select 2=prelim
      INTEGER selt(nobx)
c RMS of observation error, of magnitude
      DOUBLE PRECISION rmsat(nobx),rmsdt(nobx),rmsmagt(nobx)
c ============residuals and other data from rearwo============
      DOUBLE PRECISION resa(nobx),resd(nobx),chi2(nobx)
      DOUBLE PRECISION resmag(nobx)
c sorting 
      INTEGER iperm(nobx)
c directory char
      INCLUDE 'sysdep.h'
c loop index
      INTEGER i,j
      INTEGER ld
      INTEGER lench
      EXTERNAL lench
c =============EXECUTION BEGINS======================
c  compute file name
      ld=lench(obsdir)
      IF(ld.GT.0) THEN
          IF(obsdir(ld:ld).EQ.dircha) THEN
              file=obsdir(1:ld)//astna0
          ELSE
              file=obsdir(1:ld)//dircha//astna0
          END IF
      ELSE
          file=astna0
      END IF
      CALL rmsp(file,lfile)
c existence of .rwo, .obs, .rad
      INQUIRE(file=file(1:lfile)//'.rwo',exist=rwo)
      INQUIRE(file=file(1:lfile)//'.obs',exist=mpc)
      INQUIRE(file=file(1:lfile)//'.rad',exist=rad)
c For now we will not do radar only orbits
      IF(.not.rwo .and. .not. mpc)THEN
         WRITE(*,*)'You must provide either a .obs or .rwo file',
     +        'in directory ',obsdir
         obs0=.false.
         RETURN
      ENDIF
c select operations mode
      IF(.not.rwo)THEN
         WRITE(*,*) 'No .rwo file, so reading .obs and/or .rad files.'
c there is no .rwo, so read .obs and/or .rad
         IF(mpc)THEN
c Input of astrometric observations from a file (MPC format)
            CALL mpcin(mpc,file(1:lfile)//'.obs',objid,iobs,tau,tut,
     +           aln,den,idsta,acct,acca,accd,smag,m,nlef)
            WRITE(*,*)'mpcin: ',m,' obs in ',file(1:lfile)//'.obs'
            WRITE(iun20,*)'mpcin: ',m,' obs in ',file(1:lfile)//'.obs'
         ENDIF
c read radar jpl data
         IF(rad)THEN
            nlefm=nlef-m
            CALL jplin(rad,file(1:lfile)//'.rad',objid(m+1),iobs(m+1),
     +           tau(m+1),tut(m+1),aln(m+1),den(m+1),idsta(m+1),
     +           acct(m+1),acca(m+1),accd(m+1),smag(m+1),mr,nlefm)
            m=mr+m
         ENDIF
         obs0=mpc.or.rad
         IF(.not.obs0)RETURN
c find weights for these; a priori RMS of astrometric observations
         CALL obsrms(iobs,idsta,tau,acct,acca,accd,smag,
     +             rmsa,rmsd,rmsmag,m)
c give default selection flag of 1
         DO j=1,m
           sel(j)=1
         ENDDO
c output data for possible manual fixing: create weights file
         CALL wrirms(file(1:lfile)//'.rwo',objid,iobs,tut,idsta,
     +           aln,rmsa,den,rmsd,smag,rmsmag,sel,m)
         change=.true.
c select between update and overwrite of .rwo
      ELSEIF(precob)THEN
         WRITE(*,*)file(1:lfile),
     +        '.rwo found but ALL obs will come from .obs/.rad files.'
c give the precedence to the observation files .obs and .rad 
c with respect to .rwo, which is overwritten
         IF(mpc)THEN
c Input of astrometric observations from a file (MPC format)
            CALL mpcin(mpc,file(1:lfile)//'.obs',objid,iobs,tau,tut,
     +           aln,den,idsta,acct,acca,accd,smag,m,nlef)
            WRITE(*,*)'mpcin: ',m,' obs in ',file(1:lfile)//'.obs'
            WRITE(iun20,*)'mpcin: ',m,' obs in ',file(1:lfile)//'.obs'
         ELSE
            m=0
         ENDIF
c read radar jpl data
         IF(rad)THEN
            nlefm=nlef-m
            CALL jplin(rad,file(1:lfile)//'.rad',objid(m+1),iobs(m+1),
     +           tau(m+1),tut(m+1),aln(m+1),den(m+1),idsta(m+1),
     +           acct(m+1),acca(m+1),accd(m+1),smag(m+1),mr,nlefm)
            m=mr+m
         ENDIF
         obs0=rad.or.mpc
c If no obs then object does not "exist", so rwo should mnot be read:
         if(.not.obs0)return
c find weights for these; a priori RMS of astrometric observations
         CALL obsrms(iobs,idsta,tau,acct,acca,accd,smag,
     +             rmsa,rmsd,rmsmag,m)
c give default selection flag of 1
         DO j=1,m
           sel(j)=1
         ENDDO
c read .rwo  anyway, but store in temporary array the data
         CALL rearwo(file(1:lfile)//'.rwo',
     +     objidt,iobst,taut,tutt,idstat,
     +     alnt,rmsat,resa,dent,rmsdt,resd,
     +     smagt,rmsmagt,resmag,
     +     selt,chi2,mt,nlef)
c recover informations from .rwo (weights, selection flags)
         CALL addrwo(
     +           objid,iobs,tut,idsta,
     +           aln,rmsa,den,rmsd,smag,rmsmag,sel,m,
     +           objidt,iobst,tutt,idstat,
     +           alnt,rmsat,dent,rmsdt,smagt,rmsmagt,selt,mt,change)
         IF(change)THEN
            WRITE(*,*)'There are new/changed obs. New numobs=',m
c output updated .rwo file, if there are new observations (erasing residuals)
            CALL wrirms(file(1:lfile)//'.rwo',objid,iobs,tut,idsta,
     +           aln,rmsa,den,rmsd,smag,rmsmag,sel,m)
         ELSE
            WRITE(*,*)'There are no updates in .obs or .rad files.'
         ENDIF
      ELSE
c give the precedence to .rwo, the .obs and .rad files are intended
c as additional observations only; data in .rwo are not erased, can only be
c changed
c read .rwo, and store data in final array
         WRITE(*,*)'Using .rwo file, but checking .obs,.rad for update.'
         CALL rearwo(file(1:lfile)//'.rwo',
     +     objid,iobs,tau,tut,idsta,
     +     aln,rmsa,resa,den,rmsd,resd,
     +     smag,rmsmag,resmag,
     +     sel,chi2,m,nlef)
         WRITE(*,*)'rearwo: ',m,' obs from  ',file(1:lfile)//'.rwo'
         WRITE(iun20,*)'rearwo: ',m,' obs from ',file(1:lfile)//'.rwo'
         IF(m.eq.0)THEN
            obs0=.false.
         ELSE
            obs0=.true.
         ENDIF
c if there are input data
         IF(mpc)THEN
c Input of astrometric observations into temporary from a file (MPC format)
            CALL mpcin(mpc,file(1:lfile)//'.obs',
     +           objidt,iobst,taut,tutt,
     +           alnt,dent,idstat,acct,acca,accd,smagt,mt,nobx)
            IF(.not.mpc)THEN
               WRITE(*,*) file(1:lfile)//'.obs is possibly corrupt. ',
     +              'Not using any data from this file.'
            ELSE
               WRITE(*,*)'mpcin:',mt,' obs from  ',file(1:lfile)//'.obs'
               WRITE(iun20,*)'mpcin:',mt,' from ',file(1:lfile)//'.obs'
            ENDIF
         ENDIF
c read radar jpl data
         IF(rad)THEN
            nlefm=nobx-mt
            CALL jplin(rad,file(1:lfile)//'.rad',objidt(mt+1),
     +           iobst(mt+1),taut(mt+1),tutt(mt+1),alnt(mt+1),
     +           dent(mt+1),idstat(mt+1),acct(mt+1),acca(mt+1),
     +           accd(mt+1),smagt(mt+1),mr,nlefm)
            IF(.not.rad)THEN
               WRITE(*,*) file(1:lfile)//'.rad is possibly corrupt. ',
     +              'Not using any data from this file.'
            ELSE
c               WRITE(*,*)'mpcin:',mr,' obs from  ',file(1:lfile)//'.rad'
c               WRITE(iun20,*)'mpcin:',mr,' obs  ',file(1:lfile)//'.rad'
               mt=mr+mt
            ENDIF
         ENDIF
c add information from .obs and .rad
         IF(mpc.or.rad)THEN
            obs0=.true.
c find weights for these; a priori RMS of astrometric observations
            CALL obsrms(iobst,idstat,taut,acct,acca,accd,smagt,
     +           rmsat,rmsdt,rmsmagt,mt)
            CALL addobs(     
     +           objid,iobs,tau,tut,idsta,
     +           aln,rmsa,den,rmsd,smag,rmsmag,sel,m,
     +           objidt,iobst,taut,tutt,idstat,
     +           alnt,rmsat,dent,rmsdt,smagt,rmsmagt,mt,
     +           nlef,mnew,change)
            IF(change)THEN
               WRITE(*,*)'There are new/changed obs. New numobs=',mnew
c output updated .rwo file, if there are new observations (erasing residuals)
               CALL wrirms(file(1:lfile)//'.rwo',objid,iobs,tut,idsta,
     +              aln,rmsa,den,rmsd,smag,rmsmag,sel,mnew)
               m=mnew
            ELSE
               WRITE(*,*)'There are no updates in .obs or .rad files.'
            ENDIF
         else
c check for new weights anyway???
         ENDIF
      ENDIF
c get asteroid radius (if necessary) before returning 
      call astrad(objid,iobs,m)
c =======  sort data before returning =========
      call sortob(tut,iperm,m)
c copy output into temp vectors
      do i=1,m
         objidt(i)=objid(i)
         iobst(i)=iobs(i)
         taut(i)=tau(i)
         alnt(i)=aln(i)
         dent(i)=den(i)
         tutt(i)=tut(i)
         idstat(i)=idsta(i)
         selt(i)=sel(i)
         rmsat(i)=rmsa(i)
         rmsdt(i)=rmsd(i)
         rmsmagt(i)=rmsmag(i)
         smagt(i)=smag(i)
      enddo
c copy back input output vectors in sorted order
      do i=1,m
         objid(i)=objidt(iperm(i))
         iobs(i)=iobst(iperm(i))
         tau(i)=taut(iperm(i))
         aln(i)=alnt(iperm(i))
         den(i)=dent(iperm(i))
         tut(i)=tutt(iperm(i))
         idsta(i)=idstat(iperm(i))
         sel(i)=selt(iperm(i))
         rmsa(i)=rmsat(iperm(i))
         rmsd(i)=rmsdt(iperm(i))
         rmsmag(i)=rmsmagt(iperm(i))
         smag(i)=smagt(iperm(i))
      enddo

      RETURN
      END
