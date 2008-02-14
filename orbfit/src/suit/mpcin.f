* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: October 31, 1997
* version 1.8.0, Steven Chesley, Dec. 14, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          M P C I N                            *
*  *                                                               *
*  *   Input of astrometric+ radar  observations from  MPC format  *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  File name
*           NOBSX     -  Max dimension of vectors (iobs,t,alpha,delta,
*                        obscod,acct,acca,accd)
*
* OUTPUT:   IOBS      -  Observation type: 1=astrometry: alpha,delta 
*                                          2,3,4=radar: range,range-rate
*           TDT       -  Time (MJD, TDT)
*           TUTM      -  Time (MJD, UTM)
*           ALPHA     -  Right ascension (rad)-Range (AU) if radar
*           DELTA     -  Declination (rad)-Range-rate (AU/day) if radar
*           OBSCOD    -  Observatory code
*           ACCT      -  Accuracy of time (d)
*           ACCA      -  Accuracy of right ascension (rad) - of range (AU)
*           ACCD      -  Accuracy of declination (rad) - of range-rate (AU/day)
*           SMAG      -  Observed magnitude (string, incl. color)
*           NOBS      -  Number of observations read
c           OBS       -  successful input flag
*
      subroutine mpcin(obs,file,objid,iobs,tdt,tutm,alpha,delta,obscod,
     +                 acct,acca,accd,smag,nobs,nobsx)
      implicit none

      LOGICAL obs

      character*(*) file
      integer nobsx
      character*(*) objid(nobsx)
      integer iobs(nobsx)
      character*6 smag(nobsx)
      double precision tdt(nobsx),tutm(nobsx),alpha(nobsx),delta(nobsx)
      double precision acct(nobsx),acca(nobsx),accd(nobsx)
      integer nobs,obscod(nobsx)
c ==========================================
      logical error
      character*80 rec1,rec2
      character*1 obscha
      integer l,unit
      external lench
      integer lench
c use MPC radar obs if radmpc=.true.
      logical radmpc
      data radmpc/.false./

      call filopn(unit,file,'old')

      nobs=0
 1    continue
      read(unit,101,end=10) rec1
 101  format(a)
      l=lench(rec1)
      IF(l.eq.0)GOTO 1
      nobs=nobs+1
      if(nobs.gt.nobsx) stop ' **** mpcin: nobs > nobsx ****'
c ================= HANDLE RADAR ============================
      if(rec1(15:15).eq.'R'.or.rec1(15:15).eq.'r')then
c radmpc determines whether MPC radar observations are used or ignored.
         if(radmpc)then
            write(0,*)'Radar observation near record ',nobs
            read(unit,101,end=20) rec2
c  alpha is actually r, delta is rdot
            call mpcrad(rec1,rec2,iobs(nobs),
     +           objid(nobs),tdt(nobs),tutm(nobs),alpha(nobs),
     +           delta(nobs),obscod(nobs),acct(nobs),acca(nobs),
     +           accd(nobs),error)
            smag(nobs)='      '
c  generate range and range rate only if mixed observation
            if(mod(iobs(nobs),100).eq.1)then
               iobs(nobs)=iobs(nobs)+1
               iobs(nobs+1)=iobs(nobs)+1
               objid(nobs+1)=objid(nobs)
               tdt(nobs+1)=tdt(nobs)
               tutm(nobs+1)=tutm(nobs)
               alpha(nobs+1)=0d0
               delta(nobs+1)=delta(nobs)
               delta(nobs)=0d0
               obscod(nobs+1)=obscod(nobs)
               acct(nobs+1)=acct(nobs)
               acca(nobs+1)=-1d0
               accd(nobs+1)=accd(nobs)
               accd(nobs)=-1d0
               smag(nobs+1)='      '
               nobs=nobs+1
            endif
         else
            if(rec1(15:15).eq.'R')write(0,*) 'ignoring MPC radar obs'
            nobs=nobs-1
            goto 1
         endif
c ================= HANDLE SATELLITE ============================
      elseif(rec1(15:15).eq.'S'.or.rec1(15:15).eq.'s')then
         write(0,*)'Satellite observation at record ',nobs
         nobs=nobs-1
c         iobs(nobs)=3000+j
         goto 1
c ================= HANDLE OPTICAL ============================
      else
         call mpctr(rec1,objid(nobs),tdt(nobs),tutm(nobs),alpha(nobs),
     +        delta(nobs),obscod(nobs),acct(nobs),acca(nobs),
     +        accd(nobs),smag(nobs),error)
c assign iobs=1000+ichar(obscha), but replace blanks with P's
         obscha=rec1(15:15)
         if(obscha.eq.' ')obscha='P'
         if(.not.error)iobs(nobs)=1000+ichar(obscha)
      endif
      if(error) then
          l=lench(file)
          write(0,102) file(1:l),nobs
 102      format(' **** mpcin: input conversion error ****'/
     .       ' **** file = "',a,'", line',i5,' ****')
          nobs=0
          obs=.false.
          call filclo(unit,' ')
          RETURN
      end if
      goto 1
c regular ending
 10   continue
      call filclo(unit,' ')
      IF(nobs.eq.0)THEN
         obs=.false.
      ELSE
         obs=.true.
      ENDIF
      return
c erroneous radar ending
 20   write(0,*) 'mpcin: incomplete radar observation, end of file'
      stop
      end
