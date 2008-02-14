* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: October 31, 1997
* version 1.8.0, Steven Chesley, Dec. 14, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          J P L I N                            *
*  *                                                               *
*  *         Input of radar  observations from JPL format          *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  File name
*           NOBSX     -  Max dimension of vectors (iobs,t,alpha,delta,
*                        obscod,acct,acca,accd)
*
* OUTPUT:   IOBS      -  Observation type: 2000 for radar
*                                          +100 if surface return
*                                          +2 for time delay or +3 for Doppler
*           TDT       -  Time (MJD, TDT)
*           TUTM      -  Time (MJD, UTM)
*           R         -  Zero order range measurement (AU/day)
*           V         -  Zero order range rate measurement
*           OBSCOD    -  Observatory code: trxsta*10000+recsta
*           ACCT      -  Accuracy of time (d) (always 10^-10)
*           ACCR      -  Accuracy of of range (AU)
*           ACCV      -  Accuracy of range-rate (AU/day)
*           SMAG      -  Dummy string with six blanks
*           NOBS      -  Number of observations read
c           OBS       -  successful input flag
*
      subroutine jplin(obs,file,objid,iobs,tdt,tutm,r,v,obscod,
     +                 acct,accr,accv,smag,nobs,nobsx)
      implicit none

      LOGICAL obs

      character*(*) file
      integer nobsx
      character*(*) objid(nobsx)
      integer iobs(nobsx)
      character*6 smag(nobsx)
      double precision tdt(nobsx),tutm(nobsx),r(nobsx),v(nobsx)
      double precision acct(nobsx),accr(nobsx),accv(nobsx)
      integer nobs,obscod(nobsx)


      logical error
      character*100 rec
      integer i,l,unit
      external lench
      integer lench

      call filopn(unit,file,'old')

      nobs=0
      do 1 i=1,nobsx
         read(unit,101,end=10) rec
 101     format(a)
         l=lench(rec)
         IF(l.eq.0)GOTO 1
         nobs=nobs+1
         if(nobs.gt.nobsx) stop ' **** mpcin: nobs > nobsx ****'
c alpha is actually r, delta is rdot
         call jplrad(rec,iobs(nobs),
     +        objid(nobs),tdt(nobs),tutm(nobs),r(nobs),
     +        v(nobs),obscod(nobs),acct(nobs),accr(nobs),
     +        accv(nobs),error)
         smag(nobs)='      '
         if(error) then
            l=lench(file)
            write(0,102) file(1:l),nobs
 102        format(' **** mpcin: input conversion error ****'/
     .           ' **** file = "',a,'", line',i5,' ****')
            nobs=0
            obs=.false.
            call filclo(unit,' ')
            RETURN
         end if
 1    enddo

c regular ending
 10   continue
      call filclo(unit,' ')
      IF(nobs.eq.0)THEN
         obs=.false.
         write(0,*) 'Warning: Found ',nobs,' obs in ',file
      ELSE
         obs=.true.
         write(0,*) 'Found ',nobs,' obs in ',file
      ENDIF
      return
      end
