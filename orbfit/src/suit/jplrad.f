* Copyright (C) 1999 ORBFIT Consortium
* Version: Sept. 28, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          J P L R A D                          *
*  *                                                               *
*  *   Transformation of a radar observation (JPL format)          *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    REC       - Record from JPL (HTML) radar file
*
* OUTPUT:   IOBS      -  Obs. type: 2002=r 2003 rdot
*                        if surface bounce add 100
*           OBJID     -  IAU Object designation
*           TDT       -  Time (MJD, TDT)
*           TUTM      -  Time (MJD, UTM)
*           R         -  Range (AU) order 0
*           V         -  Range rate (AU/day) order 0
*           OBSCOD    -  Observatory code
*           ACCT      -  Accuracy of time (d)
*           ACCR      -  Accuracy of range
*           ACCV      -  Accuracy of range rate
*           ERROR     -  Conversion error
*
      SUBROUTINE jplrad(rec,iobs,objid,tdt,tutm,r,v,obscod,
     +          acct,accr,accv,error)
      IMPLICIT NONE
      CHARACTER*(*) rec
      CHARACTER*(*) objid
      DOUBLE PRECISION tdt,tutm,r,v,acct,accr,accv
      INTEGER obscod,iobs
      LOGICAL error

      INCLUDE 'trig.h'
      INCLUDE 'vlight.h'
      INTEGER errcod
c name
      character*6 number
      character*17 nametmp
      integer lnum,lnam
c time
      integer year,month,day,hour,min,isec,mjd,mjdt
      double precision sec,sect
      character*3 scale
c measurements
      double precision obs,rms,freq
      character*2 unit 
      character*3 surf
      logical range
c obscode
      character*9 trxstr,recstr
      integer iotr,iore
c functions
      INTEGER station,ix
      DOUBLE PRECISION tjm1
      EXTERNAL tjm1

      error=.true.
      errcod=1
c ========== HANDLE OBJECT NAME =================
      READ(rec,101,ERR=10) number,nametmp
 101  FORMAT(a5,1x,a17)
      call rmsp(number,lnum)
      call rmsp(nametmp,lnam)
      if(lnam.gt.9)lnam=9
      if(lnum.eq.0)then
c remove identified object if present ('1991AQ=1994RD')
         ix=index(nametmp,'=')
         if(ix.gt.0)lnam=ix-1
         objid=nametmp(1:lnam)
      else
         objid=number(1:lnum)
      endif
c ========== HANDLE DATE AND TIME =================
      READ(rec,102,ERR=10) year,month,day,hour,min,isec
 102  FORMAT(24X,I4,5(1X,I2))
      IF(year.LT.1972) THEN
          scale='UT1'
      ELSE
          scale='UTC'
      END IF
      sec=(hour*60d0+min)*60d0+isec
      mjd=nint(tjm1(day,month,year,0.d0))
      CALL cnvtim(mjd,sec,scale,mjdt,sect,'TDT')
      tutm=mjd+sec/86400.d0
      tdt=mjdt+sect/86400.d0
      acct=10d-10
c ========== READ MEASUREMENT =================
      READ(rec,103,ERR=10) obs,rms,unit,surf,freq
 103  FORMAT(44x,f13.2,1x,f7.3,1x,a2,1x,a3,1x,f5.0)
      freq=freq*1d6
      if(unit.eq.'us')then
         iobs=2002
         range=.true.
      elseif(unit.eq.'Hz')then
         iobs=2003
         range=.false.
      else
         errcod=37
         goto 10         
      endif
      if(surf.eq.'COM')then
         continue
      elseif(surf.eq.'PP ')then
         iobs=iobs+100
      else
         errcod=47
         goto 10         
      endif
      if(range)then
         r=obs*1d-6/86400d0*vlight/2.d0
         accr=rms*1d-6/86400d0*vlight/2.d0
         v=0d0
         accv=-1d0
      else
         v=-obs*vlight/(freq*2.d0)
         accv=rms*vlight/(freq*2.d0)
         r=0d0
         accr=-1d0
      endif
c This is the old code. Why 
c order zero range
c      r=dt*vlight/2.d0      
c order zero range rate
c      v=-vlight*df/(hz*2.d0)
c scaling to zeroth order
c      accr = accr*vlight/2.d0
c      accv = accv*vlight/hz
c deal with surface vs. mass center return:
c ======================= HANDLE OBSERVATORY CODES ====================
      READ(rec,104,ERR=10) trxstr,recstr
 104  FORMAT(79x,a9,1x,a9)
      errcod=5
      iotr=station(trxstr)
      iore=station(recstr)
      obscod=iotr*10000+iore
c ====================== CLEAN UP =====================================
      error=.false.
 10   CONTINUE
      IF(error) THEN
         WRITE(0,105) errcod,rec
 105     FORMAT(' jplrad: error code',I3,'rec:',A)
         tdt=0
         r=-1.
         v=1.d99
         obscod=-1
         acct=1.d99
         accr=1.d99
         accv=1.d99
      ENDIF
      END
c ====================== station function =============================      
c compute observatory code from string
      integer function station(stastr)
      implicit none
      character*(*) stastr
c We have the following stations:
c     'Arecibo  ' = 251
c     'DSS 13   ' = 252
c     'DSS 14   ' = 253
c     'Haystack ' = 254
c     'Evpatoria' = 255 (not in MPC file...)
      if(stastr.eq.'Arecibo  ')then
         station=251
      elseif(stastr.eq.'DSS 13   ')then
         station=252
      elseif(stastr.eq.'DSS 14   ')then
         station=253
      elseif(stastr.eq.'Haystack ')then
         station=254
      elseif(stastr.eq.'Evpatoria')then
         station=255
      else
         station=500
         write(0,*)'jplrad: unknown station: ',stastr
      endif
      return
      end
