* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 12, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          M P C R A D                          *
*  *                                                               *
*  *   Transformation of a radar observation (MPC format)          *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    REC1,REC2 -  2 MPC records with R,r in column 15
*
* OUTPUT:   IOBS      -  Obs. type: 2001=r,rdot 2002=r 2003 rdot
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
      SUBROUTINE mpcrad(rec1,rec2,iobs,objid,tdt,tutm,r,v,obscod,
     +          acct,accr,accv,error)
      IMPLICIT NONE
      CHARACTER*80 rec1,rec2
      CHARACTER*(*) objid
      DOUBLE PRECISION tdt,tutm,r,v,acct,accr,accv
      INTEGER obscod,iobs
      LOGICAL error

      INCLUDE 'trig.h'
      INCLUDE 'vlight.h'

      CHARACTER*80 recsw
      INTEGER year,month,day,year2,month2,obscod2
      INTEGER ll,pp,ndd,mjd,mjdt,errcod
      DOUBLE PRECISION sec,sect
      CHARACTER*12 mpcname1,mpcname2
      CHARACTER*9 chdate,chdate2
      CHARACTER*3 scale

      DOUBLE PRECISION hz,dt,df
      LOGICAL rng,vel

      INTEGER lench
      DOUBLE PRECISION tjm1
      EXTERNAL lench,tjm1

      INTEGER isec,iisec
c suface bounce
      LOGICAl surf

      error=.true.
      errcod=1
c sort R and r record
      IF(rec1(15:15).eq.'R')THEN
         IF(rec2(15:15).eq.'r')THEN
c all right, proceed
         ELSE
c not a good pair of records
            errcod=50
            GOTO 10
         ENDIF
      ELSEIF(rec1(15:15).eq.'r')THEN
         IF(rec2(15:15).eq.'R')THEN
c swap
            recsw=rec1
            rec1=rec2
            rec2=recsw
         ELSE
c not a good pair of records
            errcod=50
            GOTO 10
         ENDIF
      ELSE
c not a good pair of records, maybe no radar at all
         errcod=50
         GOTO 10
      ENDIF
c Object Name
      mpcname1=rec1(1:12)
      mpcname2=rec2(1:12)
      if(mpcname1.ne.mpcname2)then
         write(*,*)'name error radar obs. ',mpcname1,' ',mpcname2
         errcod=44
         goto 10
      endif
      call iaucod(mpcname1,objid,error)
      if (error) then
         errcod=17
         goto 10
      endif
c ========== HANDLE DATE AND TIME =================
      READ(rec1,100,ERR=10) year,month,chdate
 100  FORMAT(15X,I4,1X,I2,1X,A9)
      READ(rec2,100,ERR=10) year2,month2,chdate2
      if(year.ne.year2.or.month.ne.month2.or.chdate.ne.chdate2)then
         write(*,*)'date error radar obs. ',year,' ',month,' ',chdate
         write(*,*)'                      ',year2,' ',month2,' ',chdate2
         errcod=45
         goto 10
      endif
      ll=lench(chdate)
      IF(ll.LE.0) GOTO 10
* Position of the decimal point
      pp=INDEX(chdate,'.')
      IF(pp.EQ.0) THEN
          WRITE(*,*)'mpcrad: radar data without correct time'
          STOP
c          errcod=2
c         READ(chdate,*,ERR=10) day
c         sec=0
c         acct=1
      ELSE
          errcod=3
          READ(chdate(1:pp-1),*,ERR=10) day
          READ(chdate(pp:),*,ERR=10) sec
c convert to seconds and round, radar observations are normal points 
c made at integer seconds.
          sec=sec*86400.d0
          isec=nint(sec)
c but, if we get either 59 sec or 1 sec, we rather believe that
c MPC made a mess by rounding time to 10^-5 days 
          iisec=mod(isec,60)
          IF(iisec.eq.1)THEN
            sec=isec-1
          ELSEIF(iisec.eq.59)THEN      
            sec=isec+1
          ELSE
            sec=isec
          ENDIF
c          WRITE(*,*)sec,isec,iisec
          ndd=ll-pp
          acct=10.0d0**(-ndd)
      END IF
      IF(year.LT.1972) THEN
          scale='UT1'
      ELSE
          scale='UTC'
      END IF
      mjd=nint(tjm1(day,month,year,0.d0))
      CALL cnvtim(mjd,sec,scale,mjdt,sect,'TDT')
      tutm=mjd+sec/86400.d0
      tdt=mjdt+sect/86400.d0
c ========== READ TIME DELAY (CONVERT TO ZERO ORDER RANGE) =================
c time delay (in seconds, then convert to days) 
      read(rec1(33:47),FMT='(BZ,F15.10)')dt
      read(rec2(34:47),FMT='(BZ,F14.10)')accr
      dt=dt/86400.d0
      accr=accr/86400.d0
c ========== READ DOPPLER SHIFT =================
c doppler shift (megahertz)
c set obs. type: 2=r,rdot 3=r only 4=rdot only
      read(rec1(48:62),FMT='(BZ,F15.4)')df
      read(rec2(48:62),FMT='(BZ,F15.4)')accv
c ========== READ TRANSMITTER FREQUENCY IN HZ=================
      read(rec1(63:68),FMT='(BZ,F6.1)')hz
      hz=hz*1.d6
      IF(lench(rec2(63:68)).ne.0) THEN
c        continuation of frequency is not handled for now.
         errcod=37
         goto 10
      endif
c ========== CONVERT TO ZERO ORDER RANGE & RANGE-RATE =================
c order zero range
      r=dt*vlight/2.d0      
c order zero range rate
      v=-vlight*df/(hz*2.d0)
c scaling to zeroth order
      accr = accr*vlight/2.d0
      accv = accv*vlight/hz
c deal with surface vs. mass center return:
      if(rec2(33:33).eq.'S')then
c rough fix; it should be deduced from H magnitude, e.g. stored in a file
         surf=.true.
c anyway the weighting needs to be changed in this case???
      elseif(rec2(33:33).eq.'C')then
         surf=.false.         
c observation is already reduced to the asteroid center of mass
c no correction is needed
      else
         errcod=73
         goto 10
      endif
c ========== GET OBSERVATION TYPE =================
      rng=.true.
      vel=.true.
      IF(lench(rec1(33:47)).eq.0) rng=.false.
      IF(lench(rec1(48:62)).eq.0) vel=.false.
c Assign obs type: 2001=r,rdot 2002=r only 2003=rdot only
      IF(rng.and.vel)THEN
         iobs=2001
      ELSEIF(rng)THEN
         iobs=2002
         v=0.d0
         accv=-1.d0
      ELSEIF(vel)THEN
         iobs=2003
         r=0.d0
         accr=-1.d0
      ELSE
         errcod=31
         goto 10
      ENDIF
      IF(surf)iobs=iobs+100
c ============= HANDLE OBSERVATORY CODES ====================
      errcod=5
c obscodes should be identical:
      IF(rec1(69:80).ne.rec2(69:80))THEN
         errcod=46
         GOTO 10
      ENDIF
c read observatory codes
      READ(rec1(69:71),*,ERR=10) obscod
      READ(rec1(78:80),*,ERR=10) obscod2
c divide by 10000 to get the transmitter, remainder is the receiver 
      obscod=obscod*10000+obscod2
      error=.false.

 10   CONTINUE
      IF(error) THEN
         WRITE(*,101) errcod,rec1
 101     FORMAT(' mpcrad: error code',I3,'rec:',A)
         tdt=0
         r=-1.
         v=1.d99
         obscod=-1
         acct=1.d99
         accr=1.d99
         accv=1.d99
      END IF

      END
