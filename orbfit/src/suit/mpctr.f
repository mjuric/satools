* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 12, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          M P C T R                            *
*  *                                                               *
*  *   Transformation of an astrometric observation (MPC format)   *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    REC       -  MPC record
*
* OUTPUT:   OBJID     -  IAU Object designation
*           TDT       -  Time (MJD, TDT)
*           TUTM      -  Time (MJD, UTM)
*           ALPHA     -  Right ascension (rad)
*           DELTA     -  Declination (rad)
*           OBSCOD    -  Observatory code
*           ACCT      -  Accuracy of time (d)
*           ACCA      -  Accuracy of right ascension (rad)
*           ACCD      -  Accuracy of declination (rad)
*           SMAG      -  Apparent magnitude and color (string)
*           ERROR     -  Conversion error
*
      SUBROUTINE mpctr(rec,objid,tdt,tutm,alpha,delta,obscod,acct,acca,
     +                 accd,smag,error)
      IMPLICIT NONE
      CHARACTER*80 rec
      CHARACTER*(*) objid
      CHARACTER*6 smag
      DOUBLE PRECISION tdt,tutm,alpha,delta,acct,acca,accd
      INTEGER obscod
      LOGICAL error

      INCLUDE 'trig.h'

      INTEGER year,month,day,ll,pp,ndd,mjd,mjdt,errcod
      DOUBLE PRECISION sec,sect
      CHARACTER*12 mpcname
      CHARACTER*9 chdate
      CHARACTER*3 scale
      LOGICAL err1,err2,err3


      INTEGER lench
      DOUBLE PRECISION tjm1
      EXTERNAL lench,tjm1

      error=.true.
      errcod=1
c Object Name
      mpcname=rec(1:12)
      call iaucod(mpcname,objid,err3)
      if (err3) errcod=17
c Date
      READ(rec,100,ERR=10) year,month,chdate
 100  FORMAT(15X,I4,1X,I2,1X,A9)
      ll=lench(chdate)
      IF(ll.LE.0) GOTO 10
* Position of the decimal point
      pp=INDEX(chdate,'.')
      IF(pp.EQ.0) THEN
          errcod=2
          READ(chdate,*,ERR=10) day
          sec=0
          acct=1
      ELSE
          errcod=3
          READ(chdate(1:pp-1),*,ERR=10) day
          READ(chdate(pp:),*,ERR=10) sec
          sec=sec*86400
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

      CALL rdanga(rec(33:44),alpha,acca,err1)
      CALL rdanga(rec(45:56),delta,accd,err2)
      errcod=4
      IF(err1.OR.err2.or.err3) GOTO 10

      alpha=alpha*radh
      acca =acca *radh
      delta=delta*radeg
      accd =accd *radeg

      errcod=5
      READ(rec(78:80),*,ERR=10) obscod
      READ(rec(66:71),166,ERR=10)smag
 166  FORMAT(a6)
      error=.false.

 10   CONTINUE
      IF(error) THEN
          WRITE(0,101) errcod
          tdt=0
          alpha=0
          delta=0
          obscod=-1
          acct=1.d99
          acca=1.d99
          accd=1.d99
      END IF
 101  FORMAT(' mpctr: error code',I3)

      END
