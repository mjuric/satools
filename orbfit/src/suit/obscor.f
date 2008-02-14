* Copyright (C) 2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 28, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         O B S C O R                           *
*  *                                                               *
*  * Correlation between two observations of the same observatory  *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    OBSCOD    -  Observatory code
*           T1        -  Time (MJD)                         \
*           ALPHA1    -  Right ascension (rad)               |
*           DEC1      -  Declination (rad)                   |  OBS 1
*           WRA1      -  A-priori weight for RA (rad**(-2))  |
*           WDEC1     -  A-priori weight for DEC (rad**(-2)) |
*           IDST1     -  Decision step (RA/DEC)             /
*           T2        -  Time (MJD)                         \
*           ALPHA2    -  Right ascension (rad)               |
*           DEC2      -  Declination (rad)                   |  OBS 2
*           WRA2      -  A-priori weight for RA (rad**(-2))  |
*           WDEC2     -  A-priori weight for DEC (rad**(-2)) |
*           IDST2     -  Decision step (RA/DEC)             /
*
* OUTPUT:   COVRA     -  Off-diagonal covariance in RA (rad**2)
*           COVDEC    -  Off-diagonal covariance in DEC (rad**2)
*
      SUBROUTINE obscor(obscod,t1,alpha1,dec1,wra1,wdec1,idst1,
     +                  t2,alpha2,dec2,wra2,wdec2,idst2,covra,covdec)
      IMPLICIT NONE

      INTEGER obscod,idst1(2),idst2(2)
      DOUBLE PRECISION t1,t2,alpha1,alpha2,dec1,dec2,wra1,wra2
      DOUBLE PRECISION wdec1,wdec2,covra,covdec

      INCLUDE 'parcor.h'
      INCLUDE 'parobc.h'
      INCLUDE 'comcor.h'

      INTEGER ic,ip1,if1,lf
      DOUBLE PRECISION dt
      CHARACTER*80 file
      LOGICAL first,fail,fail1,found,doit

      DATA first/.true./
      SAVE first,doit

      INTEGER lench
      EXTERNAL lench

* Initialization
      IF(first) THEN
          first=.false.
          fail=.false.
* Flag telling whether time correlations are to be computed at all
          doit=.false.
          CALL rdnlog('errmod.obscor.','use',doit,.false.,found,
     +                fail1,fail)
* Name of input file
          IF(doit) THEN
              file='obscor.mod'
              CALL rdncha('errmod.obscor.','file',file,.false.,found,
     +                    fail1,fail)
          END IF
          IF(fail) STOP '**** obscor: abnormal end ****'

* Input of correlation model
          IF(doit) THEN
              lf=lench(file)
              WRITE(0,210) file(1:lf)
              CALL rdcorm(file)
          ELSE
              WRITE(0,211)
          END IF
      END IF
 210  FORMAT('INFO(obscor): reading error correlation model ',
     +       'from file "',A,'"')
 211  FORMAT('INFO(obscor): NO error correlation model is used')

      covra=0
      covdec=0
      IF(.NOT.doit) RETURN

      IF(iiccor.NE.36) STOP '**** obscor: internal error (01) ****'
      IF(obscod.LT.obsc1.OR.obscod.GT.obsc2)
     +    STOP '**** obscor: internal error (02) ****'
      dt=ABS(t2-t1)

* Right ascension
      ic=1
      IF(idst1(ic).EQ.0 .OR. idst2(ic).EQ.0) GOTO 1
      IF(idst1(ic).GT.maxdst) GOTO 1
      IF(idst2(ic).GT.maxdst) GOTO 1
      IF(wra1.LT.minapw) GOTO 1
      IF(wra2.LT.minapw) GOTO 1
      if1=pto2f(obscod,ic,1)
      IF(if1.LE.0) GOTO 1
      ip1=ptf2p(1,if1)
      CALL fcorob(kfun(if1),nfo(obscod,ic),par(ip1),nparf(if1),dt,
     +            covra)
      covra=covra/SQRT(wra1*wra2)
 1    CONTINUE

* Declination
      ic=2
      IF(idst1(ic).EQ.0 .OR. idst2(ic).EQ.0) GOTO 2
      IF(idst1(ic).GT.maxdst) GOTO 2
      IF(idst2(ic).GT.maxdst) GOTO 2
      IF(wdec1.LT.minapw) GOTO 2
      IF(wdec2.LT.minapw) GOTO 2
      if1=pto2f(obscod,ic,1)
      IF(if1.LE.0) GOTO 2
      ip1=ptf2p(1,if1)
      CALL fcorob(kfun(if1),nfo(obscod,ic),par(ip1),nparf(if1),dt,
     +            covdec)
      covdec=covdec/SQRT(wdec1*wdec2)
 2    CONTINUE

      END
