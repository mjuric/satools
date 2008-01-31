* Copyright (C) 1997-1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 26, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         A S T R O W                           *
*  *                                                               *
*  *     Computation of a-priori RMS of optical observations       *
*  *         from results of a statistical analysis or             *
*  *    from their accuracy (= number of digits in input file)     *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    MPCTYP    -  Observation type (column 15 of MPC record)
*           TDT       -  Time of observation (MJD, TDT)
*           IDSTA     -  Observatory code
*           ACCA      -  Accuracy of right ascension (rad)
*           ACCD      -  Accuracy of declination (rad)
*
* OUTPUT:   RMSA      -  A-priori RMS of right ascension (rad)
*           RMSD      -  A-priori RMS of declination (rad)
*
      SUBROUTINE astrow(mpctyp,tdt,idsta,acca,accd,rmsa,rmsd)
      IMPLICIT NONE

      INTEGER idsta
      DOUBLE PRECISION tdt,acca,accd,rmsa,rmsd
      CHARACTER*(*) mpctyp

      INCLUDE 'trig.h'

      INTEGER le
      DOUBLE PRECISION rmsmin
      CHARACTER*5 ads(2)
      CHARACTER*80 filea,filed,ermnam
      LOGICAL ermuse,fail,fail1,found,error

      SAVE ermuse

      LOGICAL first
      DATA first/.true./
      SAVE first

* Additional info on how RMS are obtained
      INCLUDE 'astrow.h'

      INTEGER lench
      EXTERNAL lench

* Input of RMS class definitions
      IF(first) THEN
          fail=.false.
          ermuse=.false.
          CALL rdnlog('errmod.','use',ermuse,.false.,found,
     +                fail1,fail)
          IF(ermuse) THEN
              ermnam='num'
              CALL rdncha('errmod.','name',ermnam,.false.,found,
     +                    fail1,fail)
              IF(fail) STOP '**** astrow: abnormal END ****'
              le=lench(ermnam)
              filea=ermnam(1:le)//'.cla'
              filed=ermnam(1:le)//'.cld'
              CALL rrmscl(filea,filed,.false.)
          END IF
          IF(fail) STOP '**** astrow: abnormal END ****'
          first=.false.
      END IF

* Negative values means: not yet assigned
      rmsa=-1.D0
      rmsd=-1.D0
      orstep(1)=0
      orstep(2)=0

* STEPs 1/2: look for a specific RMS class for the observatory
      IF(ermuse) THEN
          CALL accstr(acca,accd,ads(1),ads(2),error)
          IF(error) GOTO 2
          CALL crmscl(idsta,ads,mpctyp,tdt,rmsa,rmsd,idcl,orstep,tdtlim)
          IF(orstep(1).GT.0) rmsa=rmsa*radsec
          IF(orstep(2).GT.0) rmsd=rmsd*radsec
      END IF

* STEP 3: default, rough rule-of-thumb error model
 2    CONTINUE
      IF(rmsa.LT.0.D0 .OR. rmsd.LT.0.D0) THEN
* Minimum value of RMS (time dependent)
* Before 1890
          IF(tdt.LT.11368.d0) THEN
              rmsmin=3.d0
* From 1890 to 1950
          ELSE IF(tdt.LT.33282.d0) THEN
              rmsmin=2.d0
* After 1950
          ELSE
              rmsmin=1.d0
          END IF
          rmsmin=rmsmin*radsec
          IF(rmsa.LT.0.D0) THEN
              rmsa=MAX(rmsmin,acca)
              orstep(1)=3
          END IF
          IF(rmsd.LT.0.D0) THEN
              rmsd=MAX(rmsmin,accd)
              orstep(2)=3
          END IF
      END IF

      END
