* Copyright (C) 1997-2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 28, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         A S T R W B                           *
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
*           BIASA     -  Bias in right ascension (rad)
*           BIASD     -  Bias in declination (rad)
*           DSTEP     -  Decision step (RA/DEC):
*                           0 = ERROR
*                           1 = single-station class
*                           2 = multi-station class
*                           3 = default (1/2/3 rule)
*
      SUBROUTINE astrwb(mpctyp,tdt,idsta,acca,accd,rmsa,rmsd,
     +                  biasa,biasd,dstep)
      IMPLICIT NONE

      INTEGER idsta,dstep(2)
      DOUBLE PRECISION tdt,acca,accd,rmsa,rmsd,biasa,biasd
      CHARACTER*(*) mpctyp

      INCLUDE 'trig.h'

      INTEGER le
      DOUBLE PRECISION rmsmin,rmsap,rmsdp,avea,aved
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
              IF(fail) STOP '**** astrwb: abnormal END ****'
              le=lench(ermnam)
              filea=ermnam(1:le)//'.cla'
              filed=ermnam(1:le)//'.cld'
              write(99,210) ermnam(1:le)
              CALL rrmscl(filea,filed,.false.)
          ELSE
              write(99,211)
          END IF
          IF(fail) STOP '**** astrwb: abnormal END ****'
          first=.false.
      END IF
 210  FORMAT('INFO(astrwb): reading error RMS model ',
     +       'from files "',A,'.cl[ad]"')
 211  FORMAT('INFO(astrwb): NO specific error RMS model is used')

* Negative values means: not yet assigned
      rmsa=-1.D0
      rmsd=-1.D0
      biasa=0.D0
      biasd=0.D0
      dstep(1)=0
      dstep(2)=0

* STEPs 1/2: look for a specific RMS class for the observatory
      IF(ermuse) THEN
          CALL accstr(acca,accd,ads(1),ads(2),error)
          IF(error) GOTO 2
          CALL crmscn(idsta,ads,mpctyp,tdt,rmsap,rmsdp,
     +                avea,aved,rmsa,rmsd,idcl,dstep,tdtlim)
          IF(dstep(1).GT.0) THEN
              rmsa=rmsa*radsec
              biasa=avea*radsec
          END IF
          IF(dstep(2).GT.0) THEN
              rmsd=rmsd*radsec
              biasd=aved*radsec
          END IF
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
              dstep(1)=3
          END IF
          IF(rmsd.LT.0.D0) THEN
              rmsd=MAX(rmsmin,accd)
              dstep(2)=3
          END IF
      END IF
      orstep(1)=dstep(1)
      orstep(2)=dstep(2)

      END
