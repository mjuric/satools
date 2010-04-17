* Copyright (C) 1997-1999 by Mario Carpino (carpino@brera.mi.astro.it)
*                            Andrea Milani (milani@dm.unipi.it)
*
* Version: February 12, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         O E F D E T                           *
*  *                                                               *
*  *         Auto-detects format of orbital element files          *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Input FORTRAN unit (must be already opened)
*           FILNAM    -  Input file name (for error messages)
*
* OUTPUT:   FORM      -  Format:
*                          1) OEF:   ORBFIT orbital element file
*                          2) BA1:   Bowell's astrob.dat (pre-1999)
*                          3) BA2:   Bowell's astrob.dat (post-1999)
*                          4) MPC-A: MPC (asteroids)
*                          5) BAC:   Bowell private format with C
* in the future:
*                          6) MPC-C: MPC (comets)
*
      SUBROUTINE oefdet(unit,filnam,form)
      IMPLICIT NONE

      INTEGER unit
      CHARACTER*(*) filnam,form

      INCLUDE 'parcmc.h'

* Number of supported formats
      INTEGER nfx
      PARAMETER (nfx=5)

      INTEGER lf,lr,i,k,npf,ipf,lrwb
      LOGICAL poss(nfx),neoh,error
      CHARACTER rec*300,recwb*200,b4*4,p4*4
      CHARACTER*50 tmp,tmp1,tmp2

      INTEGER lench
      EXTERNAL lench

* Names of supported formats
      CHARACTER*10 fname(nfx)
      DATA fname/'OEF','BA1','BA2','MPC-A','BAC'/

      form=' '

* METHOD: at the beginning, we flag as possible all the supported
* formats. Then we scan the first records of the file and discard
* formats as we find lines which are not compatible with them.
* At the end, detection is successful if one and only one format
* remains.
      DO 1 i=1,nfx
      poss(i)=.true.
 1    CONTINUE

      neoh=.true.
* Scan only first 100 records of the file
      DO 2 k=1,100
      READ(unit,100,END=3) rec
 100  FORMAT(A)
      lr=lench(rec)

* Format 1 (OEF)
      IF(poss(1).AND.neoh) THEN
          recwb=rec
          CALL rmsp(recwb,lrwb)
          IF(lrwb.GT.0) THEN
              IF(recwb.EQ.'END_OF_HEADER') THEN
                  neoh=.false.
              ELSEIF(recwb(1:1).NE.comcha) THEN
                  IF(index(recwb,'=').EQ.0) THEN
                      poss(1)=.false.
                  ELSEIF(recwb(1:7).EQ.'format=') THEN
                      tmp=recwb(8:)
                      i=index(tmp,comcha)
                      IF(i.GT.0) THEN
                          tmp1=tmp(1:i-1)
                          tmp=tmp1
                      END IF
                      CALL strcnt(tmp,tmp1,tmp2,error)
                      IF(error) THEN
                          poss(1)=.false.
                      ELSEIF(tmp1.NE.'OEF1.1') THEN
                          poss(1)=.false.
                      END IF
                  END IF
              END IF
          END IF
      END IF

* Format 2 (BA1)
      IF(poss(2)) THEN
          IF(lr.NE.265) poss(2)=.false.
          b4(1:1)=rec(5:5)
          b4(2:2)=rec(24:24)
          b4(3:3)=rec(40:40)
          b4(4:4)=rec(46:46)
          IF(b4.NE.'    ') poss(2)=.false.
          p4(1:1)=rec(117:117)
          p4(2:2)=rec(128:128)
          p4(3:3)=rec(139:139)
          p4(4:4)=rec(149:149)
          IF(p4.NE.'....') poss(2)=.false.
      END IF

* Format 3 (BA2)
      IF(poss(3)) THEN
          IF(lr.NE.266) poss(3)=.false.
          b4(1:1)=rec(6:6)
          b4(2:2)=rec(25:25)
          b4(3:3)=rec(41:41)
          b4(4:4)=rec(47:47)
          IF(b4.NE.'    ') poss(3)=.false.
          p4(1:1)=rec(118:118)
          p4(2:2)=rec(129:129)
          p4(3:3)=rec(140:140)
          p4(4:4)=rec(150:150)
          IF(p4.NE.'....') poss(3)=.false.
      END IF

* Format 4 (MPC-A)
      IF(poss(4)) THEN
          b4(1:1)=rec(8:8)
          b4(2:2)=rec(26:26)
          b4(3:3)=rec(58:58)
          b4(4:4)=rec(80:80)
          IF(b4.NE.'    ') poss(4)=.false.
          p4(1:1)=rec(30:30)
          p4(2:2)=rec(41:41)
          p4(3:3)=rec(52:52)
          p4(4:4)=rec(63:63)
          IF(p4.NE.'....') poss(4)=.false.
      END IF

c Format 5 (BAC)
      IF(poss(5)) THEN
          IF(mod(k,9).eq.2) THEN
              IF(rec(1:1).eq.'B')THEN
                  poss(5)=.true.
              ELSEIF(rec(1:1).eq.'M')THEN
                  poss(5)=.true.
              ELSE
                  poss(5)=.false.
              END IF
          ELSE
              poss(5)=.false.
          END IF
      END IF

 2    CONTINUE
 3    CONTINUE
      IF(neoh) poss(1)=.false.

* Final check
      ipf=0
      npf=0
      DO 10 i=1,nfx
      IF(poss(i)) THEN
          npf=npf+1
          ipf=i
      END IF
 10   CONTINUE

      IF(npf.EQ.1) form=fname(ipf)

      IF(form.EQ.' ') THEN
          lf=lench(filnam)
          write(99,200) filnam(1:lf)
      END IF
 200  FORMAT('ERROR: format auto-detection failed for file "',A,'":'/
     +       '       please specify format explicitly')

      END
