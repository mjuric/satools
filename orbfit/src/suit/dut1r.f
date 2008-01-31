* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          D U T 1 R                            *
*  *                                                               *
*  *             Computation of DUT1R = UT1 - UT1R                 *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Day (TDT)
*           NTDER     -  Required order of time derivatives
*
* OUTPUT:   DU        -  DUT1R (s)
*           DUD       -  dUT1R/dt (s/d)
*           DUDD      -  d2UT1R/d2 (s/d**2)
*
* UT1R is a regularized form of UT1, which does not include terms
* due to zonal tides with periods up to 35 days
*
      SUBROUTINE dut1r(tjme,du,dud,dudd,ntder)
      IMPLICIT NONE

      INTEGER ntder
      DOUBLE PRECISION tjme,du,dud,dudd

      INTEGER nterms
      PARAMETER (nterms=41)

      INTEGER unit,i,k
      INTEGER kmul(5,nterms)
      DOUBLE PRECISION p,cc,a,ad,add,coef(nterms)
      DOUBLE PRECISION arg(5),argd(5),argdd(5),sina,cosa
      CHARACTER*10 rec
      LOGICAL first
      SAVE first,kmul,coef
      DATA first/.true./



      IF(ntder.LT.0 .OR. ntder.GT.2) STOP '**** dut1r: ntder = ? ****'

      IF(first) THEN
          first=.false.
          CALL filopl(unit,'dut1r.coe')
 1        CONTINUE
          READ(unit,100) rec
          IF(rec.NE.'----------') GOTO 1

          DO 2 k=1,nterms
          READ(unit,*)(kmul(i,k),i=1,5),p,cc
          coef(k)=cc*1.D-4
 2        CONTINUE

          READ(unit,*,END=3) p
          STOP' **** dut1r: error (01) ****'
 3        CONTINUE
          CLOSE(unit)

      END IF
 100  FORMAT(a)

      CALL nutarg(tjme,arg,argd,argdd,ntder)

      du=0.d0
      dud=0.d0
      dudd=0.d0

      DO 7 k=1,nterms
      a=0.d0
      DO 4 i=1,5
      a=a+kmul(i,k)*arg(i)
 4    CONTINUE
      sina=SIN(a)
      du=du+coef(k)*sina
      IF(ntder.GE.1) THEN
          ad=0.d0
          DO 5 i=1,5
          ad=ad+kmul(i,k)*argd(i)
 5        CONTINUE
          cosa=COS(a)
          dud=dud+coef(k)*cosa*ad
      END IF
      IF(ntder.GE.2) THEN
          add=0.d0
          DO 6 i=1,5
          add=add+kmul(i,k)*argdd(i)
 6        CONTINUE
          dudd=dudd+coef(k)*(COS(a)*add-sina*(ad**2))
      END IF
 7    CONTINUE

      END
