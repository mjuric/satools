* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          D U T 1 S                            *
*  *                                                               *
*  *             Computation of DUT1S = UT1 - UT1S                 *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Day (TDT)
*           NTDER     -  Required order of time derivatives
*
* OUTPUT:   DU        -  DUT1S (s)
*           DUD       -  dUT1S/dt (s/d)
*           DUDD      -  d2UT1S/d2 (s/d**2)
*
* UT1S is a regularized form of UT1, which does not include terms
* due to zonal tides with periods up to 35 days
*
      SUBROUTINE dut1s(tjme,du,dud,dudd,ntder)
      IMPLICIT NONE

      INTEGER ntder
      DOUBLE PRECISION tjme,du,dud,dudd

      INTEGER nterms
      PARAMETER (nterms=62)

      INTEGER unit,i,k
      INTEGER kmul(5,nterms)
      DOUBLE PRECISION cs,cc,p,arg(5),argd(5),argdd(5)
      DOUBLE PRECISION a,ad,add,cosa,sina
      DOUBLE PRECISION coefc(nterms),coefs(nterms)
      CHARACTER*10 rec
      LOGICAL first
      SAVE first,kmul,coefc,coefs
      DATA first/.true./



      IF(ntder.LT.0 .OR. ntder.GT.2) STOP '**** dut1s: ntder = ? ****'

      IF(first) THEN
          first=.false.
          CALL filopl(unit,'dut1s.coe')
 1	  CONTINUE
          READ(unit,100) rec
          IF(rec.NE.'----------') GOTO 1
          DO 2 k=1,nterms
          READ(unit,101)(kmul(i,k),i=1,5),cs,cc
          coefs(k)=cs*1.d-4
          coefc(k)=cc*1.d-4
 2        CONTINUE
          READ(unit,*,END=3) p
          STOP '**** dut1s: internal error (01) ****'
 3        CONTINUE
          CLOSE(unit)
      END IF
 100  FORMAT(A)
 101  FORMAT(I3,4I4,11X,F9.2,F7.2)

      CALL nutarg(tjme,arg,argd,argdd,ntder)

      du=0.d0
      dud=0.d0
      dudd=0.d0

      DO 7 k=1,nterms
      a=0.d0
      DO 4 i=1,5
      a=a+kmul(i,k)*arg(i)
 4    CONTINUE
      cosa=COS(a)
      sina=SIN(a)
      du=du+coefc(k)*cosa+coefs(k)*sina

      IF(ntder.GE.1) THEN
          ad=0.d0
          DO 5 i=1,5
          ad=ad+kmul(i,k)*argd(i)
 5        CONTINUE
          dud=dud+ad*(-coefc(k)*sina+coefs(k)*cosa)
      END IF
      IF(ntder.GE.2) THEN
          add=0.d0
          DO 6 i=1,5
          add=add+kmul(i,k)*argdd(i)
 6        CONTINUE
          dudd=dudd-coefc(k)*(sina*add+cosa*(ad**2))
     +             +coefs(k)*(cosa*add-sina*(ad**2))
      END IF
 7    CONTINUE

      END
