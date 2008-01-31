* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 12, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         I E R S T S                           *
*  *                                                               *
*  *          IERS time series for polar motion, TDT-UT1           *
*  *                   and nutation correction                     *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Day (ET)
*           NDER      -  Required order for derivatives (0/1/2)
*
* OUTPUT:   EOP(5)    -  Earth orientation parameters at TJME
*                            EOP(1) = X pole (arcsec)
*                            EOP(2) = Y pole (arcsec)
*                            EOP(3) = TDT-UT1 (s)
*                            EOP(4) = Dpsi (arcsec)
*                            EOP(5) = Depsilon (arcsec)
*           EOPD(5)   -  dEOP/dt (/d)
*           EOPDD(5)  -  d2EOP/dt2 (/d**2)
*
      SUBROUTINE iersts(tjme,eop,eopd,eopdd,ntder)
      IMPLICIT NONE

      INTEGER ntder
      DOUBLE PRECISION tjme
      DOUBLE PRECISION eop(5),eopd(5),eopdd(5)

      DOUBLE PRECISION eps
      PARAMETER (eps=1.D-6)

      INCLUDE 'parier.h'
      INCLUDE 'parlgi.h'

* NEEDED common blocks:
      INCLUDE 'cmiers.h'

      INTEGER iret,i,iextr
      DOUBLE PRECISION du,dud,dudd,dtbes

* Work arrays for pcwlgi
      DOUBLE PRECISION copler(0:lgintx,niplix,5),tlpler(2,niplix)
      LOGICAL vdpler(niplix)

      LOGICAL start
      INCLUDE 'bessyr.h'
      DOUBLE PRECISION bessep
      EXTERNAL bessep

      SAVE start,copler,tlpler,vdpler
      DATA start/.true./



      IF(iicier.NE.36) THEN
          CALL ierini
          IF(iicier.NE.36) STOP' **** iersts: internal error (01) ****'
      END IF

* Extrapolation
      iextr=0
      IF(extra) THEN
          IF(tjme.LT.tint1) THEN
              iextr=-1
          ELSEIF(tjme.GT.tint2) THEN
              iextr=1
          END IF
      END IF

      IF(iextr.EQ.0) THEN
          CALL pcwlgi(xiers,tiers,niers,5,niersx,
     +                tjme,eop,eopd,eopdd,
     +                nlpler,nvpler,nspler,nsmopl,ntder,start,rmserx,
     +                copler,tlpler,vdpler,lgintx,niplix,iret)
          IF(iret.NE.0) THEN
              IF(extra) THEN
                  IF(ABS(tjme-tint1).LE.eps) THEN
                      iextr=-1
                  ELSEIF(ABS(tjme-tint2).LE.eps) THEN
                      iextr=1
                  END IF
                  IF(iextr.EQ.0)
     +                STOP' **** iersts: internal error (02) ****'
              ELSE
                  WRITE(*,100) tjme
                  STOP '**** iersts: abnormal END ****'
              END IF
          END IF
      END IF
 100  FORMAT(' iersts: time is out of bounds: TJME =',f12.5)

      IF(iextr.NE.0) THEN
          DO 2 i=1,5
          eop(i)=0.d0
          eopd(i)=0.d0
          eopdd(i)=0.d0
 2        CONTINUE
          eopd(3)=dutd
          IF(iextr.EQ.-1) THEN
              eop(3)=utd1+dutd*(tjme-tint1)
          ELSEIF(iextr.EQ.1) THEN
              eop(3)=utd2+dutd*(tjme-tint2)
          ELSE
              STOP '**** iersts: internal error (03) ****'
          END IF
      END IF

* Subtracting applied smoothing
      IF(iutsmo.EQ.0) THEN
          CONTINUE
      ELSEIF(iutsmo.EQ.1) THEN
          CALL dut1r(tjme,du,dud,dudd,ntder)
* (TDT-UT1) = (TDT-UT1R) - (UT1-UT1R)
          eop(3)=eop(3)-du
          IF(ntder.GE.1) eopd(3)=eopd(3)-dud
          IF(ntder.GE.2) eopdd(3)=eopdd(3)-dudd
      ELSEIF(iutsmo.EQ.2) THEN
          CALL dut1s(tjme,du,dud,dudd,ntder)
* (TDT-UT1) = (TDT-UT1S) - (UT1-UT1S)
          eop(3)=eop(3)-du
          IF(ntder.GE.1) eopd(3)=eopd(3)-dud
          IF(ntder.GE.2) eopdd(3)=eopdd(3)-dudd
      ELSE
          STOP '**** iersts: internal error (04) ****'
      END IF

* Adding consistency correction
      IF(cciera) THEN
          dtbes=bessep(tjme)-cnep0
          DO 1 i=1,5
          eop(i)=eop(i)+cncor0(i)+cncor1(i)*dtbes
          IF(ntder.GE.1) eopd(i)=eopd(i)+cncor1(i)/bessyr
 1        CONTINUE
      END IF

      END
