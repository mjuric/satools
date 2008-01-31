* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                         D E L T H P                         *
*  *                                                             *
*  *        Computation of DT = ET-UT1 as a function of ET       *
*  *                   (with time derivatives)                   *
*  *                                                             *
*  ***************************************************************
*
* INPUT:    MJDE      -  Modified Julian Time (ET): integer part
*           SECE      -  Modified Julian Time (ET): seconds within
*                        the day ( 0<= sece < 86400, so that
*                        TJME = mjde + sece/86400)
*           NDER      -  (=0,1,2) required order for derivatives
*
* OUTPUT:   DT        -  DT = ET - UT1  (s)
*           DT1       -  dDT/dET        (s/s)      (only if nder >= 1)
*           DT2       -  d^2 DT/dET^2   (s/s^2)    (only if nder  = 2)
*
      SUBROUTINE delthp(mjde,sece,dt,dt1,dt2,nder)
      IMPLICIT NONE

      INTEGER mjde,nder
      DOUBLE PRECISION sece,dt,dt1,dt2

      DOUBLE PRECISION c1,c2,eop(5),eopd(5),eopdd(5),tjme
      LOGICAL first
      SAVE first,c1,c2
      DATA first/.true./



      IF(first) THEN
          first=.false.
          c1=1.d0/86400.d0
          c2=c1/86400.d0
      END IF

      tjme=mjde+sece/86400.d0
      CALL iersts(tjme,eop,eopd,eopdd,nder)
      dt=eop(3)
      dt1=eopd(3)*c1
      dt2=eopdd(3)*c2

      END
