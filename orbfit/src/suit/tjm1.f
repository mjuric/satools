* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 9, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                           T J M 1                             *
*  *                                                               *
*  *            Computation of Modified Julian Date                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    IDAY      -  Day of the month ( 1 <= IDAY <= 31 )
*           MONTH     -  Month of the year ( 1 <= MONTH <= 12 )
*           IYEAR     -  Year (e.g.: 1987)
*           H         -  Hour of the day ( 0. <= H < 24. )
*
* OUTPUT:   TJM1      -  Modified Julian Day MJD = JD -2,400,000.5
*
      DOUBLE PRECISION FUNCTION tjm1(iday,month,iyear,h)
      IMPLICIT NONE

      DOUBLE PRECISION h
      INTEGER iday,month,iyear,iy,im,ib,k1,k2,k3

      IF(month.LE.2) THEN
          iy=iyear-1
          im=month+12
      ELSE
          iy=iyear
          im=month
      END IF
      IF(iyear.GT.1582) THEN
          ib=iy/400-iy/100
      ELSE
          ib=-2
          IF(iyear.EQ.1582) THEN
              IF(month.gt.10) THEN
                  ib=iy/400-iy/100
              ELSEIF(month.EQ.10.AND.iday.GE.15) THEN
                  ib=iy/400-iy/100
              END IF
          END IF
      END IF
      k1=365.25d0*iy
      k2=30.6001d0*(im+1)
      k3=k1+k2+ib-679004+iday
      tjm1=k3+h/24.d0
      END
