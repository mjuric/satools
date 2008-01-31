* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 27, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         M J D D A T                           *
*  *                                                               *
*  *   Transformation of Modified Julian Date to calendar date     *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJM       -  Modified Julian Day
*
* OUTPUT:   IDAY      -  Day of the month  ( 1 <= IDAY <= 31 )
*           IMONTH    -  Month of the year ( 1 <= IMONTH <= 12 )
*           IYEAR     -  Year              (e.g., 1987)
*           HOUR      -  Hour of the day   ( 0.0 <= HOUR < 24.0 )
*
      SUBROUTINE mjddat(tjm,iday,imonth,iyear,hour)
      IMPLICIT NONE

      DOUBLE PRECISION tjm,hour
      INTEGER iday,imonth,iyear

      DOUBLE PRECISION a
      INTEGER ia,ib,ic,id,ie,if

      INTEGER intlo
      EXTERNAL intlo

      a=tjm+2400001.d0
      ia=a
      hour=(a-ia)*24.d0
      IF(ia.LT.2299161) THEN
          ic=ia+1524
      ELSE
          ib=intlo((ia-1867216.25d0)/36524.25d0)
          ic=ia+ib-intlo(ib/4.d0)+1525
      END IF
      id=intlo((ic-122.1d0)/365.25d0)
      ie=intlo(365.25d0*id)
      if=intlo((ic-ie)/30.6001d0)
      iday=ic-ie-intlo(30.6001d0*if)
      imonth=if-1-12*intlo(if/14.d0)
      iyear=id-4715-intlo((7+imonth)/10.d0)

      END
