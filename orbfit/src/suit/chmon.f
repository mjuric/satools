* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 10, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          C H M O N                            *
*  *                                                               *
*  *    Transforms integer month (1-12) into a 3-letter code       *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    INTMON    -  Integer month (1-12)
*
      CHARACTER*3 FUNCTION chmon(intmon)
      IMPLICIT NONE

      INTEGER intmon

      CHARACTER*3 c3(12)
      DATA c3/'Jan','Feb','Mar','Apr','May','Jun',
     +        'Jul','Aug','Sep','Oct','Nov','Dec'/

      IF(intmon.GE.1 .AND. intmon.LE.12) THEN
          chmon=c3(intmon)
      ELSE
          chmon='???'
      END IF

      END
