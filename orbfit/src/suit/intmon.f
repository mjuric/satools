* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 26, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         I N T M O N                           *
*  *                                                               *
*  *   Transforms a 3-letter code of a month into integer value    *
*  *                      (0 = input error)                        *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    CHMON     -  3-letter month
*
      INTEGER FUNCTION intmon(chmon)
      IMPLICIT NONE

      CHARACTER*(*) chmon

      CHARACTER*3 c

      c=chmon
      CALL upcase(c)

      IF(c.EQ.'JAN' .OR. c.EQ.'GEN') THEN
          intmon=1
      ELSEIF(c.EQ.'FEB') THEN
          intmon=2
      ELSEIF(c.EQ.'MAR') THEN
          intmon=3
      ELSEIF(c.EQ.'APR') THEN
          intmon=4
      ELSEIF(c.EQ.'MAY' .OR. c.EQ.'MAG') THEN
          intmon=5
      ELSEIF(c.EQ.'JUN' .OR. c.EQ.'GIU') THEN
          intmon=6
      ELSEIF(c.EQ.'JUL' .OR. c.EQ.'LUG') THEN
          intmon=7
      ELSEIF(c.EQ.'AUG' .OR. c.EQ.'AGO') THEN
          intmon=8
      ELSEIF(c.EQ.'SEP' .OR. c.EQ.'SET') THEN
          intmon=9
      ELSEIF(c.EQ.'OCT' .OR. c.EQ.'OTT') THEN
          intmon=10
      ELSEIF(c.EQ.'NOV') THEN
          intmon=11
      ELSEIF(c.EQ.'DEC' .OR. c.EQ.'DIC') THEN
          intmon=12
      ELSE
          intmon=0
      END IF

      END
