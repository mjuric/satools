* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 4, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C H M O 2 I                           *
*  *                                                               *
*  *    Transforms 3-character month names into integer (1-12)     *
*  *                   (0 = not a month code)                      *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    INTMON    -  Integer month (1-12)
*
      INTEGER FUNCTION chmo2i(chm)
      IMPLICIT NONE

      CHARACTER*(*) chm

      CHARACTER*3 c3(12)
      DATA c3/'JAN','FEB','MAR','APR','MAY','JUN',
     +        'JUL','AUG','SEP','OCT','NOV','DEC'/

      INTEGER i
      CHARACTER*3 chm1

      chm1=chm
      CALL upcase(chm1)

      chmo2i=0
      DO 1 i=1,12
      IF(chm1.EQ.c3(i)) THEN
          chmo2i=i
          RETURN
      END IF
 1    CONTINUE

      END
