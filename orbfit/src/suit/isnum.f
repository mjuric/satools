* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 26, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          I S N U M                            *
*  *                                                               *
*  *    Tells whether a character string contains only digits      *
*  *                                                               *
*  *****************************************************************
*
* WARNING: assumes ASCII internal representation of characters
*
* INPUT:    STRING    -  Input string
*
      LOGICAL FUNCTION isnum(string)
      IMPLICIT NONE

      CHARACTER*(*) string

      INTEGER i,icc

      isnum=.false.
      DO 1 i=1,LEN(string)
      icc=ICHAR(string(i:i))
      IF(icc.LT.48 .OR. icc.GT.57) RETURN
 1    CONTINUE

      isnum=.true.

      END
