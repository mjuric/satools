* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 15, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         I S B N U M                           *
*  *                                                               *
*  *    Tells whether a character string contains only digits      *
*  *                 and/or blank characters                       *
*  *                                                               *
*  *****************************************************************
*
* WARNING: assumes ASCII internal representation of characters
*
* INPUT:    STRING    -  Input string
*
      LOGICAL FUNCTION isbnum(string)
      IMPLICIT NONE

      CHARACTER*(*) string

      INTEGER i,icc

      isbnum=.false.
      DO 1 i=1,LEN(string)
      IF(string(i:i).EQ.' ') GOTO 1
      icc=ICHAR(string(i:i))
      IF(icc.LT.48 .OR. icc.GT.57) RETURN
 1    CONTINUE

      isbnum=.true.

      END
