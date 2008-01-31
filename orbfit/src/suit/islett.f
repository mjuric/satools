* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 26, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         I S L E T T                           *
*  *                                                               *
*  *    Tells whether a character string contains only letters     *
*  *                          (a-z,A-Z)                            *
*  *                                                               *
*  *****************************************************************
*
* WARNING: assumes ASCII internal representation of characters
*
* INPUT:    STRING    -  Input string
*
      LOGICAL FUNCTION islett(string)
      IMPLICIT NONE

      CHARACTER*(*) string

      INTEGER i,icc

      islett=.false.
      DO 1 i=1,LEN(string)
      icc=ICHAR(string(i:i))
      IF(icc.GE.65 .AND. icc.LE.90) GOTO 1
      IF(icc.GE.97 .AND. icc.LE.122) GOTO 1
      RETURN
 1    CONTINUE

      islett=.true.

      END
