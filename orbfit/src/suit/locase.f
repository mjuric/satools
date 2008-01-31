* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 26, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         L O C A S E                           *
*  *                                                               *
*  *         Transforms a character string into lowercase          *
*  *                                                               *
*  *****************************************************************
*
* WARNING: assumes ASCII internal representation of characters
*
* INPUT:    STRING    -  Input string
*
* OUTPUT:   STRING    -  String transformed to lowercase
*
      SUBROUTINE locase(string)
      IMPLICIT NONE

      CHARACTER*(*) string

      INTEGER i,icc

      DO 1 i=1,LEN(string)
      icc=ICHAR(string(i:i))
      IF(icc.GE.65 .AND. icc.LE.90) string(i:i)=CHAR(icc+32)
 1    CONTINUE

      END
