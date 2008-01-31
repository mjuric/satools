* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 26, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         U P C A S E                           *
*  *                                                               *
*  *         Transforms a character string into uppercase          *
*  *                                                               *
*  *****************************************************************
*
* WARNING: assumes ASCII internal representation of characters
*
* INPUT:    STRING    -  Input string
*
* OUTPUT:   STRING    -  String transformed to uppercase
*
      SUBROUTINE upcase(string)
      IMPLICIT NONE

      CHARACTER*(*) string

      INTEGER i,icc

      DO 1 i=1,LEN(string)
      icc=ICHAR(string(i:i))
      IF(icc.GE.97 .AND. icc.LE.122) string(i:i)=CHAR(icc-32)
 1    CONTINUE

      END
