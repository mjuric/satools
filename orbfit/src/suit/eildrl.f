* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: September 11, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         E I L D R L                           *
*  *                                                               *
*  *         Emulation of internal, list-directed read             *
*  *                   for logical variables                       *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    STRING    -  Character string
*
* OUTPUT:   V         -  Logical value
*           REST      -  Remaining (unread) part of the input string
*           ERROR     -  Error flag
*
      SUBROUTINE eildrl(string,v,rest,error)
      IMPLICIT NONE

      INCLUDE 'spaces1.h'

      CHARACTER*(*) string,rest
      LOGICAL v
      LOGICAL error

      INTEGER ls,i1,i2,icc,k,ii
      CHARACTER fmt*10

      INTEGER lench
      EXTERNAL lench

      INCLUDE 'spaces2.h'

      ls=lench(string)

* Skip leading spaces
      DO 1 i1=1,ls
      icc=ICHAR(string(i1:i1))
      DO 2 k=1,nspac
      IF(icc.EQ.icspac(k)) GOTO 1
 2    CONTINUE
      GOTO 3
 1    CONTINUE
      GOTO 10

* Find end of the string
 3    CONTINUE
      DO 4 i2=i1+1,ls
      icc=ICHAR(string(i2:i2))
      DO 5 k=1,nspac
      IF(icc.EQ.icspac(k)) GOTO 6
 5    CONTINUE
 4    CONTINUE
 6    CONTINUE
      i2=i2-1
      ii=i2-i1+1
      IF(ii.LE.0 .OR. ii.GT.99)
     +    STOP '**** eildrl: internal error (01) ****'
      WRITE(fmt,100) ii
 100  FORMAT('(L',I2.2,')')
      READ(string(i1:),fmt,ERR=10) v
      rest=string(i2+1:)
      error=.false.
      RETURN

 10   CONTINUE
      error=.true.
      v=.false.

      END
