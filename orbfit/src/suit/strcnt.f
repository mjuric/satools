* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 26, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         S T R C N T                           *
*  *                                                               *
*  *                Content of a character string                  *
*  *               (enclosed within quotes or not)                 *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    STRING    -  Input string
*
* OUTPUT:   CONT      -  String content
*           REST      -  Rest of the input string (after the content)
*           ERROR     -  Error flag
*
      SUBROUTINE strcnt(string,cont,rest,error)
      IMPLICIT NONE

      INCLUDE 'spaces1.h'

      CHARACTER*(*) string,cont,rest
      LOGICAL error

      INTEGER ls,icc,i,k,i1,i2
      LOGICAL quoted
      CHARACTER*1 qch

      INCLUDE 'spaces2.h'

* Determine the beginning of the CONTENT string, by discarding leading
* spaces and finding the position of the opening quotes.
* I1 is the position of the first character of CONTENT;
* QCH is the quote character found
      ls=LEN(string)
      DO 1 i=1,ls
      icc=ICHAR(string(i:i))
* Skip leading space characters
      DO 2 k=1,nspac
      IF(icc.EQ.icspac(k)) GOTO 1
 2    CONTINUE
      i1=i
* Look if the first non-space character is a quote
      IF(string(i1:i1).EQ.'''') THEN
          quoted=.true.
          qch=''''
          i1=i1+1
      ELSEIF(string(i1:i1).EQ.'"') THEN
          quoted=.true.
          qch='"'
          i1=i1+1
      ELSE
          quoted=.false.
      END IF
      GOTO 3
 1    CONTINUE
* The input string contains only space characters
      error=.false.
      cont=' '
      rest=' '
      RETURN
 3    CONTINUE
* Determine the end of the CONTENT string, by looking for a
* second occurrence of the quote character (QCH)
* I2 is the position of the last character of CONTENT
      IF(quoted) THEN
          DO 4 i=i1,ls
          IF(string(i:i).EQ.qch) THEN
              i2=i-1
              GOTO 7
          END IF
 4        CONTINUE
* Missing closing quotes
          error=.true.
          cont=' '
          rest=' '
          RETURN
      ELSE
          DO 5 i=i1,ls
          icc=ICHAR(string(i:i))
          DO 6 k=1,nspac
          IF(icc.EQ.icspac(k)) THEN
              i2=i-1
              GOTO 7
          END IF
 6        CONTINUE
 5        CONTINUE
          i2=ls
      END IF
* Extract CONTENT and REST
 7    CONTINUE
      IF(i2.LT.i1) THEN
          cont=' '
      ELSE
          IF(LEN(cont).LT.i2-i1+1)
     +        STOP '**** strcnt: insufficient length for CONT ****'
          cont=string(i1:i2)
      END IF
      error=.false.
      IF(i2.GT.ls-2) THEN
          rest=' '
      ELSE
          IF(LEN(rest).LT.ls-i2-1)
     +        STOP '**** strcnt: insufficient length for REST ****'
          rest=string(i2+2:)
      END IF

      END
