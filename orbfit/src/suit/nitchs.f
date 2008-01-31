* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 10, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         N I T C H S                           *
*  *                                                               *
*  *  Number of items (separated by spaces) in a character string  *
*  *                                                               *
*  *****************************************************************
*
      INTEGER FUNCTION nitchs(c)
      IMPLICIT NONE

      INTEGER lc,i

      INCLUDE 'parch.h'

      CHARACTER*(*) c
      CHARACTER*(lchx) c1
      LOGICAL quoted

      CALL chkpdf(LEN(c),lchx,'lchx')

      c1=c
      CALL norstr(c1,lc)
      IF(lc.LE.0) THEN
          nitchs=0
          RETURN
      END IF

      nitchs=1
      quoted=.false.
      DO 1 i=1,lc
      IF(c1(i:i).EQ.'''') quoted=(.NOT.quoted)
      IF(quoted) GOTO 1
      IF(c1(i:i).EQ.' ') nitchs=nitchs+1
 1    CONTINUE

      END
