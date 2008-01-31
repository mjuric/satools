* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 9, 1996
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                          L E N C H                          *
*  *                                                             *
*  *       Computation of the length of a character string       *
*  *                  ignoring trailing spaces                   *
*  *                                                             *
*  ***************************************************************
*
      INTEGER FUNCTION lench(c)
      IMPLICIT NONE

      INTEGER icc,k

      INCLUDE 'spaces1.h'

      CHARACTER c*(*)

      INCLUDE 'spaces2.h'

      lench=LEN(c)
 1    CONTINUE

      icc=ICHAR(c(lench:lench))
      DO 3 k=1,nspac
      IF(icc.eq.icspac(k)) GOTO 2
 3    CONTINUE
      RETURN

 2    lench=lench-1
      IF(lench.le.0) RETURN
      GOTO 1

      END
