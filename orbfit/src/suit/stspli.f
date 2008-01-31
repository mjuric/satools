* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: September 4, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         S T S P L I                           *
*  *                                                               *
*  *      Split a character string using a given separator         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    C         -  Character string
*           SEP       -  Separator
*
* OUTPUT:   C         -  Character string without first item
*           ITEM1     -  First item of the input string
*           NOSPLI    -  (LOG) if true, no splitting is possible
*
      SUBROUTINE stspli(c,sep,item1,nospli)
      IMPLICIT NONE

      CHARACTER*(*) c,item1
      CHARACTER sep*1,c1*200
      LOGICAL nospli
      INTEGER is

      INTEGER lench
      EXTERNAL lench

      nospli=.false.

      is=INDEX(c,sep)
      IF(is.LE.0) THEN
          IF(lench(c).LE.0) THEN
              nospli=.true.
              item1=' '
          ELSE
              item1=c
              c=' '
          END IF
      ELSE
          item1=c(1:is-1)
          c1=c(is+1:)
          c=c1
      END IF

      END
