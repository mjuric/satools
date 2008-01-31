* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 21, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R F I N T                           *
*  *                                                               *
*  *                   Write an integer value                      *
*  *           to a simplified file-header namelist                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Output unit
*           KEY       -  Keyword
*           VAL       -  Value
*           COMM      -  Comment
*
      SUBROUTINE wrfint(unit,key,val,comm)
      IMPLICIT NONE

      INCLUDE 'parcmc.h'

* NEEDED common blocks:
      INCLUDE 'comwfi.h'

      INTEGER unit
      CHARACTER*(*) key,comm
      INTEGER val

      INTEGER lk,lc

      INTEGER lench
      EXTERNAL lench

      IF(iicwfi.NE.36) STOP '**** wrfint: internal error (01) ****'

      lk=lench(key)
      lc=lench(comm)

      IF(lc.GT.0) THEN
          WRITE(unit,100) key(1:lk),val,comcha,comm(1:lc)
      ELSE
          WRITE(unit,101) key(1:lk),val
      END IF
 100  FORMAT(A,' =',I8,1X,A,1X,A)
 101  FORMAT(A,' =',I8)

      END
