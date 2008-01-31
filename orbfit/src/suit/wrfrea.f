* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 21, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R F R E A                           *
*  *                                                               *
*  *                     Write a real value                        *
*  *           to a simplified file-header namelist                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Output unit
*           KEY       -  Keyword
*           VAL       -  Value
*           COMM      -  Comment
*
      SUBROUTINE wrfrea(unit,key,val,comm)
      IMPLICIT NONE

      INCLUDE 'parcmc.h'

* NEEDED common blocks:
      INCLUDE 'comwfr.h'

      INTEGER unit
      CHARACTER*(*) key,comm
      DOUBLE PRECISION val

      INTEGER lk,lc

      INTEGER lench
      EXTERNAL lench

      IF(iicwfr.NE.36) STOP '**** wrfrea: internal error (01) ****'

      lk=lench(key)
      lc=lench(comm)

      IF(lc.GT.0) THEN
          WRITE(unit,100) key(1:lk),val,comcha,comm(1:lc)
      ELSE
          WRITE(unit,101) key(1:lk),val
      END IF
 100  FORMAT(A,' =',1P,E22.14,1X,A,1X,A)
 101  FORMAT(A,' =',1P,E22.14)

      END
