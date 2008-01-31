* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 20, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R F L O G                           *
*  *                                                               *
*  *                   Write a logical value                       *
*  *           to a simplified file-header namelist                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Output unit
*           KEY       -  Keyword
*           VAL       -  Value
*           COMM      -  Comment
*
      SUBROUTINE wrflog(unit,key,val,comm)
      IMPLICIT NONE

      INCLUDE 'parcmc.h'

      INTEGER unit
      CHARACTER*(*) key,comm
      LOGICAL val

      INTEGER lk,lc

      INTEGER lench
      EXTERNAL lench

      lk=lench(key)
      lc=lench(comm)

      IF(lc.GT.0) THEN
          WRITE(unit,100) key(1:lk),val,comcha,comm(1:lc)
      ELSE
          WRITE(unit,101) key(1:lk),val
      END IF
 100  FORMAT(A,' = ',L1,1X,A,1X,A)
 101  FORMAT(A,' = ',L1)

      END
