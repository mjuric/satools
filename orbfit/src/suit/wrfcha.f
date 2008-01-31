* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 20, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R F C H A                           *
*  *                                                               *
*  *                  Write a character string                     *
*  *           to a simplified file-header namelist                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Output unit
*           KEY       -  Keyword
*           VAL       -  Value
*           COMM      -  Comment
*
      SUBROUTINE wrfcha(unit,key,val,comm)
      IMPLICIT NONE

      INCLUDE 'parcmc.h'

      INTEGER unit
      CHARACTER*(*) key,comm
      CHARACTER*(*) val

      INTEGER lk,lv,lc

      INTEGER lench
      EXTERNAL lench

      lk=lench(key)
      lv=lench(val)
      lc=lench(comm)

      IF(lc.GT.0) THEN
          WRITE(unit,100) key(1:lk),val(1:lv),comcha,comm(1:lc)
      ELSE
          WRITE(unit,101) key(1:lk),val(1:lv)
      END IF
 100  FORMAT(A,' = ''',A,''' ',A,1X,A)
 101  FORMAT(A,' = ''',A,'''')

      END
