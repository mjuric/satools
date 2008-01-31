* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 8, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         G E T R S C                           *
*  *                                                               *
*  *         Get a data record skipping comment lines              *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Input unit
*           NR        -  Number of records read so far
*
* OUTPUT:   REC       -  Record
*           NR        -  Number of records read so far (updated)
*           END       -  End of file reached
*
      SUBROUTINE getrsc(unit,rec,nr,end)
      IMPLICIT NONE

      INCLUDE 'parcmc.h'

      INTEGER unit,nr
      CHARACTER*(*) rec
      LOGICAL end

      end=.false.

 1    CONTINUE
      READ(unit,100,END=2) rec
 100  FORMAT(A)
      nr=nr+1
      IF(rec(1:1).EQ.comcha) GOTO 1
      RETURN

 2    CONTINUE
      end=.true.
      rec=' '

      END
