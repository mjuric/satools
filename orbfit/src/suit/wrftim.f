* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 21, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R F T I M                           *
*  *                                                               *
*  *                     Write a real value                        *
*  *           to a simplified file-header namelist                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Output unit
*           KEY       -  Keyword
*           MJD       -  Modified Julian Date (integer part)
*           SEC       -  Seconds within the day
*           SCALE     -  Time scale
*           COMM      -  Comment
*
      SUBROUTINE wrftim(unit,key,mjd,sec,scale,comm)
      IMPLICIT NONE

      INCLUDE 'parcmc.h'

      INTEGER unit
      CHARACTER*(*) key,comm
      INTEGER mjd
      DOUBLE PRECISION sec
      CHARACTER*(*) scale

      INTEGER lk,lc,ls

      INTEGER lench
      EXTERNAL lench

      lk=lench(key)
      lc=lench(comm)
      ls=lench(scale)

      IF(lc.GT.0) THEN
          WRITE(unit,100) key(1:lk),mjd,sec,scale(1:ls),
     +                    comcha,comm(1:lc)
      ELSE
          WRITE(unit,101) key(1:lk),mjd,sec,scale(1:ls)
      END IF
 100  FORMAT(A,' = MJD ',I6,1X,F10.3,1X,A,1X,A,1X,A)
 101  FORMAT(A,' = MJD ',I6,1X,F10.3,1X,A)

      END
