* Copyright (C) 2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 11, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         F I L S T R                           *
*  *                                                               *
*  *              Fill a string with another string                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    CIN       -  Input character string
*           LT        -  Total length of output string
*           NBI       -  Initial number of blanks (when possible)
*           JUST      -  Justification (-1=left, 0=center, 1=right)
*
* OUTPUT:   COUT      -  Output character string
*
      SUBROUTINE filstr(cin,cout,lt,nbi,just)
      IMPLICIT NONE

      INTEGER lt,nbi,just
      CHARACTER*(*) cin,cout

      INTEGER li,nr1,nr2,nb1,nb2,nbt
      CHARACTER*100 blank

      INTEGER lench
      EXTERNAL lench

      blank=' '
      li=lench(cin)

* NR1 = number of characters left in COUT after filling in CIN
      nr1=lt-li
      IF(nr1.LT.nbi) THEN
          nb1=nr1
          IF(nb1.GT.0) THEN
              cout=blank(1:nb1)//cin
          ELSE
              cout=cin
          END IF
          RETURN
      END IF

* NR2 = number of characters left in COUT after filling in CIN and
*       NBI initial blanks
      nr2=lt-li-nbi
      IF(nr2.LE.0) THEN
          cout=blank(1:nbi)//cin
          RETURN
      END IF

      IF(just.EQ.0) THEN
          nb2=nr2/2
      ELSEIF(just.LT.0) THEN
          nb2=0
      ELSE
          nb2=nr2
      END IF

      nbt=nbi+nb2
      IF(nbt.GT.0) THEN
          cout=blank(1:nbt)//cin
      ELSE
          cout=cin
      END IF

      END
