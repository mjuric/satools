* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 16, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         I S B A D R                           *
*  *                                                               *
*  *          Check whether a record is a data record              *
*  *                   from IERS Bulletin A                        *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    REC       -  Input record
*
      LOGICAL FUNCTION isbadr(rec)
      IMPLICIT NONE

      CHARACTER*(*) rec

      LOGICAL isnum,isbnum
      EXTERNAL isnum,isbnum

      isbadr=.false.

* Example of record:
*
*       1999 11 12  51494        .0277       .3755       .41726
*12345678901234567890123456789012345678901234567890123456789012
*         1         2         3         4         5         6

      IF(rec(1:7).NE.'       ') RETURN
      IF(.NOT.isnum(rec(8:11))) RETURN
      IF(rec(12:12).NE.' ') RETURN
      IF(.NOT.isbnum(rec(13:14))) RETURN
      IF(rec(15:15).NE.' ') RETURN
      IF(.NOT.isbnum(rec(16:17))) RETURN
      IF(rec(18:19).NE.'  ') RETURN
      IF(.NOT.isnum(rec(20:24))) RETURN
      IF(rec(25:28).NE.'    ') RETURN
      IF(rec(33:33).NE.'.') RETURN
      IF(.NOT.isbnum(rec(34:37))) RETURN
      IF(rec(38:41).NE.'    ') RETURN
      IF(rec(45:45).NE.'.') RETURN
      IF(.NOT.isbnum(rec(46:49))) RETURN
      IF(rec(50:53).NE.'    ') RETURN
      IF(rec(57:57).NE.'.') RETURN
      IF(.NOT.isbnum(rec(58:62))) RETURN

      isbadr=.true.

      END
