* Copyright (C) 1996-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: January 12, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         F I L C L O                           *
*  *                                                               *
*  *                File closing and unit release                  *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    IUN       -  Unit to be closed
*           STATUS    -  Close status (if blank, it is not used)
*
      SUBROUTINE filclo(iun,status)
      IMPLICIT NONE

      INCLUDE 'comfil.h'

      INTEGER iun,ls
      CHARACTER*(*) status

      INTEGER lench
      EXTERNAL lench

      IF(iicfil.NE.36) STOP '**** filclo: internal error (01) ****'
      IF(iun.LT.iunf1.OR.iun.GT.iunf2)
     +          STOP '**** filclo: internal error (02) ****'
      IF(.NOT.allunt(iun)) THEN
          WRITE(*,200) iun
          STOP
      END IF
 200  FORMAT(' **** filclo: unit',i4,' is not opened ****')

      ls=lench(status)
      IF(ls.LE.0) THEN
          CLOSE(iun)
      ELSE
          CLOSE(iun,STATUS=status)
      END IF

      allunt(iun)=.false.
      filnam(iun)=' '

      END
