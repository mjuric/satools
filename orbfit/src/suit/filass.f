* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: September 19, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         F I L A S S                           *
*  *                                                               *
*  *               Unit allocation for file opening                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    NAME      -  File name to be opened
*
* OUTPUT:   IUN       -  Allocated unit
*
      SUBROUTINE filass(iun,name)
      IMPLICIT NONE

      INCLUDE 'comfil.h'

      INTEGER iun,i
      CHARACTER*(*) name

      IF(iicfil.NE.36) THEN
          DO 1 i=iunf1,iunf2
 1        allunt(i)=.false.
          iicfil=36
      END IF

      DO 2 iun=iunf1,iunf2
      IF(allunt(iun)) GOTO 2
      filnam(iun)=name
      allunt(iun)=.true.
      RETURN
 2    CONTINUE

      STOP '**** filass: all units are already allocated ****'

      END
