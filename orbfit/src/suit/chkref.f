* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 7, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C H K R E F                           *
*  *                                                               *
*  *       Check existence of a reference system indicator         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    RSYS      -  Reference system
*           EPOCH     -  Epoch
*
* OUTPUT:   ERROR     -  Error flag
*
      SUBROUTINE chkref(rsys,epoch,error)
      IMPLICIT NONE

      CHARACTER*(*) rsys,epoch
      LOGICAL error

      error=.true.

      IF(rsys.EQ.'EQUM') GOTO 1
      IF(rsys.EQ.'EQUT') GOTO 1
      IF(rsys.EQ.'ECLM') GOTO 1
      RETURN

 1    CONTINUE
      IF(epoch.EQ.'J2000') GOTO 2
      IF(epoch.EQ.'OFDATE') GOTO 2
      RETURN

 2    CONTINUE
      error=.false.

      END
