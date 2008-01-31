* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 7, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C L O R B F                           *
*  *                                                               *
*  *                Close an orbital element file                  *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  File name
*
      SUBROUTINE clorbf
      IMPLICIT NONE

      INCLUDE 'comorb.h'

      IF(orbunt.LE.0) STOP '**** clorbf: internal error (01) ****'

      CALL filclo(orbunt,' ')

      orbunt=0
      orbfn=' '
      iicorb=0

      END
