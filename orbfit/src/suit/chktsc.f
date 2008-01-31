* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: February 24, 1997
*
*  *****************************************************************
*  *                                                               *
*  *                         C H K T S C                           *
*  *                                                               *
*  *             Check existence of a time scale                   *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    SCALE     -  Time scale
*
* OUTPUT:   ERROR     -  Error flag
*
      SUBROUTINE chktsc(scale,error)
      IMPLICIT NONE

      CHARACTER*(*) scale
      LOGICAL error

      error=.false.

      IF(scale.EQ.'UTC') RETURN
      IF(scale.EQ.'ET')  RETURN
      IF(scale.EQ.'TDT') RETURN
      IF(scale.EQ.'UT1') RETURN
      IF(scale.EQ.'TAI') RETURN
      IF(scale.EQ.'GPS') RETURN

      error=.true.

      END
