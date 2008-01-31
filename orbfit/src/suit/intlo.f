* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: April 2, 1996
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                          I N T L O                          *
*  *                                                             *
*  *                        Lower integer                        *
*  *                                                             *
*  ***************************************************************
*
* INPUT:    A         -  Value
*
* OUTPUT:   INTLO     -  Lower integer INTLO <= A < INTLO+1
*
      INTEGER FUNCTION intlo(a)
      IMPLICIT NONE

      DOUBLE PRECISION a

      intlo=a
      IF(intlo.GT.a) intlo=intlo-1

      END
