* Copyright (C) 1999 by A. Milani
* Version: February 4, 1999
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                          I N T H I                          *
*  *                                                             *
*  *                        Higher integer                       *
*  *                                                             *
*  ***************************************************************
*
* INPUT:    A         -  Value
*
* OUTPUT:   INTLO     -  Higher integer INTLO-1 <= A < INTLO
*
      INTEGER FUNCTION inthi(a)
      IMPLICIT NONE

      DOUBLE PRECISION a

      inthi=a
      IF(inthi.LT.a) inthi=inthi+1

      END
