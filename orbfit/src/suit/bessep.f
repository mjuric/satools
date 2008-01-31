* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 5, 1999
* ---------------------------------------------------------------------
*
* Besselian epoch as a function of Modified Julian Date
*
      DOUBLE PRECISION FUNCTION bessep(tjme)
      IMPLICIT NONE

      DOUBLE PRECISION tjme

      INCLUDE 'bessyr.h'

      bessep=1900.d0+(tjme-15019.81352d0)/bessyr

      END
