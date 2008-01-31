* Copyright (C) 1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 3, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         D I F I N I                           *
*  *                                                               *
*  *        Initialization of options for iteration control        *
*  *                     in SUBROUTINE DIFCOR                      *
*  *                                                               *
*  *****************************************************************
*
      SUBROUTINE difini
      IMPLICIT NONE

* Common blocks to be initialized:
      INCLUDE 'comdif.h'

      LOGICAL found,fail1,fail

      fail=.false.

      batch=.false.
      CALL rdnlog('difcor.','batch',batch,.false.,found,
     +            fail1,fail)

      itmax=20
      CALL rdnint('difcor.','nit_max',itmax,.false.,found,
     +            fail1,fail)

      itgmax=5
      CALL rdnint('difcor.','nitg_max',itgmax,.false.,found,
     +            fail1,fail)

      divrat=0.999d0
      CALL rdnrea('difcor.','div_cntr',divrat,.false.,found,
     +            fail1,fail)

      IF(fail) STOP '**** difini: abnormal end ****'

      iicdif=36

      END
