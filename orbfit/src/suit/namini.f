* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 10, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         N A M I N I                           *
*  *                                                               *
*  *           Namelist common block initialization                *
*  *                                                               *
*  *****************************************************************
*
      SUBROUTINE namini
      IMPLICIT NONE

      INCLUDE 'parnam.h'

* Common blocks to be initialized:
      INCLUDE 'comnam.h'

      nne=0
      kuorl=0
      CALL chkpdf(1,nnex,'nnex')

      iicnam=36

      END
