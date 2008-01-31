* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         E P S I N I                           *
*  *                                                               *
*  *             Computation of round-off precision                *
*  *                                                               *
*  *****************************************************************
*
      SUBROUTINE epsini
      IMPLICIT NONE

      INTEGER n
      DOUBLE PRECISION eps1

* Common blocks to be initialized:
      INCLUDE 'comeps.h'

      eps=1.d0
      n=0
 1    eps1=eps/2.d0
      IF(1.d0+eps1.EQ.1.d0) THEN
          l2eps=n
          iiceps=36
          RETURN
      END IF
      eps=eps/2.d0
      n=n+1
      IF(n.GT.300) STOP '**** epsini: internal error (01) ****'
      GOTO 1

      END
