* Copyright (C) 2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 16, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         F C O R O B                           *
*  *                                                               *
*  *              Computation of correlation function              *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    KFUN      -  Function integer codes
*           NFUN      -  Number of elementary functions
*           PAR       -  Parameter values
*           NPARF     -  Number of parameters per function
*           DT        -  Time lag
*
* OUTPUT:   FUN       -  Function value
*
      SUBROUTINE fcorob(kfun,nfun,par,nparf,dt,fun)
      IMPLICIT NONE

      INTEGER nfun
      INTEGER nparf(nfun),kfun(nfun)
      DOUBLE PRECISION par(*),dt,fun

      INTEGER if1,ip1,kf1
      DOUBLE PRECISION par1,ff1,fd1,term

      fun=0
      term=0

      ip1=1
      DO 1 if1=1,nfun
      kf1=kfun(if1)
      par1=par(ip1)

      INCLUDE 'fcfund.h'

      ELSE
          STOP '**** fcorob: internal error (01) ****'
      END IF

      IF(kf1.EQ.1) THEN
          fun=fun+term
          term=ff1
      ELSE
          term=term*ff1
      END IF
      ip1=ip1+nparf(if1)

 1    CONTINUE
      fun=fun+term

      END
