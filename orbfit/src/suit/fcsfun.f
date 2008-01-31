* Copyright (C) 2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 3, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         F C S F U N                           *
*  *                                                               *
*  *              LS fit of covariance functions:                  *
*  *       function integer codes and number of parameters         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    NAME      -  Function name
*
* OUTPUT:   KFUN      -  Function integer identificator (0=don't know)
*           NPAR      -  Number of parameters
*
      SUBROUTINE fcsfun(name,kfun,npar)
      IMPLICIT NONE

      INTEGER kfun,npar
      CHARACTER*(*) name

      kfun=0
      npar=0
* Multiplicative coefficient (constant)
      IF(name.EQ.'+COEF') THEN
          kfun=1
          npar=1
* Exponential function
      ELSEIF(name.EQ.'*EXP') THEN
          kfun=2
          npar=1
* Normal function
      ELSEIF(name.EQ.'*NORM') THEN
          kfun=3
          npar=1
* Parabola (to be multiplied by exponential or normal functions)
      ELSEIF(name.EQ.'*PARAB') THEN
          kfun=4
          npar=1
* ADD HERE NEW FUNCTIONS, using increasing integer identificator (KFUN)
* Remember to update accordingly also subroutines FCFUND and FCWPAR
      END IF

      END
