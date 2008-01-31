* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 27, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         S E S S A G                           *
*  *                                                               *
*  *       Transform an angle into sessagesimal notation           *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    ANG       -  Angle
*
* OUTPUT:   SIGA      -  Sign
*           INTA      -  Integer part
*           MINA      -  Minutes
*           SECA      -  Seconds
*
      SUBROUTINE sessag(ang,siga,inta,mina,seca)
      IMPLICIT NONE

      DOUBLE PRECISION ang,seca
      INTEGER inta,mina,truncat
      CHARACTER*1 siga

      DOUBLE PRECISION anga,u


      IF(ang.GE.0.d0) THEN
          anga=ang
          siga='+'
      ELSE
          anga=ABS(ang)
          siga='-'
      END IF

      inta=truncat(anga,1d-10)
      u=(anga-inta)*60.d0
      u=ABS(u)
      mina=truncat(u,1d-8)
      seca=(u-mina)*60.d0

      END
