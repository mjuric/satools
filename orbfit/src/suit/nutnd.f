* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 10, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          N U T N D                            *
*  *                                                               *
*  *            Nutation angles with time derivatives              *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Day (ET)
*           NDER      -  Required order for derivatives (0/1/2)
*
* OUTPUT:   DPSI      -  Nutation in longitude Dpsi (rad)
*           DEPS      -  Nutation in obliquity Deps (rad)
*           DPSI1     -  dDpsi/dt (rad/s) (only if NDER >= 1)
*           DEPS1     -  dDeps/dt (rad/s) (only if NDER >= 1)
*           DPSI2     -  d^2 Dpsi/dt^2 (rad/s^2) (only if NDER = 2)
*           DEPS2     -  d^2 Deps/dt^2 (rad/s^2) (only if NDER = 2)
*
      SUBROUTINE nutnd(tjme,dpsi,deps,dpsi1,deps1,dpsi2,deps2,nder)
      IMPLICIT NONE

      INTEGER nder
      DOUBLE PRECISION tjme,dpsi,deps,dpsi1,deps1,dpsi2,deps2

* OPTIONS:
* NUTCOR: correction to nutation theory
*         0 - No correction
*         1 - IERS correction
      INTEGER nutcor
      PARAMETER (nutcor=1)
* Time tollerance (years)
      DOUBLE PRECISION ttol
      PARAMETER (ttol=1.D-8)

      DOUBLE PRECISION fc0,fc1,fc2,eop(5),eopd(5),eopdd(5)
      LOGICAL first


      SAVE first,fc0,fc1,fc2
      DATA first/.true./

      IF(first) THEN
          first=.false.
          fc0=ATAN(1.d0)/(45.d0*3600.d0)
          fc1=fc0/86400.d0
          fc2=fc1/86400.d0
      END IF

* Nominal model for nutation
      CALL nutwhr(tjme,dpsi,deps,dpsi1,deps1,dpsi2,deps2,nder)

* Corrections to the nominal model
      IF(nutcor.LT.0.OR.nutcor.GT.1) STOP '**** nutnd: nutcor = ? ****'
      IF(nutcor.EQ.1) THEN
          CALL iersts(tjme,eop,eopd,eopdd,nder)
          dpsi  = dpsi  + fc0 * eop(4)
          deps  = deps  + fc0 * eop(5)
          dpsi1 = dpsi1 + fc1 * eopd(4)
          deps1 = deps1 + fc1 * eopd(5)
          dpsi2 = dpsi2 + fc2 * eopdd(4)
          deps2 = deps2 + fc2 * eopdd(5)
      END IF

      END
