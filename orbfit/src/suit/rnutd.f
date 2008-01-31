* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 9, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          R N U T D                            *
*  *                                                               *
*  *     Nutation matrix according to Wahr (IAU 1980) theory       *
*  *                   (with time derivatives)                     *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Time (ET)
*           NDER      -  Required order for derivatives (0/1/2)
*
* OUTPUT:   ROT       -  Nutation matrix N(Delta_psi,Delta_eps)
*           ROT1      -  First time derivative dN/dt (s^(-1))
*                        (only if NDER >= 1)
*           ROT2      -  Second time derivative d^2N/dt^2 (s^(-2))
*                        (only if NDER = 2)
*
* The nutation matrix N(t) performs the transformation from the
* cartesian coordinates referred to the mean equator and equinox
* of date (at time t) Xmean to the coordinates referred to the true
* equator and equinox of date (at the same time t) Xtrue
*
*  Xtrue = N(t) Xmean
*
      SUBROUTINE rnutd(tjme,rot,rot1,rot2,nder)
      IMPLICIT NONE

      INTEGER nder
      DOUBLE PRECISION tjme,rot(3,3),rot1(3,3),rot2(3,3)

      DOUBLE PRECISION epsm,epsm1,epsm2,epst,epst1,epst2
      DOUBLE PRECISION dpsi,deps,dpsi1,deps1,dpsi2,deps2
      DOUBLE PRECISION ra(3,3),rb(3,3),rc(3,3)
      DOUBLE PRECISION ra1(3,3),rb1(3,3),rc1(3,3)
      DOUBLE PRECISION ra2(3,3),rb2(3,3),rc2(3,3)

      IF(nder.LT.0 .OR. nder.GT.2) STOP '**** rnutd: nder = ? ****'

* Obliquity of the ecliptic eps(t)
      CALL obleqd(tjme,epsm,epsm1,epsm2,nder)

* Nutation angles Delta_psi(t) and Delta_eps(t)
      CALL nutnd(tjme,dpsi,deps,dpsi1,deps1,dpsi2,deps2,nder)

*  N(Delta_psi,Delta_eps) = R1(-eps-Delta_eps) R3(-Delta_psi) R1(eps)
      epst = epsm + deps
      CALL rotmt( -epst , ra , 1)
      CALL rotmt( -dpsi , rb , 3)
      CALL rotmt(  epsm , rc , 1)
      IF(nder.GE.1) THEN
          epst1 = epsm1 + deps1
          CALL rotmt1( -epst , ra1 , 1, -epst1)
          CALL rotmt1( -dpsi , rb1 , 3, -dpsi1)
          CALL rotmt1(  epsm , rc1 , 1,  epsm1)
      END IF
      IF(nder.GE.2) THEN
          epst2 = epsm2 + deps2
          CALL rotmt2( -epst , ra2 , 1, -epst1, -epst2)
          CALL rotmt2( -dpsi , rb2 , 3, -dpsi1, -dpsi2)
          CALL rotmt2(  epsm , rc2 , 1,  epsm1,  epsm2)
      END IF

      IF(nder.GE.2) CALL assmat(rot2,rc2)
      IF(nder.GE.1) CALL assmat(rot1,rc1)
      CALL assmat(rot,rc)

      IF(nder.GE.2) CALL pd2mat(rb,rb1,rb2,rot,rot1,rot2)
      IF(nder.GE.1) CALL pd1mat(rb,rb1,rot,rot1)
      CALL pdmat(rb,rot)

      IF(nder.GE.2) CALL pd2mat(ra,ra1,ra2,rot,rot1,rot2)
      IF(nder.GE.1) CALL pd1mat(ra,ra1,rot,rot1)
      CALL pdmat(ra,rot)

      END
