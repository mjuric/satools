* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 9, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         D I U R O T                           *
*  *                                                               *
*  *       Diurnal rotation matrix (with time derivatives)         *
*  *                                                               *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    MJDE      -  Modified Julian Time (ET): integer part
*           SECE      -  Modified Julian Time (ET): seconds within
*                        the day (0 <= SECU < 86400)
*           NDER      -  Required order for derivatives (0/1/2)
*
* OUTPUT:   ROT       -  Diurnal rotation matrix D(t)
*           ROT1      -  First time derivative dD/dt (s^(-1))
*                        (only if NDER >= 1)
*           ROT2      -  Second time derivative d^2D/dt^2 (s^(-2))
*                        (only if NDER = 2)
*
* The diurnal rotation matrix D(t) performs the transformation from
* the cartesian coordinates  Xtrue referred to the true equator and
* equinox of date (at time t) to the body-fixed coordinates XBF,
* referred to the true equator and Greenwich meridian (at the same
* time t)
*
*  XBF = D(t) Xtrue
*
      SUBROUTINE diurot(mjde,sece,rot,rot1,rot2,nder)
      IMPLICIT NONE

      INTEGER mjde,nder
      DOUBLE PRECISION sece,rot(3,3),rot1(3,3),rot2(3,3)

      INTEGER mjdu
      DOUBLE PRECISION dt,dt1,dt2,tjme,eqeq,eqeq1,eqeq2,secu,utdet
      DOUBLE PRECISION gmst,gmstd1,gmstd2,gast,gast1,gast2,utddet
      DOUBLE PRECISION xpol(2),x1pol(2),x2pol(2)
      DOUBLE PRECISION ra(3,3),rb(3,3),rc(3,3)
      DOUBLE PRECISION ra1(3,3),rb1(3,3),rc1(3,3)
      DOUBLE PRECISION ra2(3,3),rb2(3,3),rc2(3,3)

      IF(nder.LT.0 .OR. nder.GT.2) STOP '**** diurot: nder = ? ****'

* Computation of DeltaT = ET-UT1
      CALL delthp(mjde,sece,dt,dt1,dt2,nder)

* Computation of the coordinates of the terrestrial pole Xpol and Ypol
      tjme=mjde+sece/86400.d0
      CALL xypol(tjme,xpol,x1pol,x2pol,nder)

* Computation of the equation of the equinoxes Delta_psi*cos(eps)
      CALL equeqd(tjme,eqeq,eqeq1,eqeq2,nder)

* Computation of UT1 = ET-DeltaT
      mjdu=mjde
      secu=sece-dt
      CALL timnf(mjdu,secu,'UT1')

* Computation of Greenwich Mean Sidereal Time Theta_mean
      CALL gmstd(mjdu,secu,gmst,gmstd1,gmstd2,nder)

* Computation of Greenwich Apparent Sidereal Time
* Theta_app = Theta_mean + Delta_psi*cos(eps)
      gast=gmst+eqeq

* D(t) = R2(-Xpol) R1(-Ypol) R3(Theta_app)
      CALL rotmt( -xpol(1) , ra , 2)
      CALL rotmt( -xpol(2) , rb , 1)
      CALL rotmt(     gast , rc , 3)

      IF(nder.GE.1) THEN

*  dUT1/dET = 1 - dDeltaT/dET
          utdet=1.d0-dt1

*  dTheta_app/dET =
*       = dTheta_mean/dET + d[Delta_psi*cos(eps)]/dET
*       = dTheta_mean/dUT1*dUT1/dET + d[Delta_psi*cos(eps)]/dET
          gast1=gmstd1*utdet+eqeq1
          CALL rotmt1( -xpol(1) , ra1 , 2, -x1pol(1))
          CALL rotmt1( -xpol(2) , rb1 , 1, -x1pol(2))
          CALL rotmt1(     gast , rc1 , 3,     gast1)
      END IF

      IF(nder.GE.2) THEN

*  d^2 UT1/dET^2 = -d^2 DeltaT/dET^2
          utddet=-dt2

*  d^2 Theta_app/dET^2 =
*       = d^2 Theta_mean/dET^2 + d^2[(Delta_psi*cos(eps)]/dET^2
*       = d^2 Theta_mean/dUT1^2 * (dUT1/dET)^2 +
*         + dTheta_ mean/dUT1 * d^2 UT1/dET^2 +
*         + d^2 [Delta_psi*cos(eps)]/dET^2
          gast2=gmstd2*(utdet**2)+gmstd1*utddet+eqeq2
          CALL rotmt2( -xpol(1) , ra2 , 2, -x1pol(1), -x2pol(1))
          CALL rotmt2( -xpol(2) , rb2 , 1, -x1pol(2), -x2pol(2))
          CALL rotmt2(     gast , rc2 , 3,     gast1,     gast2)
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
