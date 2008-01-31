* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 9, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          P R E C D                            *
*  *                                                               *
*  *             Precession matrix (with time derivatives)         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Time (ET)
*           NDER      -  Required order for derivatives (0/1/2)
*
* OUTPUT:   ROT       -  Precession matrix P(t)
*           ROT1      -  First time derivative dP/dt (s^(-1))
*                        (only if NDER >= 1)
*           ROT2      -  Second time derivative d^2P/dt^2 (s^(-2))
*                        (only if NDER = 2)
*
* The precession matrix P(t) performs the transformation from the
* cartesian coordinates Xj2000 referred to the mean equator and equinox
* of J2000 to the coordinates Xt referred to the mean equator and
* equinox of date (corresponding to time t)
*
*  Xt = P(t) Xj2000
*
      SUBROUTINE precd(tjme,rot,rot1,rot2,nder)
      IMPLICIT NONE

      INTEGER nder
      DOUBLE PRECISION tjme,rot(3,3),rot1(3,3),rot2(3,3)

      INCLUDE 'trig.h'

      DOUBLE PRECISION s2r,s2c,s2c2,tt,zeta,z,theta,zeta1,z1,theta1
      DOUBLE PRECISION zeta2,z2,theta2
      DOUBLE PRECISION zed,zd,thd,zedd,zdd,thdd,zeddd,zddd,thddd
      DOUBLE PRECISION ra(3,3),rb(3,3),rc(3,3)
      DOUBLE PRECISION ra1(3,3),rb1(3,3),rc1(3,3)
      DOUBLE PRECISION ra2(3,3),rb2(3,3),rc2(3,3)
      LOGICAL first


      SAVE first,s2r,s2c,s2c2,zed,zd,thd,zedd,zdd,thdd,zeddd,zddd,thddd

      DOUBLE PRECISION ssum,dsum,ddsum,c1,c2,c3,t
      DATA first/.true./

* Computation of a 3rd  degree polynomial with derivatives
      ssum  (c1,c2,c3,t) = ((c3*t+c2)*t+c1)*t
      dsum (c1,c2,c3,t) = (3.d0*c3*t+2.d0*c2)*t+c1
      ddsum(c1,c2,c3,t) = 6.d0*c3*t+2.d0*c2

      IF(nder.LT.0 .OR. nder.GT.2) STOP '**** precd: nder = ? ****'

* Time series for precession angles (zeta_A, z_A and theta_A):
* IAU 1980 coefficients (see Supplement to the Astronomical Almanac
* 1984, page S19)
      IF(first) THEN
          first=.false.
          s2r = pig/(180.d0*3600.d0)
          s2c = 1.d0/(36525.d0*86400.d0)
          s2c2= s2c**2
* Linear term
          zed   =   2306.2181d0 * s2r
          zd    =   2306.2181d0 * s2r
          thd   =   2004.3109d0 * s2r
* Quadratic term
          zedd  =   0.30188d0 * s2r
          zdd   =   1.09468d0 * s2r
          thdd  = - 0.42665d0 * s2r
* Cubic term
          zeddd =   0.017998d0 * s2r
          zddd  =   0.018203d0 * s2r
          thddd = - 0.041833d0 * s2r
      END IF

* Computation of fundamental angles zeta_A, z_A and theta_A
      tt    = ( tjme - 51544.5d0 ) / 36525.d0
      zeta  = ssum(zed,zedd,zeddd,tt)
      z     = ssum( zd, zdd, zddd,tt)
      theta = ssum(thd,thdd,thddd,tt)

* P(t) = R3 (-z_A) R2(theta_A) R3(-zeta_A)
      CALL rotmt(    -z , ra , 3)
      CALL rotmt( theta , rb , 2)
      CALL rotmt( -zeta , rc , 3)
      IF(nder.GE.1) THEN
          zeta1  = dsum(zed,zedd,zeddd,tt) * s2c
          z1     = dsum( zd, zdd, zddd,tt) * s2c
          theta1 = dsum(thd,thdd,thddd,tt) * s2c
          CALL rotmt1(    -z , ra1 , 3,    -z1)
          CALL rotmt1( theta , rb1 , 2, theta1)
          CALL rotmt1( -zeta , rc1 , 3, -zeta1)
      END IF
      IF(nder.GE.2) THEN
          zeta2  = ddsum(zed,zedd,zeddd,tt) * s2c2
          z2     = ddsum( zd, zdd, zddd,tt) * s2c2
          theta2 = ddsum(thd,thdd,thddd,tt) * s2c2
          CALL rotmt2(    -z , ra2 , 3,    -z1,    -z2)
          CALL rotmt2( theta , rb2 , 2, theta1, theta2)
          CALL rotmt2( -zeta , rc2 , 3, -zeta1, -zeta2)
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
