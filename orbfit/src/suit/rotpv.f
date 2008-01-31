* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 9, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          R O T P V                            *
*  *                                                               *
*  *        Computation of position and velocity vectors           *
*  *               in different reference frames                   *
*  *                                                               *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    RSYS1     -  Initial reference system
*           DRAG1     -  Dragging of velocities in the initial
*                        reference system (see below)
*           MJD1      -  Modified Julian Day (ET) of the initial
*                        reference system (integer part)
*           SEC1      -  Modified Julian Day (ET) of the initial
*                        reference system (seconds within the day)
*           P1        -  Position vector in RSYS1
*           V1        -  Velocity vector in RSYS1
*           RSYS2     -  Final reference system
*           DRAG2     -  Dragging of velocities in the final
*                        reference system (see below)
*           MJD2      -  Modified Julian Day (ET) of the final
*                        reference system (integer part)
*           SEC2      -  Modified Julian Day (ET) of the final
*                        reference system (seconds within the day)
*
* OUTPUT:   P2        -  Position vector in RSYS2
*           V2        -  Velocity vector in RSYS2
*
* "Initial" and "final" reference systems are specified through
* the string (CHARACTER*4) variables RSYS1 and RSYS2, with the
* following conventions:
*    'ECLM'  =  mean ecliptic and equinox
*    'MEAN'  =  mean equator and equinox
*    'TRUE'  =  true equator and equinox
*    'PBF '  =  pseudo body-fixed (without pole motion and with uniform
*               rotation rate)
*    'BF  '  =  body-fixed (with pole motion and UT1 from IERS file)
*
* The flags DRAG1 and DRAG2 indicate whether the velocities in the
* corresponding reference system are computed with (DRAGn=.true.)
* or without (DRAGn=.false.) dragging, namely whether they are the real
* velocities with respect to the considered frame or barely the inertial
* velocities rotated in the new frame.
*
* WARNING: if the final reference system is BF or PBF and the
* transformation is performed with the aim of obtaining the initial
* conditions for integrating the equations of motions in those systems,
* the time of the reference system must always coincide with the time
* at which position and velocity vectors are given.
*
      SUBROUTINE rotpv(rsys1,drag1,mjd1,sec1,p1,v1,
     +                 rsys2,drag2,mjd2,sec2,p2,v2)
      IMPLICIT NONE

      INTEGER mjd1,mjd2
      DOUBLE PRECISION sec1,sec2,p1(3),v1(3),p2(3),v2(3)
      CHARACTER*4 rsys1,rsys2
      LOGICAL drag1,drag2

* Time of J2000 in MJD (ET) (integer number of days + sec)
      INTEGER mj2000
      PARAMETER (mj2000=51544)
      DOUBLE PRECISION s2000
      PARAMETER (s2000=43200.D0)

      DOUBLE PRECISION rot(3,3),rot1(3,3),rot2(3,3),piner(3),viner(3)
      DOUBLE PRECISION vtr(3)

* SUBROUTINE ROTSYS rotates the axes, not the vectors; it gives
* the derivatives of the rotation matrix with respect to the time of
* the final frame
*
* STEP 1: from RSYS1 to J2000; computation of position and velocity
* vectors in an intermediate inertial frame (Xin, Vin)
* If the velocity vector in the initial reference frame V1 includes
* dragging, then the transformation from the coordinates expressed
* in the J2000 reference frame (Xin, Vin) to (X1, V1) is given by
*
*  X1 = R1 Xin
*  V1 = dR1/dt Xin + R1 Vin
*
* where R1 is the rotation matrix which performs the transformation
* J2000 -> RSYS1 ;the inverse transformation is therefore given by
*
*  Xin = R1' X1
*  Vin = R1' V1 - R1' dR1/dt Xin
*
* (where M' indicates the transpose of marix M)
      IF(drag1) THEN
          CALL rotsys('MEAN',mj2000,s2000,rsys1,mjd1,sec1,rot,rot1,
     +                       rot2,1)
          CALL trsp3(rot)
          CALL prodmv(piner,rot,p1)
          CALL prodmv(viner,rot,v1)
          CALL pdmat(rot,rot1)
          CALL prodmv(vtr,rot1,piner)
          CALL prodvs(vtr,-1.d0,vtr)
          CALL sumv(viner,vtr,viner)
      ELSE
* If the velocity in the initial reference frame does not include
* dragging, the transformation equations are simply
*
*  X1 = R1 Xin
*  V1 = R1 Vin
*
* and
*
*  Xin = R1' X1
*  Vin = R1' V1
          CALL rotsys('MEAN',mj2000,s2000,rsys1,mjd1,sec1,rot,rot1,
     +                       rot2,0)
          CALL trsp3(rot)
          CALL prodmv(piner,rot,p1)
          CALL prodmv(viner,rot,v1)
      END IF

* STEP 2: from J2000 to RSYS2; computation of position and velocity
* vectors in the final reference frame (X2, V2) from the intermediate
* inertial vectors (Xin, Vin)
*
* If the final reference frame includes dragging for the velocity
* vector, the transformation is given by
*
*  X2 = R2 Xin
*  V2 = dR2/dt Xin + R2 Vin
*
* where R2 is the rotation matrix which performs the transformation
* J2000 -> RSYS2

      IF(drag2) THEN
          CALL rotsys('MEAN',mj2000,s2000,rsys2,mjd2,sec2,rot,rot1,
     +                       rot2,1)
          CALL prodmv(p2,rot,piner)
          CALL prodmv(v2,rot,viner)
          CALL prodmv(vtr,rot1,piner)
          CALL sumv(v2,vtr,v2)
      ELSE
* otherwise, it is simply
*
*  X2 = R2 Xin
*  V2 = R2 Vin
          CALL rotsys('MEAN',mj2000,s2000,rsys2,mjd2,sec2,rot,rot1,
     +                       rot2,0)
          CALL prodmv(p2,rot,piner)
          CALL prodmv(v2,rot,viner)
      END IF

      END
