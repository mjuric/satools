* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 9, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R O T S Y S                           *
*  *                                                               *
*  *          Rotation matrix for the transformation               *
*  *            between different reference systems                *
*  *                                                               *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    RSYS1     -  Initial reference system
*           MJDE1     -  Date of the initial reference system t1
*                        (MJD, ET, integer part)
*           SECE1     -  Date of the initial reference system t1
*                        (MJD, ET, seconds within the day)
*           RSYS2     -  Final reference system
*           MJDE2     -  Date of the final reference system t2
*                        (MJD, ET, integer part)
*           SECE2     -  Date of the final reference system t2
*                        (MJD, ET, seconds within the day)
*           NDER      -  Required order for derivatives (0/1/2)
*
* OUTPUT:   ROT       -  Rotation matrix R
*           ROT1      -  First time derivative dR/dt2 (s^(-1))
*                        with respect to t2 and to an inertial reference
*                        frame (only if NDER >= 1)
*           ROT2      -  Second time derivative d^2R/dt2^2 (s^(-2))
*                        with respect to t2 and to an inertial reference
*                        frame(only if NDER = 2)
*
* The rotation matrix R performs the transformation from the cartesian
* coordinates X1 referred to the "initial" reference system to the
* cartesian coordinates X2 referred to the "final" reference system
*
*  X2 = R X1
*
* The matrix R is computed as the product of two rotation matrices
* R = R2(t2) R1(t1), where R1(t1) performs the transformation from
* the "initial" reference system to J2000, and R2(t2) from J2000 to
* the "final" reference system. The time derivatives of R are defined as
*
*  dR/dt2     = R1(t1) dR2/dt2
*  d^2R/dt2^2 = R1(t1) d^2R2/dt2^2
*
* Due to this definition, the derivatives of the rotation matrix can
* be used directly in dynamic equations.
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
      SUBROUTINE rotsys(rsys1,mjde1,sece1,rsys2,mjde2,sece2,
     +                  rot,rot1,rot2,nder)
      IMPLICIT NONE

      INTEGER mjde1,mjde2,nder
      DOUBLE PRECISION sece1,sece2,rot(3,3),rot1(3,3),rot2(3,3)
      CHARACTER*4 rsys1,rsys2

* Time tollerance
      DOUBLE PRECISION eps
      PARAMETER (eps=1.D-6)

      INTEGER mjd1,mjd2,mjd,i,j
      DOUBLE PRECISION sec1,sec2,sec,t1,t2,t,dt,obl,obl1,obl2,gmst,gmst1
      DOUBLE PRECISION eqeq,eqeq1,eqeq2,gast,gast1,gast2
      DOUBLE PRECISION r(3,3),r1(3,3),r2(3,3)
      CHARACTER*4 rsys
      LOGICAL gomean

* Check of input parameters
      IF(nder.LT.0 .OR. nder.GT.2) STOP '**** rotsys: nder = ? ****'
      IF(rsys1.NE.'ECLM'.AND.rsys1.ne.'MEAN'.and.rsys1.ne.'TRUE'
     +                  .AND.rsys1.NE.'PBF '.and.rsys1.ne.'BF  ')
     +                   STOP '**** rotsys: unknown RSYS1 ****'
      IF(rsys2.NE.'ECLM'.AND.rsys2.ne.'MEAN'.and.rsys2.ne.'TRUE'
     +                  .AND.rsys2.NE.'PBF '.and.rsys2.ne.'BF  ')
     +                   STOP '**** rotsys: unknown RSYS2 ****'

* Time normalization
      mjd1=mjde1
      sec1=sece1
      CALL timnf(mjd1,sec1,'ET ')
      mjd2=mjde2
      sec2=sece2
      CALL timnf(mjd2,sec2,'ET ')

* GENERAL CONCEPT: the complete rotation matrix R is built up by chain
* multiplication of single rotation matrices which perform the total
* transformation step by step; the route from the initial to the final
* reference frames is computed by following the graph:
*
*               PBF            PBF
*                |              |
*      BF  ---  TRUE           TRUE  ---  BF
*                |              |
*               MEAN   -----   MEAN
*                |              |
*               ECLM           ECLM
*
*              (t=t1)         (t=t2)
*
* where the transformation between reference system which are adjacent
* in the graph is standard (and is given by separate routines). As an
* example, the transformation between BF(t1) and PBF(t2) is
* accomplished with the following intermediate passages:
*
* BF(t1) -> TRUE(t1) -> MEAN(t1) -> MEAN(t_2) -> TRUE(t_2) -> PBF(t_2)
*
* however, if t2=t1 and no derivatives are required, the following
* route is sufficient:
*
* BF(t1) -> TRUE(t1) -> PBF(t1)
*
* the computation of the derivatives of the rotation matrix always
* requires the passage through the inertial (MEAN) reference frame,
* in order to be able to compute dR2/dt2.

      t1=mjd1+sec1/86400.d0
      t2=mjd2+sec2/86400.d0

* The total rotation matrix from RSYS1 to RSYS2 is built up by going
* along the trasformation tree step by step and multiplying the rotation
* matrix previously obtained by the matrix corresponding to the new
* step. The variables RSYS1, MJD, SEC and T keep memory of the point
* reached so far. At the beginning, this is coincident with the initial
* reference system

      rsys=rsys1
      mjd=mjd1
      sec=sec1
      t=t1

* The initial value of the total rotation matrix R is the unit matrix;
* all its derivatives are zero
      DO 1 i=1,3
      DO 1 j=1,3
      rot1(i,j)=0.D0
 1    CONTINUE
      CALL assmat(rot2,rot1)
      CALL assmat(rot,rot1)
      DO 2 i=1,3
      rot(i,i)=1.d0
 2    CONTINUE
      dt=(mjd-mjd2)*86400.D0+sec-sec2

* The flag GOMEAN indicates whether the direction of the trasformation
* is approaching J2000 mean reference system from RSYS1 (phase 1),
* or going away from J2000 towards RSYS2 (phase 2).
* Passage through J2000 is necessary when:
* a) t1 is different from t2 and the application of the
*    precession matrix is required;
* b) the derivatives of the rotation matrix are required (theese
*    must be always computed from the mean, inertial reference system)

      gomean=(ABS(dt).GT.eps .OR. nder.GE.1)
 3    CONTINUE

* PHASE 1: path from RSYS1 to J2000. In this phase the time derivatives
* of the rotation matrix are NOT updated at each step, since the total
* derivative is computed with respect to J2000

      IF(gomean) THEN
          IF(rsys.EQ.'ECLM') THEN
* Trasformation ECLM -> MEAN: rotation around x1 axis by the negative
* value of the mean obliquity of the ecliptic
              CALL obleqd(t,obl,obl1,obl2,0)
              CALL rotmt(-obl,r,1)
              CALL pdmat(r,rot)
              rsys='MEAN'
          ELSEIF(rsys.EQ.'TRUE') THEN
* Trasformation TRUE -> MEAN: transpose of the nutation matrix
              CALL rnutd(t,r,r1,r2,0)
              CALL trsp3(r)
              CALL pdmat(r,rot)
              rsys='MEAN'
          ELSEIF(rsys.EQ.'PBF ') THEN
* Trasformation PBF -> TRUE: rotation around x3 axis by the negative
* value of the nominal Greenwich Apparent Sidereal Time
              CALL gmsnom(mjd,sec,gmst,gmst1)
              CALL equeqd(t,eqeq,eqeq1,eqeq2,0)
              gast=gmst+eqeq
              CALL rotmt(-gast,r,3)
              CALL pdmat(r,rot)
              rsys='TRUE'
          ELSEIF(rsys.EQ.'BF  ') THEN
* Trasformation BF -> TRUE: transpose of the diurnal rotation matrix
              CALL diurot(mjd,sec,r,r1,r2,0)
              CALL trsp3(r)
              CALL pdmat(r,rot)
              rsys='TRUE'
          ELSEIF(rsys.EQ.'MEAN') THEN
* Trasformation MEAN(t1) -> MEAN(t2): precession matrix
              CALL precd(t,r,r1,r2,0)
              CALL trsp3(r)
              CALL pdmat(r,rot)
* In this point J2000 reference frame has been reached; here starts the
* computation of the derivatives of the rotation matrix
              CALL precd(t2,r,r1,r2,nder)
              IF(nder.GE.2) CALL pd2mat(r,r1,r2,rot,rot1,rot2)
              IF(nder.GE.1) CALL pd1mat(r,r1,rot,rot1)
              CALL pdmat(r,rot)
              mjd=mjd2
              sec=sec2
              t=t2
              gomean=.false.
          ELSE
              STOP '**** rotsys: internal error (01) ****'
          END IF

* PHASE 2: path from J2000 to RSYS2. In this phase the time derivatives
* of the rotation matrix are updated at each step
      ELSEIF(rsys.NE.rsys2) THEN
          IF(rsys.EQ.'TRUE') THEN
              IF(rsys2.EQ.'MEAN'.OR.rsys2.eq.'ECLM') THEN
* Trasformation TRUE -> MEAN: transpose of the nutation matrix
                  CALL rnutd(t,r,r1,r2,nder)
                  CALL trsp3(r)
                  IF(nder.GE.1) CALL trsp3(r1)
                  IF(nder.GE.2) CALL trsp3(r2)
                  IF(nder.GE.2) CALL pd2mat(r,r1,r2,rot,rot1,rot2)
                  IF(nder.GE.1) CALL pd1mat(r,r1,rot,rot1)
                  CALL pdmat(r,rot)
                  rsys='MEAN'
              ELSEIF(rsys2.EQ.'PBF ') THEN
* Trasformation TRUE -> PBF: rotation around x3 axis by the nominal
* Greenwich Apparent Sidereal Time
                  CALL gmsnom(mjd,sec,gmst,gmst1)
                  CALL equeqd(t,eqeq,eqeq1,eqeq2,nder)
                  gast=gmst+eqeq
                  CALL rotmt(gast,r,3)
                  IF(nder.GE.1) THEN
                      gast1=gmst1+eqeq1
                      CALL rotmt1(gast,r1,3,gast1)
                  END IF
                  IF(nder.GE.2) THEN
                      gast2=eqeq2
                      CALL rotmt2(gast,r2,3,gast1,gast2)
                  END IF
                  IF(nder.GE.2) CALL pd2mat(r,r1,r2,rot,rot1,rot2)
                  IF(nder.GE.1) CALL pd1mat(r,r1,rot,rot1)
                  CALL pdmat(r,rot)
                  rsys='PBF '
              ELSEIF(rsys2.EQ.'BF  ') THEN
* Trasformation TRUE -> BF: diurnal rotation matrix
                  CALL diurot(mjd,sec,r,r1,r2,nder)
                  IF(nder.GE.2) CALL pd2mat(r,r1,r2,rot,rot1,rot2)
                  IF(nder.GE.1) CALL pd1mat(r,r1,rot,rot1)
                  CALL pdmat(r,rot)
                  rsys='BF  '
              ELSE
                  STOP ' **** rotsys: internal error (02) ****'
              END IF
          ELSEIF(rsys.EQ.'MEAN') THEN
              IF(rsys2.EQ.'TRUE'.OR.rsys2.eq.'PBF '
     +                          .OR.rsys2.EQ.'BF  ') THEN
* Trasformation MEAN -> TRUE: nutation matrix
                  CALL rnutd(t,r,r1,r2,nder)
                  IF(nder.GE.2) CALL pd2mat(r,r1,r2,rot,rot1,rot2)
                  IF(nder.GE.1) CALL pd1mat(r,r1,rot,rot1)
                  CALL pdmat(r,rot)
                  rsys='TRUE'
              ELSEIF(rsys2.EQ.'ECLM') THEN
* Trasformation MEAN -> ECLM: rotation around x1 axis by the mean
* obliquity of the ecliptic
                  CALL obleqd(t,obl,obl1,obl2,nder)
                  CALL rotmt(obl,r,1)
                  IF(nder.GE.1) CALL rotmt1(obl,r1,1,obl1)
                  IF(nder.GE.2) CALL rotmt2(obl,r2,1,obl1,obl2)
                  IF(nder.GE.2) CALL pd2mat(r,r1,r2,rot,rot1,rot2)
                  IF(nder.GE.1) CALL pd1mat(r,r1,rot,rot1)
                  CALL pdmat(r,rot)
                  rsys='ECLM'
              ELSE
                  STOP' **** rotsys: internal error (03) ****'
              END IF
          ELSEIF(rsys.EQ.'ECLM') THEN
* Trasformation ECLM -> MEAN: rotation around x1 axis by the negative
* value of the mean obliquity of the ecliptic
              CALL obleqd(t,obl,obl1,obl2,nder)
              CALL rotmt(-obl,r,1)
              IF(nder.GE.1) CALL rotmt1(-obl,r1,1,-obl1)
              IF(nder.GE.2) CALL rotmt2(-obl,r2,1,-obl1,-obl2)
              IF(nder.GE.2) CALL pd2mat(r,r1,r2,rot,rot1,rot2)
              IF(nder.GE.1) CALL pd1mat(r,r1,rot,rot1)
              CALL pdmat(r,rot)
              rsys='MEAN'
          ELSEIF(rsys.EQ.'PBF ') THEN
* Trasformation PBF -> TRUE: rotation around x3 axis by the negative
* value of the nominal Greenwich Apparent Sidereal Time
              CALL gmsnom(mjd,sec,gmst,gmst1)
              CALL equeqd(t,eqeq,eqeq1,eqeq2,nder)
              gast=gmst+eqeq
              CALL rotmt(-gast,r,3)
              IF(nder.GE.1) THEN
                  gast1=gmst1+eqeq1
                  CALL rotmt1(-gast,r1,3,-gast1)
              END IF
              IF(nder.GE.2) THEN
                  gast2=eqeq2
                  CALL rotmt2(-gast,r2,3,-gast1,-gast2)
              END IF
              IF(nder.GE.2) CALL pd2mat(r,r1,r2,rot,rot1,rot2)
              IF(nder.GE.1) CALL pd1mat(r,r1,rot,rot1)
              CALL pdmat(r,rot)
              rsys='TRUE'
          ELSEIF(rsys.EQ.'BF  ') THEN
* Trasformation BF -> TRUE: transpose of the diurnal rotation matrix
              CALL diurot(mjd,sec,r,r1,r2,nder)
              CALL trsp3(r)
              IF(nder.GE.1) CALL trsp3(r1)
              IF(nder.GE.2) CALL trsp3(r2)
              IF(nder.GE.2) CALL pd2mat(r,r1,r2,rot,rot1,rot2)
              IF(nder.GE.1) CALL pd1mat(r,r1,rot,rot1)
              CALL pdmat(r,rot)
              rsys='TRUE'
          ELSE
              STOP' **** rotsys: internal error (04) ****'
          END IF
      ELSE
          RETURN
      END IF
      GOTO 3

      END
