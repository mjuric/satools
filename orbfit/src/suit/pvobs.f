* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 4, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          P V O B S                            *
*  *                                                               *
*  *      Position of the observer with respect to the center      *
*  *         of mass of the Earth (mean ecliptic J2000)            *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    T         -  Time (MJD, TDT)
*           IDSTA     -  Observatory code
*
* OUTPUT:   DX,DV     -  Position and velocity of the observer w.r.
*                        to the center of the Earth (mean ecl. J2000)
*
      SUBROUTINE pvobs(t,idsta,dx,dv)
      IMPLICIT NONE

      DOUBLE PRECISION rot(3,3),dxbf(3),dxtod(3)
      DOUBLE PRECISION dx(3),omega(3),dvbf(3),dvtod(3),dv(3)
c station identifiers
      INTEGER idsta
c times
      INTEGER mjd1,mjd2
      DOUBLE PRECISION t,sec1,sec2,tut,gast
c functions
      DOUBLE PRECISION gmst,equequ
      EXTERNAL gmst,equequ
***************************************
c startup
      CHARACTER*20 name
      LOGICAL first

      INCLUDE 'trig.h'

c static memory allocation only for:
      SAVE first,omega
      DATA first/.true./

      IF(first) THEN
          first=.false.
* Earth angular velocity (rad/d)
          omega(1)=0.d0
          omega(2)=0.d0
          omega(3)=dpig*1.00273790934d0
      END IF
***************************************
* Station name 
      CALL obscoo(idsta,dxbf,name)
* Station position and velocity in the body fixed (equatorial) frame
      CALL prvec(omega,dxbf,dvbf)
* ET decomposed in days plus seconds; no trick (every day is 86400 sec)
      mjd1=t
      sec1=(t-mjd1)*86400
* Computation of UT1
      CALL cnvtim(mjd1,sec1,'ET ',mjd2,sec2,'UT1')
      tut=sec2/86400+mjd2
* 
* Greenwich Apparent Sidereal Time = Greenwich Mean Sidereal Time +
* Equation of the Equinoxes
      gast=gmst(tut)+equequ(t)
* Diurnal rotation matrix (transformation from body-fixed to
* true-of-date frames), neglecting polar motion
      CALL rotmt(-gast,rot,3)
* 
      CALL prodmv(dxtod,rot,dxbf)
      CALL prodmv(dvtod,rot,dvbf)
* Station position and velocity in the J2000 (ecliptic) frame
      CALL rotpn(rot,'EQUT','OFDATE',t,'ECLM','J2000',0.d0)
      CALL prodmv(dx,rot,dxtod)
      CALL prodmv(dv,rot,dvtod)

      END
