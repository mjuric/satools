* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 8, 1997
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
      SUBROUTINE pvobs2(t,idsta,dx,dv)
      IMPLICIT NONE

      DOUBLE PRECISION dxbf(3)
      DOUBLE PRECISION dx(3),dvbf(3),dv(3)
c station identifiers
      INTEGER idsta
c times
      INTEGER mjd1,mjdjpl
      DOUBLE PRECISION t,sec1,secjpl
c indexes
      INTEGER i
***************************************
c station name
      CHARACTER*20 name
***************************************
* Station name 
      CALL obscoo(idsta,dxbf,name)
      DO 1 i=1,3
      dvbf(i)=0.d0
 1    CONTINUE
c date of J2000
      mjdjpl = 51544
      secjpl = 43200.d0 
* ET decomposed in days plus seconds; no trick (every day is 86400 sec)
      mjd1=t
      sec1=(t-mjd1)*86400
c rotation of coordinates, with dragging of velocities
      CALL rotpv('BF  ',.true.,mjd1,sec1,dxbf,dvbf,
     +                 'ECLM',.true.,mjdjpl,secjpl,dx,dv)
* Transformation of velocity from AU/s to AU/d
      DO 2 i=1,3
      dv(i)=dv(i)*86400
 2    CONTINUE

      END
