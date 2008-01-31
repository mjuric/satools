* Copyright (C) 1997-2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: May 31, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         P O S O B S                           *
*  *                                                               *
*  *   Heliocentric position of the observer (mean equator J2000)  *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TDT       -  Time (MJD, TDT)
*           OBSCOD    -  Observatory code
*           N         -  Number of observations
*
* OUTPUT:   X         -  Heliocentric position of the observer
*
      SUBROUTINE posobs(tdt,obscod,n,x)
      IMPLICIT NONE

      INTEGER n,obscod(n)
      DOUBLE PRECISION tdt(n),x(3,n)

      INTEGER i,k
      DOUBLE PRECISION et2(2),r6(6),rot(3,3),dxe(3),dve(3),dx(3)
      LOGICAL first
      DATA first/.true./

      SAVE first,rot

      IF(first) THEN
          CALL rotpn(rot,'ECLM','J2000',0.d0,'EQUM','J2000',0.d0)
          first=.false.
      END IF

      et2(1)=2400000.5d0

      DO 1 i=1,n
      et2(2)=tdt(i)
      CALL dpleph(et2,3,11,r6,1)
      CALL pvobs(tdt(i),obscod(i),dxe,dve)
      CALL prodmv(dx,rot,dxe)
      DO 2 k=1,3
      x(k,i)=r6(k)+dx(k)
 2    CONTINUE
 1    CONTINUE

      END
