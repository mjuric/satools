* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 10, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R O T M T 1                           *
*  *                                                               *
*  *           First time derivative of a rotation matrix          *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    A         -  Rotation angle alpha (rad)
*           K         -  (=1,2,3) Rotation axis (1=x, 2=y, 3=z)
*           ADOT      -  First time derivative of the rotation angle
*                        dalpha/dt (rad/arbitrary time unit)
*
* OUTPUT:   R         -  First time derivative of the rotation matrix
*                        dR_k/dt (rad/arbitrary time unit)
*
* The time derivative of the rotation matrix R_k(alpha) (for its
* definition, see SUBROUTINE ROTMT) is computed according to the formula
* 
* dR_k/dt = dR_k/dalpha * dalpha/dt
* 
      SUBROUTINE rotmt1(a,r,k,adot)
      IMPLICIT NONE

      INTEGER k
      DOUBLE PRECISION a,r(3,3),adot

      INTEGER i1,i2,i3
      DOUBLE PRECISION cosa,sina

      IF(k.LT.1 .OR. k.GT.3) STOP '**** rotmt1: k = ??? ****'

      cosa=COS(a)
      sina=SIN(a)
      i1=k
      IF(i1.GT.3) i1=i1-3
      i2=i1+1
      IF(i2.GT.3) i2=i2-3
      i3=i2+1
      IF(i3.GT.3) i3=i3-3

      r(i1,i1) =  0.d0
      r(i1,i2) =  0.d0
      r(i1,i3) =  0.d0
      r(i2,i1) =  0.d0
      r(i2,i2) = -sina*adot
      r(i2,i3) =  cosa*adot
      r(i3,i1) =  0.d0
      r(i3,i2) = -cosa*adot
      r(i3,i3) = -sina*adot

      END
