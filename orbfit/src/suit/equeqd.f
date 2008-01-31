* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 8, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          G M S T D                            *
*  *                                                               *
*  *       Equation of the equinoxes (with time derivatives)       *
*  *                                                               *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TJME      -  Modified Julian Time (ET)
*           NDER      -  Required order for derivatives (0/1/2)
*
* OUTPUT:   EQEQ      -  Equation of the equinoxes (difference
*                        between apparent sidereal time and mean
*                        sidereal time) in radians
*           EQEQ1     -  dEQEQ/dt (rad/s) (only if NDER >= 1)
*           EQEQ2     -  d^2EQEQ/dt^2 (rad/s^2) (only if NDER = 2)
*
      SUBROUTINE equeqd(tjme,eqeq,eqeq1,eqeq2,nder)
      IMPLICIT NONE

      INTEGER nder
      DOUBLE PRECISION tjme,eqeq,eqeq1,eqeq2

      DOUBLE PRECISION obl,obl1,obl2,dpsi,deps,dpsi1,deps1,dpsi2,deps2
      DOUBLE PRECISION cose,sine

      IF(nder.LT.0.OR.nder.GT.2) STOP '**** equeqd: nder = ? ****'
      CALL obleqd(tjme,obl,obl1,obl2,nder)
      CALL nutnd(tjme,dpsi,deps,dpsi1,deps1,dpsi2,deps2,nder)

      cose=COS(obl)
      eqeq=dpsi*cose
      IF(nder.GE.1) THEN
          sine=SIN(obl)
          eqeq1=dpsi1*cose-dpsi*sine*obl1
      END IF

      IF(nder.GE.2) THEN
          eqeq2=dpsi2*cose-2.d0*dpsi1*sine*obl1
     .                    -dpsi1*cose*obl1**2-dpsi*sine*obl2
      END IF

      END
