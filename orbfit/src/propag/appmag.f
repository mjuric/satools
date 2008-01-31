*
*  ***************************************************************
*  *                                                             *
*  *                         A P P M A G                         *
*  *                                                             *
*  *     Calcolo della magnitudine apparente di un asteroide     *
*  *                                                             *
*  ***************************************************************
*
*
* INPUT:    H         -  Magnitudine assoluta
*           G         -  Parametro di slope
*           DS        -  Distanza dal Sole (AU)
*           DT        -  Distanza dalla Terra (AU)
*           BETA      -  Angolo di fase solare (rad): angolo tra il
*                        Sole e l'osservatore (Terra), visto dall'asteroide
*
      DOUBLE PRECISION FUNCTION appmag(h,g,ds,dt,beta)
      IMPLICIT NONE

      include 'phase.h'

      DOUBLE PRECISION h,g,ds,dt,beta
      DOUBLE PRECISION a1,a2,b1,b2
      DOUBLE PRECISION tb2,phi1,phi2
      SAVE a1,a2,b1,b2

* Costanti per il calcolo della magnitudine
      DATA a1,a2,b1,b2/3.33d0,1.87d0,0.63d0,1.22d0/

      tb2=TAN(beta/2.d0)
      phi1=EXP(-a1*tb2**b1)
      phi2=EXP(-a2*tb2**b2)
      umag=5.d0*LOG10(ds*dt)+h
      appmag=umag-2.5d0*LOG10((1.d0-g)*phi1+g*phi2)

      END


