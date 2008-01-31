* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 12, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         S R T E P T                           *
*  *                                                               *
*  *                 Sorting of ephemeris epochs                   *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    T         -  Ephemeris epochs
*           N         -  Number of ephemeris epochs
*           T0        -  Epoch of initial conditions
*
* OUTPUT:   IPT       -  Sorting pointer
*
      SUBROUTINE srtept(t,n,t0,ipt)
      IMPLICIT NONE

      INTEGER n,ipt(n)
      DOUBLE PRECISION t(n),t0

      INTEGER i,i1,i2,k1,k2,nit
      LOGICAL ge1,ge2,rev,change

* Initial guess for IPT (assuming ephemeris times to be sorted
* in ascending order)
      i1=n+1
      DO 1 i=1,n
      IF(t(i).GE.t0) THEN
          i1=i
          GOTO 2
      END IF
 1    CONTINUE
 2    CONTINUE

* I1 is now the number of the first t(i)>=t0
      i2=0
      DO 3 i=i1,n
      i2=i2+1
      ipt(i2)=i
 3    CONTINUE
      DO 4 i=i1-1,1,-1
      i2=i2+1
      ipt(i2)=i
 4    CONTINUE
      IF(i2.NE.n) STOP '**** srtept: internal error (01) ****'

* Sorting
      nit=0
 5    CONTINUE
      nit=nit+1
      IF(nit.GT.n+3) STOP '**** srtept: internal error (02) ****'
      change=.false.
      DO 6 k1=1,n-1
      k2=k1+1
      i1=ipt(k1)
      i2=ipt(k2)
      ge1=(t(i1).GE.t0)
      ge2=(t(i2).GE.t0)
      IF(ge1) THEN
          IF(ge2) THEN
              rev=(t(i1).GT.t(i2))
          ELSE
              rev=.false.
          END IF
      ELSE
          IF(ge2) THEN
              rev=.true.
          ELSE
              rev=(t(i1).LT.t(i2))
          END IF
      END IF
      IF(rev) THEN
          i=ipt(k1)
          ipt(k1)=ipt(k2)
          ipt(k2)=i
          change=.true.
      END IF
 6    CONTINUE
      IF(change) GOTO 5

      END
