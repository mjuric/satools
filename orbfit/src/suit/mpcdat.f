* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 10, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         M P C D A T                           *
*  *                                                               *
*  *         Computes MJD from MPC-style packed dates              *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    PDATE     -  MPC-style packed date
*
* OUTPUT:   TJM       -  Modified Julian Date (TDT)
*           ERROR     -  Error flag (cannot understand input)
*
      SUBROUTINE mpcdat(pdate,tjm,error)
      IMPLICIT NONE

      CHARACTER*(*) pdate
      DOUBLE PRECISION tjm
      LOGICAL error

      INTEGER year,yy,month,day

      INTEGER lench
      LOGICAL isnum
      DOUBLE PRECISION tjm1
      EXTERNAL lench,isnum,tjm1

      error=.true.
      tjm=0.d0
      IF(lench(pdate).NE.5) RETURN

* Year
      IF(pdate(1:1).EQ.'I') THEN
          year=1800
      ELSEIF(pdate(1:1).EQ.'J') THEN
          year=1900
      ELSEIF(pdate(1:1).EQ.'K') THEN
          year=2000
      ELSE
          RETURN
      END IF
      READ(pdate(2:3),100,ERR=10) yy
 100  FORMAT(I2)
      year=year+yy

* Month
      IF(isnum(pdate(4:4))) THEN
          READ(pdate(4:4),101,ERR=10) month
      ELSEIF(pdate(4:4).EQ.'A') THEN
          month=10
      ELSEIF(pdate(4:4).EQ.'B') THEN
          month=11
      ELSEIF(pdate(4:4).EQ.'C') THEN
          month=12
      ELSE
          RETURN
      END IF
 101  FORMAT(I1)

* Day
      IF(isnum(pdate(5:5))) THEN
          READ(pdate(5:5),101,ERR=10) day
      ELSE
          day=ichar(pdate(5:5))-55
          IF(day.LT.10 .OR. day.GT.31) GOTO 10
      END IF
      tjm=tjm1(day,month,year,0.d0)
      error=.false.

 10   CONTINUE

      END
