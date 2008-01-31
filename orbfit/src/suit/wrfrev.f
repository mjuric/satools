* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 21, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R F R E V                           *
*  *                                                               *
*  *                     Write a real vector                       *
*  *           to a simplified file-header namelist                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Output unit
*           KEY       -  Keyword
*           VAL       -  Value
*           N         -  Dimension
*           COMM      -  Comment
*
      SUBROUTINE wrfrev(unit,key,val,n,comm)
      IMPLICIT NONE

      INCLUDE 'parcmc.h'

* NEEDED common blocks:
      INCLUDE 'comwfr.h'

      INTEGER unit,n
      CHARACTER*(*) key,comm
      DOUBLE PRECISION val(n)

      INTEGER lk,lc,lr,lt,i
      CHARACTER rec*200,tmp*50

      INTEGER lench
      EXTERNAL lench

      IF(iicwfr.NE.36) STOP '**** wrfrev: internal error (01) ****'

      lk=lench(key)
      lc=lench(comm)

      lr=0
      rec=' '
      lt=22
      DO 1 i=1,n
      tmp=' '
      IF(lr+lt.GT.200) STOP '**** wrfrev: internal error (02) ****'
      WRITE(tmp,200) val(i)
 200  FORMAT(1P,E22.14)
      rec(lr+1:lr+lt)=tmp(1:lt)
      lr=lr+lt
 1    CONTINUE

      IF(lc.GT.0) THEN
          WRITE(unit,100) key(1:lk),rec(1:lr),comcha,comm(1:lc)
      ELSE
          WRITE(unit,101) key(1:lk),rec(1:lr)
      END IF
 100  FORMAT(A,' =',A,1X,A,1X,A)
 101  FORMAT(A,' =',A)

      END
