* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 31, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         S P L K V C                           *
*  *                                                               *
*  *     Split a namelist record into keyword+value+comment        *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    REC       -  Record
*
* OUTPUT:   KEY       -  Keyword field
*           VAL       -  Value field
*           COMM      -  Comment field
*           SKIP      -  "Only comment" flag
*           END       -  "End of namelist" flag
*
      SUBROUTINE splkvc(rec,key,val,comm,skip,end)
      IMPLICIT NONE

      INCLUDE 'parcmc.h'

      CHARACTER*(*) rec,key,val,comm
      LOGICAL skip,end

      CHARACTER*200 rec1,tmp
      INTEGER lr,lk,lv,ipc,ipu

      INTEGER lench
      EXTERNAL lench

      key=' '
      val=' '
      comm=' '

* Detection of the end of the namelist
      rec1=rec
      CALL rmsp(rec1,lr)
      IF(rec1(1:lr).EQ.'END_OF_HEADER') THEN
          end=.true.
          skip=.false.
          RETURN
      ELSE
          end=.false.
      END IF

* Compute length excluding comments
      lr=lench(rec)
      IF(lr.LT.1) THEN
          skip=.true.
          RETURN
      END IF
      ipc=INDEX(rec(1:lr),comcha)
      IF(ipc.EQ.1) THEN
          skip=.true.
          comm=rec(2:)
          RETURN
      END IF

* Separate comment from keyword+value
      IF(ipc.EQ.0) THEN
          rec1=rec(1:lr)
      ELSE
          rec1=rec(1:ipc-1)
          comm=rec(ipc+1:)
          lr=lench(rec1)
          IF(lr.LT.1) THEN
              skip=.true.
              RETURN
          END IF
      END IF
      skip=.false.

* Keyword field
      ipu=INDEX(rec1(1:lr),'=')
      IF(ipu.EQ.0) THEN
          CALL rmsp(rec1,lk)
          IF(lk.GT.LEN(key)) STOP '**** splkvc: lk > LEN(key) ****'
          key=rec1(1:lk)
          RETURN
      END IF
      tmp=rec1(1:ipu-1)
      CALL rmsp(tmp,lk)
      IF(lk.GT.LEN(key)) STOP '**** splkvc: lk > LEN(key) ****'
      key=tmp(1:lk)

* Value field
      tmp=rec1(ipu+1:)
      CALL norstr(tmp,lv)
      IF(lv.GT.LEN(val)) STOP '**** splkvc: lv > LEN(val) ****'
      val=tmp(1:lv)

      END
