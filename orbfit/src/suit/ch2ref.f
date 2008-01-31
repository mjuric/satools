* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 7, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C H 2 R E F                           *
*  *                                                               *
*  *              Translation of a character string                *
*  *              into reference system description                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    STRING    -  Character string
*
* OUTPUT:   RSYS      -  Reference system type (EQUM/EQUT/ECLM)
*           EPOCH     -  Epoch specification (J2000/OFDATE)
*           ERROR     -  Error flag
*
* WARNING: if EPOCH=OFDATE, the reference system specification must be
*          completed with the date, which is derived from a different
*          source (date of orbital elements or observations)
*
      SUBROUTINE ch2ref(string,rsys,epoch,error)
      IMPLICIT NONE

      CHARACTER*(*) string,rsys,epoch
      LOGICAL error

      CHARACTER cont*20,rest*200,rec*200

      error=.false.

* First item: reference system type
      rec=string
      CALL strcnt(rec,cont,rest,error)
      IF(error) RETURN
      rsys=cont
      CALL upcase(rsys)

* Second item: epoch
      rec=rest
      CALL strcnt(rec,cont,rest,error)
      IF(error) RETURN
      epoch=cont
      CALL upcase(epoch)

      CALL chkref(rsys,epoch,error)

      END
