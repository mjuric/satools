* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: September 19, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C H K K E Y                           *
*  *                                                               *
*  *                   Check keyword validity                      *
*  *    and transfer information on keyword type to namelist       *
*  *                                                               *
*  *****************************************************************
*
      SUBROUTINE chkkey
      IMPLICIT NONE

      INCLUDE 'parnam.h'

* NEEDED common blocks:
      INCLUDE 'comkls.h'
      INCLUDE 'comnam.h'

      INTEGER lk2(nklsx),k,i,lk1,ipp,lf
      CHARACTER keywp*(lckx)
      LOGICAL fail,pare

      INTEGER lench
      EXTERNAL lench

      fail=.false.
      IF(iickls.NE.36) STOP '**** chkkey: internal error (01) ****'
      IF(iicnam.NE.36) STOP '**** chkkey: internal error (02) ****'

      DO 1 k=1,nkls
 1    lk2(k)=lench(keylst(k))

      DO 3 i=1,nne
      krtyp(i)=0
      lk1=lench(keys(i))
      ipp=INDEX(keys(i)(1:lk1),'(')
      IF(ipp.NE.0) THEN
          pare=.true.
          lk1=ipp-1
      ELSE
          pare=.false.
      END IF
      DO 2 k=1,nkls
      IF(pare) THEN
          keywp=keylst(k)
          ipp=INDEX(keywp,'(')
          IF(ipp.EQ.0) GOTO 2
          keywp=keywp(1:ipp-1)
          IF(keys(i)(1:lk1).EQ.keywp) THEN
              krtyp(i)=keytyp(k)
              GOTO 3
          END IF
      ELSE
          IF(keys(i)(1:lk1).EQ.keylst(k)(1:lk2(k))) THEN
              krtyp(i)=keytyp(k)
              GOTO 3
          END IF
      END IF
 2    CONTINUE
      lf=lench(namif(i))
      write(99,100) keys(i)(1:lk1),krecnm(i),namif(i)(1:lf)
 100  FORMAT(' ERROR: unrecognized keyword "',a,'"'/
     +        8x,'(record',i4,' in file "',a,'")')
      fail=.true.
 3    CONTINUE

      IF(fail) STOP '**** chkkey: abnormal end ***'
      END
