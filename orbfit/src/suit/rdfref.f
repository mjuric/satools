* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 8, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D F R E F                           *
*  *                                                               *
*  *            Read a reference system description                *
*  *           from simplified file-header namelist                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Input unit
*           KEY       -  Keyword
*           REQRD     -  If true, when the keyword is not found in the
*                        namelist, execution stops with an error message
*
* OUTPUT:   RSYS      -  Reference system type (EQUM/EQUT/ECLM)
*           EPOCH     -  Epoch specification (J2000/OFDATE)
*           FOUND     -  True when the keyword has been found in the
*                        namelist
*           KR        -  Record number in the input file
*
      SUBROUTINE rdfref(unit,key,reqrd,rsys,epoch,found,kr)
      IMPLICIT NONE

      INCLUDE 'parnam.h'

* NEEDED common blocks:
      INCLUDE 'comfnm.h'

      CHARACTER*(*) key,rsys,epoch
      LOGICAL reqrd,found
      INTEGER kr,unit

      INTEGER i,lk,lz,lf
      CHARACTER vartyp*30
      LOGICAL error

      INTEGER lench
      EXTERNAL lench

      IF(iicfnm.NE.36) STOP '**** rdfref: internal error (01) ****'
      IF(unit.NE.hnfuni) STOP '**** rdfref: internal error (02) ****'

      vartyp='REFERENCE SYSTEM'

      kr=0
      DO 1 i=1,nfne
      IF(key.EQ.keysf(i)) THEN
          found=.true.
          CALL ch2ref(valsf(i),rsys,epoch,error)
          IF(error) GOTO 10
          kr=krcfnm(i)
          kuorlf=kuorlf+1
          kuorf(i)=kuorlf
          RETURN
      END IF
 1    CONTINUE

      found=.false.
      IF(reqrd) THEN
          lk=lench(key)
          lz=lench(vartyp)
          lf=lench(nmif)
          WRITE(*,100) key(1:lk),vartyp(1:lz),nmif(1:lf)
          STOP '**** rdfref: abnormal end ****'
      END IF
 100  FORMAT(' ERROR: missing definition of keyword "',a,'" (type: ',
     +       a,')'/8x,'in file "',a,'"')

      rsys=' '
      epoch=' '

      RETURN

* Error in reading namelist value
 10   CONTINUE
      lk=lench(key)
      lz=lench(vartyp)
      lf=lench(nmif)
      WRITE(*,101) key(1:lk),vartyp(1:lz),krcfnm(i),nmif(1:lf)
 101  FORMAT(' ERROR in reading value for keyword "',a,'" (type: ',
     +       a,')'/7x,'at record',i4,' in file "',a,'"')
      STOP '**** rdfref: abnormal end ****'
      END
