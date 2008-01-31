* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 8, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D F I N T                           *
*  *                                                               *
*  *                  Read an integer quantity                     *
*  *           from simplified file-header namelist                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Input unit
*           KEY       -  Keyword
*           REQRD     -  If true, when the keyword is not found in the
*                        namelist, execution stops with an error message
*
* OUTPUT:   K         -  Integer value
*           FOUND     -  True when the keyword has been found in the
*                        namelist
*           KR        -  Record number in the input file
*
      SUBROUTINE rdfint(unit,key,reqrd,k,found,kr)
      IMPLICIT NONE

      INCLUDE 'parnam.h'

* NEEDED common blocks:
      INCLUDE 'comfnm.h'

      CHARACTER*(*) key
      CHARACTER vartyp*30
      LOGICAL reqrd,found
      INTEGER k,kr,i,lk,lz,lf,unit


      INTEGER lench
      EXTERNAL lench

      IF(iicfnm.NE.36) STOP '**** rdfint: internal error (01) ****'
      IF(unit.NE.hnfuni) STOP '**** rdfint: internal error (02) ****'

      vartyp='INTEGER'

      kr=0
      DO 1 i=1,nfne
      IF(key.EQ.keysf(i)) THEN
          found=.true.
          READ(valsf(i),*,ERR=10) k
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
          STOP '**** rdfint: abnormal end ****'
      END IF
 100  FORMAT(' ERROR: missing definition of keyword "',a,'" (type: ',
     +       a,')'/8x,'in file "',a,'"')

      RETURN

* Error in reading namelist value
 10   CONTINUE
      lk=lench(key)
      lz=lench(vartyp)
      lf=lench(nmif)
      WRITE(*,101) key(1:lk),vartyp(1:lz),krcfnm(i),nmif(1:lf)
 101  FORMAT(' ERROR in reading value for keyword "',a,'" (type: ',
     +       a,')'/7x,'at record',i4,' in file "',a,'"')
      STOP '**** rdfint: abnormal end ****'
      END
