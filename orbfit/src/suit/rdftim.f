* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 8, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D F T I M                           *
*  *                                                               *
*  *                Read a time quantity (MJD+SEC)                 *
*  *           from simplified file-header namelist                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Input unit
*           KEY       -  Keyword
*           REQRD     -  If true, when the keyword is not found in the
*                        namelist, execution stops with an error message
*
* OUTPUT:   TIMSTR    -  Time as a string
*           MJD       -  Modified Julian Date (integer part)
*           SEC       -  Seconds within the day
*           SCALE     -  Time scale
*           FOUND     -  True when the keyword has been found in the
*                        namelist
*           KR        -  Record number in the input file
*
      SUBROUTINE rdftim(unit,key,reqrd,timstr,mjd,sec,scale,found,kr)
      IMPLICIT NONE

      INCLUDE 'parnam.h'

* NEEDED common blocks:
      INCLUDE 'comfnm.h'

      CHARACTER*(*) key,timstr,scale
      CHARACTER*30 vartyp
      LOGICAL reqrd,found,error
      DOUBLE PRECISION sec
      INTEGER mjd,kr,i,lk,lz,lf,unit

      INTEGER lench
      EXTERNAL lench

      IF(iicfnm.NE.36) STOP '**** rdftim: internal error (01) ****'
      IF(unit.NE.hnfuni) STOP '**** rdftim: internal error (02) ****'

      vartyp='TIME'

      kr=0
      DO 1 i=1,nfne
      IF(key.EQ.keysf(i)) THEN
          found=.true.
          CALL ch2tim(valsf(i),mjd,sec,scale,error)
          IF(error) GOTO 10
          timstr=valsf(i)
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
          write(99,100) key(1:lk),vartyp(1:lz),nmif(1:lf)
          STOP '**** rdftim: abnormal end ****'
      END IF
 100  FORMAT(' ERROR: missing definition of keyword "',a,'" (type: ',
     +       a,')'/8x,'in file "',a,'"')

      RETURN

* Error in reading namelist value
 10   CONTINUE
      lk=lench(key)
      lz=lench(vartyp)
      lf=lench(nmif)
      write(99,101) key(1:lk),vartyp(1:lz),krcfnm(i),nmif(1:lf)
 101  FORMAT(' ERROR in reading value for keyword "',a,'" (type: ',
     +       a,')'/7x,'at record',i4,' in file "',a,'"')
      STOP '**** rdftim: abnormal end ****'
      END
