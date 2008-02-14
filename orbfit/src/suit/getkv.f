* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: September 19, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          G E T K V                            *
*  *                                                               *
*  *          Locates a key/value pair in the input namelist       *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    KEY       -  Required key
*
* OUTPUT:   VAL       -  Value field associated with requested key
*           KTYP      -  Keyword type:
*                              1 - integer
*                              2 - real
*                              3 - character string
*                              4 - logical
*                              5 - MJD
*           FILE      -  Name of the file where the key was read
*           KR        -  Record num. in the file where the key was read
*           FOUND     -  If .false., the key is not defined

      SUBROUTINE getkv(key,val,ktyp,file,kr,found)
      IMPLICIT NONE

      INCLUDE 'parnam.h'

* NEEDED common blocks:
      INCLUDE 'comnam.h'

      CHARACTER*(*) key,val,file
      INTEGER ktyp,kr,lv,lf,lk,i
      LOGICAL found

      INTEGER lench
      EXTERNAL lench

      IF(iicnam.NE.36) STOP '**** getkv: internal error (01) ****'

      lv=LEN(val)
      lf=LEN(file)
      CALL chkpdf(lv,lcvx,'lcvx')
      CALL chkpdf(lf,lcfx,'lcfx')

      lk=lench(key)
      DO 1 i=1,nne
      IF(key(1:lk).EQ.keys(i)) THEN
          val=vals(i)
          file=namif(i)
          kr=krecnm(i)
          kuorl=kuorl+1
          kuord(i)=kuorl
          ktyp=krtyp(i)
          IF(ktyp.LE.0.OR.ktyp.GT.5) THEN
              WRITE(0,100) key(1:lk),ktyp
              STOP '**** getkv: internal error (02) ****'
          END IF
          found=.true.
          RETURN
      END IF
 1    CONTINUE
 100  FORMAT(' ERROR: illegal keyword type'/
     +       '        KEY="',A,'"'/
     +       '        TYPE=',I2)

      found=.false.

      END
