* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 5, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D V C H A                           *
*  *                                                               *
*  *   Read a character vector quantity from the input namelist    *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    CAT       -  Namelist keyword category
*           KEY       -  Namelist keyword name
*           N         -  Dimension of the vector to be read
*           REQRD     -  If true, when the keyword is not found in the
*                        namelist, an error message is issued and the
*                        FAIL flag is set
*
* OUTPUT:   V         -  Value associated with keyword
*           FOUND     -  True when the keyword has been found in the
*                        namelist
*           FAIL1     -  True if(.not.found.and.reqrd); false otherwise
*           FAIL      -  Set to true if(.not.found.and.reqrd);
*                        otherwise, not changed
*
      SUBROUTINE rdvcha(cat,key,v,n,reqrd,found,fail1,fail)
      IMPLICIT NONE

      INCLUDE 'parnam.h'

      CHARACTER*(*) cat,key
      CHARACTER ck*(lckx),val*(lcvx),file*(lcfx)
      CHARACTER*50 vartyp
      LOGICAL reqrd,found,fail,fail1

      INTEGER n
      CHARACTER*(*) v(n)
      CHARACTER*(lcvx) rest
      INTEGER ktyp0,ktyp1,lc,lk,lt,kr,lz,lf,i
      LOGICAL error

      INTEGER lench
      EXTERNAL lench

      vartyp='CHARACTER VECTOR'
      ktyp0=3

      fail1=.false.
* Find in namelist the value corresponding to the requested key
      lc=lench(cat)
      lk=lench(key)
      lt=lc+lk
      CALL chkpdf(lt,lckx,'lckx')
      IF(lc.LE.0) THEN
          ck=key(1:lk)
      ELSE
          ck=cat(1:lc)//key(1:lk)
      END IF
      CALL getkv(ck(1:lt),val,ktyp1,file,kr,found)
      lk=lench(key)
      lz=lench(vartyp)
      lf=lench(file)
      IF(.NOT.found) THEN
          IF(reqrd) THEN
              WRITE(0,100) ck(1:lt),vartyp(1:lz)
              fail1=.true.
              fail=.true.
          END IF
          RETURN
      END IF
      IF(ktyp1.NE.ktyp0) THEN
          WRITE(0,106) ck(1:lt),ktyp0,ktyp1
          STOP '**** rdncha: abnormal end ****'
      END IF
 100  FORMAT(' ERROR: Missing definition of keyword "',a,'" (type: ',
     +       a,')')
 106  FORMAT(' ERROR: keyword type mismatch'/
     +       '        KEY = "',A,'"'/
     +       '        KTYP0 =',I2/
     +       '        KTYP1 =',I2)

* Read value into output variable
      DO 2 i=1,n
      CALL strcnt(val,v(i),rest,error)
      IF(error) GOTO 1
      val=rest
 2    CONTINUE
      RETURN

* Error message
 1    CONTINUE
      WRITE(0,101) ck(1:lt),vartyp(1:lz),kr,file(1:lf)
 101  FORMAT(' ERROR: Abnormal definition of keyword "',a,'" (type: ',
     +       a,')'/8x,'(record',i4,' in file "',a,'")')
      fail1=.true.
      fail=.true.
      END
