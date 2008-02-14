* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 5, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D M C H A                           *
*  *                                                               *
*  *    Read a character vector quantity from the input namelist   *
*  *  inferring the dimension from the number of values supplied   *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    CAT       -  Namelist keyword category
*           KEY       -  Namelist keyword name
*           NX        -  Dimension of the vector to be read
*           NAMNX     -  Real name of the NX variable
*           REQRD     -  If true, when the keyword is not found in the
*                        namelist, an error message is issued and the
*                        FAIL flag is set
*
* OUTPUT:   V         -  Value associated with keyword
*           N         -  Actual number of elements read in the vector
*           FOUND     -  True when the keyword has been found in the
*                        namelist
*           FAIL1     -  True if(.not.found.and.reqrd); false otherwise
*           FAIL      -  Set to true if(.not.found.and.reqrd);
*                        otherwise, not changed
*
      SUBROUTINE rdmcha(cat,key,v,n,nx,namnx,reqrd,found,fail1,fail)
      IMPLICIT NONE

      INCLUDE 'parnam.h'

      CHARACTER*(*) cat,key
      CHARACTER ck*(lckx),val*(lcvx),file*(lcfx),rest*(lcvx)
      CHARACTER*50 vartyp
      LOGICAL reqrd,found,fail,fail1,error
      INTEGER n,nx,ktyp0,ktyp1,lc,lk,lt,kr,lz,lf,i

      CHARACTER*(*) namnx
      CHARACTER*(*) v(nx)

      INTEGER lench,nitchs
      EXTERNAL lench,nitchs

      vartyp='CHARACTER VECTOR'
      ktyp0=3
      n=0

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
      n=nitchs(val)
      CALL chkpdf(n,nx,namnx)
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
