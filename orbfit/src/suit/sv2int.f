* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 23, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         S V 2 I N T                           *
*  *                                                               *
*  *        Translation of string-valued keywords to integer       *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    CAT       -  Namelist keyword category
*           KEY       -  Namelist keyword name
*           REQRD     -  If true, when the keyword is not found in the
*                        namelist, an error message is issued and the
*                        FAIL flag is set
*
* OUTPUT:   V         -  Value associated with keyword
*           IVK       -  Integer translation of the Value associated
*                        with keyword
*           FOUND     -  True when the keyword has been found in the
*                        namelist
*           FAIL1     -  True if(.NOT.found.AND.reqrd); false otherwise
*           FAIL      -  Set to true if(.NOT.found.AND.reqrd);
*                        otherwise, not changed
*
      SUBROUTINE sv2int(cat,key,v,ivk,reqrd,found,fail1,fail)
      IMPLICIT NONE

      INCLUDE 'parnam.h'

      CHARACTER*(*) cat,key
      CHARACTER ck*(lckx),val*(lcvx),file*(lcfx),rest*(lcvx)
      CHARACTER*50 vartyp
      LOGICAL reqrd,found,fail,fail1,error

* NEEDED common blocks:
      INCLUDE 'comkls.h'

      CHARACTER v*(*)
      INTEGER ivk,ktyp0,lc,lk,lt,ktyp1,kr,lz,lf,i,lk2,lv1,k,lv2,kk

      INTEGER lench
      EXTERNAL lench

      vartyp='CHARACTER'
      ktyp0=3

      IF(iickls.NE.36) STOP '**** sv2int: internal error (01) ****'

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
              write(99,100) ck(1:lt),vartyp(1:lz)
              fail1=.true.
              fail=.true.
              ivk=0
          END IF
          RETURN
      END IF
      IF(ktyp1.NE.ktyp0) THEN
          write(99,106) ck(1:lt),ktyp0,ktyp1
          STOP '**** sv2int: abnormal end ****'
      END IF
 100  FORMAT(' ERROR: Missing definition of keyword "',a,'" (type: ',
     +       a,')')
 106  FORMAT(' ERROR: keyword type mismatch'/
     +       '        KEY = "',A,'"'/
     +       '        KTYP0 =',I2/
     +       '        KTYP1 =',I2)
      CALL strcnt(val,v,rest,error)
      IF(error) GOTO 1

* Search keyword in list
      DO 10 i=1,nkls
      lk2=lench(keylst(i))
      IF(ck(1:lt).EQ.keylst(i)(1:lk2)) GOTO 2
 10   CONTINUE
      STOP '**** sv2int: internal error (02) ****'
 2    CONTINUE

* Search string value
      IF(ns2i(i).LE.0) STOP '**** sv2int: internal error (03) ****'
      lv1=lench(v)
      DO 3 k=1,ns2i(i)
      kk=k+ipos2i(i)
      lv2=lench(vallst(kk))
      IF(v(1:lv1).NE.vallst(kk)(1:lv2)) GOTO 3
      ivk=intlst(kk)
      RETURN
 3    CONTINUE

      write(99,103) ck(1:lt),vartyp(1:lz),kr,file(1:lf),v(1:lv1)
 103  FORMAT(' ERROR: unexpected value of keyword "',a,'" (type: ',
     +       a,')'/8x,'(record',i4,' in file "',a,'")'/
     +       8x,'Present value: ''',a,''''/
     +       8x,'Possible values are:')
      DO 4 k=1,ns2i(i)
      kk=k+ipos2i(i)
      lv2=lench(vallst(kk))
      write(99,102) vallst(kk)(1:lv2)
 102  FORMAT(13x,'''',a,'''')
 4    CONTINUE

      fail1=.true.
      fail=.true.
      RETURN

* Error message
 1    CONTINUE
      write(99,101) ck(1:lt),vartyp(1:lz),kr,file(1:lf)
 101  FORMAT(' ERROR: Abnormal definition of keyword "',a,'" (type: ',
     +       a,')'/8x,'(record',i4,' in file "',a,'")')
      fail1=.true.
      fail=.true.

      END
