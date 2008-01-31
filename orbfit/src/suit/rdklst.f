* Copyright (C) 1997-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: January 13, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D K L S T                           *
*  *                                                               *
*  *               Read a the list of valid keys                   *
*  *   (with optional translation of string values to integers)    *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  Input file
*
      SUBROUTINE rdklst(file)
      IMPLICIT NONE

      INCLUDE 'parnam.h'
      INCLUDE 'parcmc.h'
      INCLUDE 'parch.h'

* Common blocks to be initialized:
      INCLUDE 'comkls.h'

      CHARACTER*(*) file
      CHARACTER*(lchx) rec,rest,rec1,key1,key2,stval,defcat,kt
      CHARACTER*3 ktyp
      CHARACTER*100 file1
      LOGICAL nospli,error
      INTEGER kr,ldc,iun,lv,lr,ipc,lk,ni,i,lkt

      INTEGER lench,nitchs
      EXTERNAL lench,nitchs

      nkls=0
      ns2it=0
      kr=0
      defcat=' '
      ldc=0
      CALL filopl(iun,file)

 1    READ(iun,100,end=10) rec
 100  FORMAT(a)
      kr=kr+1

* Handling of integer translations of string values
      IF(rec(1:2).EQ.'$I') THEN
          IF(nkls.LE.0) STOP '**** rdklst: internal error (01) ****'
          IF(keytyp(nkls).NE.3) THEN
              WRITE(*,103) kr
              STOP '**** rdklst: abnormal end ****'
          END IF
          ns2it=ns2it+1
          CALL chkpdf(ns2it,ns2itx,'ns2itx')
          rest=rec(3:)
          rec=rest
          CALL strcnt(rec,stval,rest,error)
          IF(error) STOP '**** rdklst: internal error (02) ****'
          rec=rest
          READ(rec,*,ERR=11) intlst(ns2it)
          lv=lench(stval)
          CALL chkpdf(lv,lcvx,'lcvx')
          vallst(ns2it)=stval(1:lv)
          ns2i(nkls)=ns2i(nkls)+1
          GOTO 1
      END IF
 103  FORMAT(' **** rdklst: $I following a non-string keyword',
     +       'at line',i4,' ****')

* Compute length excluding comments
      lr=lench(rec)
      IF(lr.LT.1) GOTO 1
      ipc=INDEX(rec(1:lr),comcha)
      IF(ipc.EQ.1) GOTO 1
      IF(ipc.EQ.0) THEN
          rec1=rec(1:lr)
      ELSE
          rec1=rec(1:ipc-1)
          lr=lench(rec1)
          IF(lr.LT.1) GOTO 1
      END IF
      CALL norstr(rec1,lk)

* Handling of "INPUT:" special keyword
      IF(lk.LT.6) GOTO 2
      IF(rec1(1:6).EQ.'INPUT:') THEN
          CALL strcnt(rec1(7:),file1,rest,error)
          IF(error) THEN
              WRITE(*,105) kr
              STOP '**** rdklst: abnormal end ****'
          END IF
          CALL rdkls1(file1)
          GOTO 1
      END IF
 105  FORMAT(' **** rdklst: input error at line',I4,' ****')
 2    CONTINUE

* Keyword + type
      CALL chkpdf(lk,lckx,'lckx')
      kt=rec1(1:lk)
      ni=nitchs(kt)
      IF(ni.EQ.1) THEN
          key1=kt(1:lk)
          ktyp=' '
      ELSEIF(ni.EQ.2) THEN
          CALL stspli(kt,' ',key1,nospli)
          IF(nospli) STOP '**** rdklst: internal error (04) ****'
          ktyp=kt
      ELSE
          WRITE(*,101) ni,kr
          STOP '**** rdklst: abnormal end ****'
      END IF
 101  FORMAT(' **** rdklst: INTERNAL ERROR:',i3,' items at line',i5,
     +       ' ****')

* Handling of default category
      IF(ni.EQ.1) THEN
          IF(key1(lk:lk).NE.'.') THEN
              WRITE(*,102) kr
              STOP '**** rdklst: abnormal end ****'
          END IF
          ldc=lk-1
          defcat=key1(1:ldc)
          GOTO 1
      END IF
      IF(key1(1:1).EQ.'.') THEN
          IF(ldc.LE.0) STOP '**** rdklst: internal error (05) ****'
          key2=defcat(1:ldc)//key1(1:lk)
          key1=key2
          lk=lk+ldc
      END IF
 102  FORMAT(' **** rdklst: ERROR: defcat without ending dot at line',
     +       i5,' ****')

* Look if the key is already present in the namelist
      DO 3 i=1,nkls
      IF(key1(1:lk).EQ.keylst(i)) THEN
          WRITE(*,110) key1(1:lk),kr
          STOP '**** rdklst: abnormal end ****'
      END IF
 3    CONTINUE
 110  FORMAT(' **** rdklst: duplicate key ****'/
     +       '      KEY=',a,' (line',i4,')')

* Add the new keyword to the list
      nkls=nkls+1
      CALL chkpdf(nkls,nklsx,'nklsx')
      keylst(nkls)=key1
      ns2i(nkls)=0
      ipos2i(nkls)=ns2it
      IF(ktyp.EQ.'INT') THEN
          keytyp(nkls)=1
      ELSEIF(ktyp.EQ.'REA') THEN
          keytyp(nkls)=2
      ELSEIF(ktyp.EQ.'CHA') THEN
          keytyp(nkls)=3
      ELSEIF(ktyp.EQ.'LOG') THEN
          keytyp(nkls)=4
      ELSEIF(ktyp.EQ.'MJD' .OR. ktyp.EQ.'TIM') THEN
          keytyp(nkls)=5
      ELSEIF(ktyp.EQ.'REF') THEN
          keytyp(nkls)=6
      ELSE
          lkt=lench(ktyp)
          WRITE(*,111) ktyp(1:lkt),kr
          STOP '**** rdklst: abnormal end ****'
      END IF
 111  FORMAT(' **** rdklst: unknown key type ****'/
     +       '      KTYPE="',a,'" (line',i4,')')
      GOTO 1

 10   CONTINUE
      iickls=36
      CALL filclo(iun,' ')
      RETURN

 11   CONTINUE
      STOP '**** rdklst: internal error (03) ****'

      END
