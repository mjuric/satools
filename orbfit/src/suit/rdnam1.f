* Copyright (C) 1997-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: January 12, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D N A M 1                           *
*  *                                                               *
*  *     Reads a namelist from input file and stores in common     *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    IUN       -  Input FORTRAN unit
*
      SUBROUTINE rdnam1(iun)
      IMPLICIT NONE

      INTEGER iun

      INCLUDE 'parnam.h'
      INCLUDE 'parch.h'
      INCLUDE 'parcmc.h'

* NEEDED common blocks:
      INCLUDE 'comnam.h'

      CHARACTER*(lchx) rec,rec1,rec2,key1,key2,keyt,val1,infile,defcat
      CHARACTER*(lchx) rest
      CHARACTER*100 inpfl
      LOGICAL opnd,error
      INTEGER kr,ldc,lf,lr,ipc,lr2,iuna,ipu,lk,lv,ip1,ip2,lk2,i

      INTEGER lench
      EXTERNAL lench

      IF(iicnam.NE.36) STOP '**** rdnam1: internal error (01) ****'

* Name of the input file
      INQUIRE(iun,OPENED=opnd,NAME=infile)
      IF(.NOT.opnd) STOP '**** rdnam1: internal error (02) ****'
      lf=lench(infile)
      CALL chkpdf(lf,lcfx,'lcfx')

      kr=0
      defcat=' '
      ldc=0
 1    READ(iun,100,end=10) rec
 100  FORMAT(a)
      kr=kr+1

      rec1=rec
      CALL rmsp(rec1,lr)
      IF(lr.LE.0) GOTO 1

* Compute length excluding comments
      lr=lench(rec)
***   IF(lr.LT.1) GOTO 1
      ipc=INDEX(rec(1:lr),comcha)
      IF(ipc.EQ.1) GOTO 1
      IF(ipc.EQ.0) THEN
          rec1=rec(1:lr)
      ELSE
          rec1=rec(1:ipc-1)
          lr=lench(rec1)
          IF(lr.LT.1) GOTO 1
      END IF

* Processing of "INPUT:" special keyword
      rec2=rec1
      CALL norstr(rec2,lr2)
      IF(lr2.LT.6) GOTO 4
      IF(rec2(1:6).EQ.'INPUT:') THEN
          CALL strcnt(rec2(7:lr2),inpfl,rest,error)
          IF(error) GOTO 21
          CALL filopl(iuna,inpfl)
          CALL rdnam2(iuna)
          CALL filclo(iuna,' ')
          GOTO 1
      END IF
 4    CONTINUE

* Keyword field
      ipu=INDEX(rec1(1:lr),'=')
      IF(ipu.EQ.0) THEN
          CALL rmsp(rec1,lk)
          CALL chkfln(lk,lckx,'keyword',kr,infile)
          key1=rec1(1:lk)
          val1=' '
          GOTO 2
      END IF
      key1=rec1(1:ipu-1)
      CALL rmsp(key1,lk)
      CALL chkfln(lk,lckx,'keyword',kr,infile)

* Value field
      val1=rec1(ipu+1:)
      CALL norstr(val1,lv)
      CALL chkfln(lv,lcvx,'value',kr,infile)
 2    CONTINUE

* Handling of default category
      IF(key1(lk:lk).EQ.'.') THEN
          ldc=lk-1
          defcat=key1(1:ldc)
          GOTO 1
      END IF
      IF(key1(1:1).EQ.'.') THEN
          IF(ldc.LE.0) THEN
              write(99,101) key1(1:lk),infile(1:lf),kr
              STOP '**** rdnam1: abnormal end ****'
          END IF
          keyt=defcat(1:ldc)//key1(1:lk)
          key1=keyt
          lk=lk+ldc
      END IF
 101  FORMAT(' ERROR: missing default category declaration'/
     +       '        ambiguous keyword "',a,'"'/
     +       '        (file "',a,'", record',i4,')')

* Check for parentheses
      ip1=INDEX(key1,'(')
      IF(ip1.EQ.0) THEN
          ip2=INDEX(key1,')')
          IF(ip2.NE.0) THEN
              write(99,102) key1(1:lk),infile(1:lf),kr
              STOP '**** rdnam1: abnormal end ****'
          END IF
      ELSE
          key2=key1(ip1+1:)
          lk2=lench(key2)
          IF(lk2.LE.0.OR.key2(lk2:lk2).NE.')') THEN
              write(99,102) key1(1:lk),infile(1:lf),kr
              STOP '**** rdnam1: abnormal end ****'
          END IF
      END IF
 102  FORMAT(' ERROR: illegal use of parentheses'/
     +       '        in keyword "',a,'"'/
     +       '        (file "',a,'", record',i4,')')

* Look if the key is already present in the namelist
      DO 3 i=1,nne
      IF(key1(1:lk).EQ.keys(i)) THEN
          vals(i)=val1
          namif(i)=infile
          krecnm(i)=kr
          kuord(i)=0
          GOTO 1
      END IF
 3    CONTINUE

      nne=nne+1
      CALL chkpdf(nne,nnex,'nnex')
      i=nne
      keys(i)=key1
      vals(i)=val1
      namif(i)=infile
      krecnm(i)=kr
      kuord(i)=0
      GOTO 1

 10   CONTINUE
      RETURN

* Error messages
 21   CONTINUE
      write(99,106) infile(1:lf),kr
 106  FORMAT(' ERROR: illegal "INPUT:" statement'/
     +       '        (file "',a,'", record',i4,')')
      STOP '**** rdnam1: abnormal end ****'

      END
