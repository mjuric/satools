* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 8, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D F N A M                           *
*  *                                                               *
*  *            Read simplified file-header namelist               *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Input unit
*           FILE      -  Input filename (for error messages)
*
* OUTPUT:   NR        -  Records read so far
*
      SUBROUTINE rdfnam(unit,file,nr)
      IMPLICIT NONE

      INCLUDE 'parnam.h'
      INCLUDE 'parch.h'
      INCLUDE 'parcmc.h'

* Common blocks to be initialized:
      INCLUDE 'comfnm.h'

      INTEGER unit,nr
      INTEGER lf,lk,i
      CHARACTER*(lchx) rec,key1,val1,comm
      CHARACTER*(*) file
      LOGICAL skip,end

      INTEGER lench
      EXTERNAL lench

      iicfnm=0

      nfne=0
      kuorlf=0
      nmif=file
      hnfuni=unit
      lf=lench(file)
      nr=0

* Read a record from the namelist
 2    READ(unit,100,end=11) rec
 100  FORMAT(a)
      nr=nr+1
      CALL splkvc(rec,key1,val1,comm,skip,end)

* Detection of the end of the namelist
      IF(end) THEN
          iicfnm=36
          RETURN
      END IF

* Skip comment lines
      IF(skip) GOTO 2

* Look if the key is already present in the namelist
      lk=lench(key1)
      DO 4 i=1,nfne
      IF(key1(1:lk).EQ.keysf(i)) THEN
          WRITE(0,210) key1(1:lk),file(1:lf),krcfnm(i),nr
          STOP '**** rdfnam: abnormal end ****'
      END IF
 4    CONTINUE
 210  FORMAT(' rdfnam: duplicate definition of keyword "',a,
     +       '" in file "',a,'":'/
     +       '         first  occurence at line',i5/
     +       '         second occurence at line',i5)

      nfne=nfne+1
      CALL chkpdf(nfne,nfnex,'nfnex')
      i=nfne
      keysf(i)=key1
      valsf(i)=val1
      krcfnm(i)=nr
      kuorf(i)=0
      GOTO 2

 11   CONTINUE

* Abort when no "END_OF_HEADER" record is found
*     WRITE(0,200)'end',file(1:lf),nr
*     STOP '**** rdfnam: abnormal end ****'
*200  FORMAT(' rdfnam: cannot find namelist ',a,':'/
*    +       '         unexpected END of file "',a,'" after record',i6)

      iicfnm=36

      END
