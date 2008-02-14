* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: September 19, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         F I L O P L                           *
*  *                                                               *
*  *        Unit allocation and file opening (STATUS='old')        *
*  *         (searching also in default library directory)         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    NAME      -  File name to be opened
*
* OUTPUT:   IUN       -  Allocated unit
*
      SUBROUTINE filopl(iun,name)
      IMPLICIT NONE

      INCLUDE 'comfil.h'

* NEEDED common blocks:
      INCLUDE 'comlib.h'

      INTEGER iun,ll,i
      CHARACTER*(*) name
      CHARACTER*120 nam1
      LOGICAL found

      INTEGER lench
      EXTERNAL lench

      IF(iiclib.NE.36) STOP '**** filopl: internal error (01) ****'

      IF(iicfil.NE.36) THEN
          DO 1 i=iunf1,iunf2
 1        allunt(i)=.false.
          iicfil=36
      END IF

      nam1=name
      INQUIRE(FILE=nam1,EXIST=found)
      IF(.NOT.found) THEN
          nam1=libdir(1:lenld)//name
          INQUIRE(FILE=nam1,EXIST=found)
          IF(.NOT.found) THEN
              ll=lench(name)
              WRITE(0,102) name(1:ll)
              STOP '**** filopl: abnormal end ****'
          END IF
      END IF
 102  FORMAT(' **** filopl: cannot find file "',a,'" ****')

      DO 2 iun=iunf1,iunf2
      IF(allunt(iun)) GOTO 2
      OPEN(iun,FILE=nam1,STATUS='old',ERR=3)
      filnam(iun)=nam1
      allunt(iun)=.true.
      RETURN
 2    CONTINUE

      STOP '**** filopl: all units are already allocated ****'

 3    CONTINUE
      ll=lench(nam1)
      WRITE(0,101) nam1(1:ll)
 101  FORMAT(' **** filopl: cannot OPEN file "',a,'" (STATUS=old) ****')
      STOP '**** filopl: abnormal end ****'
      END
