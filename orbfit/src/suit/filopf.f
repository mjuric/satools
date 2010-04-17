* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: September 19, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         F I L O P F                           *
*  *                                                               *
*  *        Unit allocation and file opening (STATUS='old')        *
*  *         (searching also in default library directory)         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    NAME      -  File name to be opened
*
* OUTPUT:   IUN       -  Allocated unit
*           FOUND     -  Was the file found?
*
      SUBROUTINE filopf(iun,name,found)
      IMPLICIT NONE

      INCLUDE 'comfil.h'

* NEEDED common blocks:
      INCLUDE 'comlib.h'

      INTEGER iun,ll,i
      CHARACTER*(*) name
      CHARACTER*120 tname
      LOGICAL found

      INTEGER lench
      EXTERNAL lench

      IF(iiclib.NE.36) STOP '**** filopf: internal error (01) ****'

      IF(iicfil.NE.36) THEN
          DO 1 i=iunf1,iunf2
 1        allunt(i)=.false.
          iicfil=36
      END IF

      tname=name
      INQUIRE(FILE=tname,EXIST=found)
      IF(.NOT.found) THEN
          tname=libdir(1:lenld)//name
          INQUIRE(FILE=tname,EXIST=found)
          IF(.NOT.found) THEN
              iun=0
              RETURN
          END IF
      END IF

      DO 2 iun=iunf1,iunf2
      IF(allunt(iun)) GOTO 2
      OPEN(iun,FILE=tname,STATUS='old',ERR=3)
      filnam(iun)=tname
      allunt(iun)=.true.
      RETURN
 2    CONTINUE

      STOP '**** filopf: all units are already allocated ****'

 3    CONTINUE
      ll=lench(tname)
      write(99,101) tname(1:ll)
 101  FORMAT(' **** filopf: cannot OPEN file "',a,'" (STATUS=old) ****')
      STOP '**** filopf: abnormal end ****'
      END
