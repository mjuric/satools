* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 5, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         F I L O P N                           *
*  *                                                               *
*  *               Unit allocation and file opening                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    NAME      -  File name to be opened
*           STATUS    -  Open status
*
* OUTPUT:   IUN       -  Allocated unit
*
      SUBROUTINE filopn(iun,name,status)
      IMPLICIT NONE

      INCLUDE 'comfil.h'

      INTEGER iun,ll,ls,i
      CHARACTER*(*) name,status
      LOGICAL opnd

      INTEGER lench
      EXTERNAL lench

      INQUIRE(FILE=name,OPENED=opnd)
      IF(opnd) THEN
          ll=lench(name)
          WRITE(*,102) name(1:ll)
          STOP '**** filopn: abnormal end ****'
      END IF
 102  FORMAT(' **** filopn: internal error (01) ****'/
     +       ' **** FILE: ',A,' ****')

      IF(iicfil.NE.36) THEN
          DO 1 i=iunf1,iunf2
 1        allunt(i)=.false.
          iicfil=36
      END IF

      DO 2 iun=iunf1,iunf2
      IF(allunt(iun)) GOTO 2
      OPEN(iun,FILE=name,STATUS=status,ERR=3)
      filnam(iun)=name
      allunt(iun)=.true.
      RETURN
 2    CONTINUE

      STOP '**** filopn: all units are already allocated ****'

 3    CONTINUE
      ll=lench(name)
      ls=lench(status)
      WRITE(*,101) name(1:ll),status(1:ls)
 101  FORMAT(' **** filopn: cannot OPEN file "',A,'" (status=',A,
     +       ') ****')
      STOP '**** filopn: abnormal end ****'
      END
