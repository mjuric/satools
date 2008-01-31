* Copyright (C) 1996-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: January 12, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         L I B I N I                           *
*  *                                                               *
*  *         Inizialization of default library directory           *
*  *                                                               *
*  *****************************************************************
*
      SUBROUTINE libini
      IMPLICIT NONE

      INCLUDE 'sysdep.h'
      INCLUDE 'parlib.h'

* Common blocks to be initialized:
      INCLUDE 'comlib.h'

      LOGICAL found
      INTEGER unit

      INTEGER lench
      EXTERNAL lench

* The file 'libdir.dat' can be used to modify the path of the
* library directory (with respect to the built-in value contained
* in parlib.h) without need of compiling again the software:
* it is searched only in the current (working) directory
      INQUIRE(FILE='libdir.dat',EXIST=found)

      IF(found) THEN
          CALL filopn(unit,'libdir.dat','old')
          READ(unit,100,END=10,ERR=10) libdir
          CALL filclo(unit,' ')
      ELSE
          libdir=dlibd
      END IF
 100  FORMAT(A)
      lenld=lench(libdir)
      IF(libdir(lenld:lenld).NE.dircha) THEN
          lenld=lenld+1
          libdir(lenld:lenld)=dircha
      END IF

      iiclib=36
      RETURN

 10   CONTINUE
      STOP '**** libini: error reading file "libdir.dat" ****'

      END
