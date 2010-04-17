* Copyright (C) 1996-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: January 13, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C H K F L N                           *
*  *                                                               *
*  *                     Check field length                        *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    NA        -  Actual value required for the parameter
*           NX        -  Value declared for the parameter
*           NAME      -  Parameter name
*           KR        -  Record number in input file
*           FILE      -  Input file name
*
      SUBROUTINE chkfln(na,nx,name,kr,file)
      IMPLICIT NONE

      INTEGER na,nx,kr,ll,lf
      CHARACTER*(*) name,file

      INTEGER lench
      EXTERNAL lench

      IF(na.LE.nx) RETURN

      ll=lench(name)
      lf=lench(file)
      write(99,100) name(1:ll),nx,kr,file(1:lf),na
 100  FORMAT(' **** Insufficient PARAMETER definition ****'/
     + ' Sorry, the present version of the program does not',
     + ' allow'/
     + ' a length of the "',a,'" field longer than',i4,' characters'/
     + ' Please correct line',i4,' in file ',a/
     + ' (length of the field =',i4,' characters)')
      STOP '**** chkfln: abnormal end ****'
      END
