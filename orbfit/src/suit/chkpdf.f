* Copyright (C) 1996-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: January 12, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C H K P D F                           *
*  *                                                               *
*  *                 Check parameter definition                    *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    NA        -  Actual value required for the parameter
*           NX        -  Value declared for the parameter
*           NAME      -  Parameter name
*
      SUBROUTINE chkpdf(na,nx,name)
      IMPLICIT NONE

      INTEGER na,nx,ll
      CHARACTER*(*) name

      INTEGER lench
      EXTERNAL lench

      IF(na.LE.nx) RETURN

      ll=lench(name)
      WRITE(*,100) name(1:ll),na
 100  FORMAT(' **** Insufficient PARAMETER definition ****'/
     +       ' **** Please set ',a,' >=',i7,' ****')

      STOP '**** chkpdf: abnormal end ****'
      END
