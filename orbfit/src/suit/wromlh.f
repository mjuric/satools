* Copyright (C) 1997-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 21, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R O M L H                           *
*  *                                                               *
*  *       Writes the header of an orbital element file            *
*  *                     (multi-line format)                       *
*  *                                                               *
*  *****************************************************************
*
* OUTPUT:   UNIT      -  Output FORTRAN unit
*           RSYS      -  Reference system type (EQUM/EQUT/ECLM)
*           EPOCH     -  Reference system epoch (J2000/OFDATE)
*
      SUBROUTINE wromlh(unit,rsys,epoch)
      IMPLICIT NONE

      INTEGER unit
      CHARACTER*(*) rsys,epoch

      INCLUDE 'parcmc.h'

      INTEGER l1,l2,nb
      CHARACTER*100 bl

      INTEGER lench
      EXTERNAL lench

      l1=lench(rsys)
      l2=lench(epoch)
      nb=MAX(14-l1-l2,1)
      bl=' '

      WRITE(unit,100) comcha,comcha,rsys(1:l1),epoch(1:l2),
     +                bl(1:nb),comcha
 100  FORMAT('format  = ''OEF1.1''       ',A,' file format'/
     +       'rectype = ''ML''           ',A,' record type (1L/ML)'/
     +       'refsys  = ',A,1X,A,A,A,' default reference system'/
     +       'END_OF_HEADER')

      END
