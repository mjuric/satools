* Copyright (C) 1997-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 21, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R O 1 L H                           *
*  *                                                               *
*  *       Writes the header of an orbital element file            *
*  *   (single-line format, different epochs, keplerian elements)  *
*  *                                                               *
*  *****************************************************************
*
* OUTPUT:   UNIT      -  Output FORTRAN unit
*           RSYS      -  Reference system type (EQUM/EQUT/ECLM)
*           EPOCH     -  Reference system epoch (J2000/OFDATE)
*           ELTYPE    -  Type of orbital elements (EQU/KEP/CAR)
*
      SUBROUTINE wro1lh(unit,rsys,epoch,eltype)
      IMPLICIT NONE

      INTEGER unit
      CHARACTER*(*) rsys,epoch,eltype

      INCLUDE 'parcmc.h'

      INTEGER l1,l2,l3,nb
      CHARACTER*100 bl

      INTEGER lench
      EXTERNAL lench

      l1=lench(rsys)
      l2=lench(epoch)
      l3=lench(eltype)
      nb=MAX(14-l1-l2,1)
      bl=' '

      WRITE(unit,100) comcha,comcha,eltype(1:l3),comcha,
     +                rsys(1:l1),epoch(1:l2),
     +                bl(1:nb),comcha
 100  FORMAT('format  = ''OEF1.1''       ',A,' file format'/
     +       'rectype = ''1L''           ',A,' record type (1L/ML)'/
     +       'elem    = ''',A,'''          ',A,
     +                   ' type of orbital elements'/
     +       'refsys  = ',A,1X,A,A,A,' default reference system'/
     +       'END_OF_HEADER')

      IF(eltype.EQ.'KEP') THEN
          WRITE(unit,201) comcha
      ELSEIF(eltype.EQ.'CAR') THEN
          WRITE(unit,202) comcha
      ELSEIF(eltype.EQ.'EQU') THEN
          WRITE(unit,203) comcha
      END IF

 201  FORMAT(A,' Name, Epoch(MJD), a, e, i, long. node,',
     +         ' arg. peric., mean anomaly')
 202  FORMAT(A,' Name, Epoch(MJD), cartesian position and velocity',
     +         ' vectors')
 203  FORMAT(A,' Name, Epoch(MJD), a, e*sin(LP), e*cos(LP),',
     +         ' tan(i/2)*sin(LN), tan(i/2)*cos(LN), mean long.')

      END
