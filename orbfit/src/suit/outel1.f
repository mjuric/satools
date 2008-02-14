* Copyright (C) 1997-2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 13, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         O U T E L 1                           *
*  *                                                               *
*  *           Verbose output of a set orbital elements            *
*  *                       to a report file                        *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Output FORTRAN unit (report file)
*           ELEM      -  Orbital elements (ECLM J2000)
*           ELTYPE    -  Type of orbital elements (EQU/KEP/CAR)
*           T0        -  Epoch of orbital elements (MJD, TDT)
*           LABEL     -  Label
*           MULTI     -  Multi-line output
*           STDOUT    -  Standard output
*           W         -  Indentation width
*
      SUBROUTINE outel1(unit,elem,eltype,t0,label,multi,stdout,w)
      IMPLICIT NONE

      INTEGER unit,w
      DOUBLE PRECISION elem(6),t0
      CHARACTER*(*) eltype,label
      LOGICAL multi,stdout

      INCLUDE 'trig.h'

      INTEGER lt,i,day,month,year,ll
      CHARACTER cm*3
      DOUBLE PRECISION hour

      CHARACTER*50 b
      DATA b/'                                                  '/
      SAVE b

      INTEGER lench
      CHARACTER*3 chmon
      EXTERNAL lench,chmon

      IF(w.LE.0) STOP '**** outel1: internal error (01) ****'
      IF(w.GT.LEN(b)) STOP '**** outel1: internal error (02) ****'

      ll=lench(label)

      IF(multi .AND. (ll.GT.0)) THEN
          IF(unit.GT.0) WRITE(unit,133) b(1:w),label(1:ll)
          IF(stdout) WRITE(0,133) b(1:w),label(1:ll)
      END IF
      IF(eltype.EQ.'KEP') THEN
          IF(multi) THEN
              IF(unit.GT.0) WRITE(unit,100)
     +            b(1:w),elem(1),b(1:w),elem(2),
     +            (b(1:w),elem(i)*degrad,i=3,6)
              IF(stdout) WRITE(0,100)
     +            b(1:w),elem(1),b(1:w),elem(2),
     +            (b(1:w),elem(i)*degrad,i=3,6)
          ELSE
              IF(ll.GT.0) THEN
                  IF(unit.GT.0) WRITE(unit,120)
     +                b(1:w),label(1:ll),elem(1),elem(2),
     +                (elem(i)*degrad,i=3,6),t0
                  IF(stdout) WRITE(0,120)
     +                 b(1:w),label(1:ll),elem(1),elem(2),
     +                (elem(i)*degrad,i=3,6),t0
              ELSE
                  IF(unit.GT.0) WRITE(unit,130)
     +                b(1:w),elem(1),elem(2),(elem(i)*degrad,i=3,6),t0
                  IF(stdout) WRITE(unit,130)
     +                b(1:w),elem(1),elem(2),(elem(i)*degrad,i=3,6),t0
              END IF
          END IF
      ELSEIF(eltype.EQ.'EQU') THEN
          IF(multi) THEN
              IF(unit.GT.0) WRITE(unit,101)
     +            (b(1:w),elem(i),i=1,5),b(1:w),elem(6)*degrad
              IF(stdout) WRITE(0,101)
     +            (b(1:w),elem(i),i=1,5),b(1:w),elem(6)*degrad
          ELSE
              IF(ll.GT.0) THEN
                  IF(unit.GT.0) WRITE(unit,121)
     +                b(1:w),label(1:ll),(elem(i),i=1,5),
     +                elem(6)*degrad,t0
                  IF(stdout) WRITE(0,121)
     +                b(1:w),label(1:ll),(elem(i),i=1,5),
     +                elem(6)*degrad,t0
              ELSE
                  IF(unit.GT.0) WRITE(unit,131)
     +                b(1:w),(elem(i),i=1,5),elem(6)*degrad,t0
                  IF(stdout) WRITE(0,131)
     +                b(1:w),(elem(i),i=1,5),elem(6)*degrad,t0
              END IF
          END IF
      ELSEIF(eltype.EQ.'CAR') THEN
          IF(multi) THEN
              IF(unit.GT.0) WRITE(unit,102)
     +            b(1:w),(elem(i),i=1,3),b(1:w),(elem(i),i=4,6)
              IF(stdout) WRITE(0,102)
     +            b(1:w),(elem(i),i=1,3),b(1:w),(elem(i),i=4,6)
          ELSE
              IF(ll.GT.0) THEN
                  IF(unit.GT.0) WRITE(unit,122)
     +                b(1:w),label(1:ll),elem,t0
                  IF(stdout) WRITE(0,122)
     +                b(1:w),label(1:ll),elem,t0
              ELSE
                  IF(unit.GT.0) WRITE(unit,132) b(1:w),elem,t0
                  IF(stdout) WRITE(0,132) b(1:w),elem,t0
              END IF
          END IF
      ELSE
          lt=lench(eltype)
          WRITE(0,200) eltype(1:lt)
          STOP '**** outel1: abnormal end ****'
      END IF
 120  FORMAT(A,'KepElem(',A,'):',1P,E15.7,0P,F13.8,4F10.5,
     +           ' (T=',F10.3,')')
 130  FORMAT(A,'KepElem:',1P,E15.7,0P,F13.8,4F10.5,
     +           ' (T=',F10.3,')')
 121  FORMAT(A,'EQUElem(',A,'):',1P,E15.7,0P,4F13.8,F10.5,
     +           ' (T=',F10.3,')')
 131  FORMAT(A,'EQUElem:',1P,E15.7,0P,4F13.8,F10.5,' (T=',F10.3,')')
 122  FORMAT(A,'PosVel(',A,'):',1P,6E15.7,' (T=',F10.3,')')
 132  FORMAT(A,'PosVel:',1P,6E15.7,' (T=',F10.3,')')
 100  FORMAT(A,'Semimajor axis     =',1P,E23.14,0P,' AU'/
     +       A,'Eccentricity       =',F20.15/
     +       A,'Inclination        =',F18.13,' deg'/
     +       A,'Long. of node      =',F18.13,' deg'/
     +       A,'Arg. of pericenter =',F18.13,' deg'/
     +       A,'Mean anomaly       =',F18.13,' deg')
 101  FORMAT(A,'Semimajor axis     =',1P,E23.14,0P,' AU'/
     +       A,'h [e*sin(w)]       =',F20.15/
     +       A,'k [e*cos(w)]       =',F20.15/
     +       A,'P [tg(i/2)*sin(N)] =',F20.15/
     +       A,'Q [tg(i/2)*cos(N)] =',F20.15/
     +       A,'Mean longitude     =',F18.13,' deg')
 102  FORMAT(A,'Position vector   =',1X,1P,3E22.14,' AU'/
     +       A,'Velocity vector   =',1X,3E22.14,' AU/d')
 200  FORMAT('ERROR: unknown type "',A,'" of orbital elements')
 133  FORMAT(A,'Orbital elements for ',A,':')

      IF(multi) THEN
          CALL mjddat(t0,day,month,year,hour)
          cm=chmon(month)
          WRITE(unit,110) b(1:w),t0,cm,day,year,hour
          IF(stdout) WRITE(0,110) b(1:w),t0,cm,day,year,hour
      END IF
 110  FORMAT(A,'Epoch of elements  : MJD',F17.8,' TDT (',A,I3,',',
     +       I5,',',F10.6,' h)')

      END
