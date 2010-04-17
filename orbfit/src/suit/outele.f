* Copyright (C) 1997-2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 7, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         O U T E L E                           *
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
*
      SUBROUTINE outele(unit,elem,eltype,t0,label,multi,stdout)
      IMPLICIT NONE

      INTEGER unit
      DOUBLE PRECISION elem(6),t0
      CHARACTER*(*) eltype,label
      LOGICAL multi,stdout

      INCLUDE 'trig.h'

      INTEGER lt,i,day,month,year,ll
      CHARACTER cm*3
      DOUBLE PRECISION hour

      INTEGER lench
      CHARACTER*3 chmon
      EXTERNAL lench,chmon

      ll=lench(label)

      IF(multi .AND. (ll.GT.0)) THEN
          IF(unit.GT.0) WRITE(unit,133) label(1:ll)
          IF(stdout) write(99,133) label(1:ll)
      END IF
      IF(eltype.EQ.'KEP') THEN
          IF(multi) THEN
              IF(unit.GT.0) WRITE(unit,100) elem(1),elem(2),
     +                                      (elem(i)*degrad,i=3,6)
              IF(stdout) write(99,100) elem(1),elem(2),
     +                                (elem(i)*degrad,i=3,6)
          ELSE
              IF(ll.GT.0) THEN
                  IF(unit.GT.0) WRITE(unit,120) label(1:ll),elem(1),
     +                                          elem(2),
     +                                         (elem(i)*degrad,i=3,6),t0
                  IF(stdout) write(99,120) label(1:ll),elem(1),elem(2),
     +                                    (elem(i)*degrad,i=3,6),t0
              ELSE
                  IF(unit.GT.0) WRITE(unit,130) elem(1),elem(2),
     +                                         (elem(i)*degrad,i=3,6),t0
                  IF(stdout) WRITE(unit,130) elem(1),elem(2),
     +                                       (elem(i)*degrad,i=3,6),t0
              END IF
          END IF
      ELSEIF(eltype.EQ.'EQU') THEN
          IF(multi) THEN
              IF(unit.GT.0) WRITE(unit,101) (elem(i),i=1,5),
     +                                      elem(6)*degrad
              IF(stdout) write(99,101) (elem(i),i=1,5),elem(6)*degrad
          ELSE
              IF(ll.GT.0) THEN
                  IF(unit.GT.0) WRITE(unit,121) label(1:ll),
     +                                          (elem(i),i=1,5),
     +                                          elem(6)*degrad,t0
                  IF(stdout) write(99,121) label(1:ll),(elem(i),i=1,5),
     +                                    elem(6)*degrad,t0
              ELSE
                  IF(unit.GT.0) WRITE(unit,131) (elem(i),i=1,5),
     +                                          elem(6)*degrad,t0
                  IF(stdout) write(99,131) (elem(i),i=1,5),
     +                                    elem(6)*degrad,t0
              END IF
          END IF
      ELSEIF(eltype.EQ.'CAR') THEN
          IF(multi) THEN
              IF(unit.GT.0) WRITE(unit,102) elem
              IF(stdout) write(99,102) elem
          ELSE
              IF(ll.GT.0) THEN
                  IF(unit.GT.0) WRITE(unit,122) label(1:ll),elem,t0
                  IF(stdout) write(99,122) label(1:ll),elem,t0
              ELSE
                  IF(unit.GT.0) WRITE(unit,132) elem,t0
                  IF(stdout) write(99,132) elem,t0
              END IF
          END IF
      ELSE
          lt=lench(eltype)
          write(99,200) eltype(1:lt)
          STOP '**** outele: abnormal end ****'
      END IF
 120  FORMAT(8X,'KepElem(',A,'):',1P,E15.7,0P,F13.8,4F10.5,
     +           ' (T=',F10.3,')')
 130  FORMAT(8X,'KepElem:',1P,E15.7,0P,F13.8,4F10.5,
     +           ' (T=',F10.3,')')
 121  FORMAT(8X,'EQUElem(',A,'):',1P,E15.7,0P,4F13.8,F10.5,
     +           ' (T=',F10.3,')')
 131  FORMAT(8X,'EQUElem:',1P,E15.7,0P,4F13.8,F10.5,' (T=',F10.3,')')
 122  FORMAT(8X,'PosVel(',A,'):',1P,6E15.7,' (T=',F10.3,')')
 132  FORMAT(8X,'PosVel:',1P,6E15.7,' (T=',F10.3,')')
 100  FORMAT(8X,'Semimajor axis     =',1P,E23.14,0P,' AU'/
     +       8X,'Eccentricity       =',F20.15/
     +       8X,'Inclination        =',F18.13,' deg'/
     +       8X,'Long. of node      =',F18.13,' deg'/
     +       8X,'Arg. of pericenter =',F18.13,' deg'/
     +       8X,'Mean anomaly       =',F18.13,' deg')
 101  FORMAT(8X,'Semimajor axis     =',1P,E23.14,0P,' AU'/
     +       8X,'h [e*sin(w)]       =',F20.15/
     +       8X,'k [e*cos(w)]       =',F20.15/
     +       8X,'P [tg(i/2)*sin(N)] =',F20.15/
     +       8X,'Q [tg(i/2)*cos(N)] =',F20.15/
     +       8X,'Mean longitude     =',F18.13,' deg')
 102  FORMAT(8X,'Position vector   =',1X,1P,3E22.14,' AU'/
     +       8X,'Velocity vector   =',1X,3E22.14,' AU/d')
 200  FORMAT('ERROR: unknown type "',A,'" of orbital elements')
 133  FORMAT(8X,'Orbital elements for ',A,':')

      IF(multi) THEN
          CALL mjddat(t0,day,month,year,hour)
          cm=chmon(month)
          WRITE(unit,110) t0,cm,day,year,hour
          IF(stdout) write(99,110) t0,cm,day,year,hour
      END IF
 110  FORMAT(8X,'Epoch of elements  : MJD',F17.8,' TDT (',A,I3,',',
     +       I5,',',F10.6,' h)')

      END
