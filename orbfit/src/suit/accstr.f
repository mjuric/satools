* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 3, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         A C C S T R                           *
*  *                                                               *
*  *               Accuracy description (string)                   *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    ACCA      -  Accuracy of right ascension (rad)
*           ACCD      -  Accuracy of declination (rad)
*
* OUTPUT:   ADSA      -  Accuracy description string (RA)
*           ADSD      -  Accuracy description string (DEC)
*           ERROR     -  Error flag
*
      SUBROUTINE accstr(acca,accd,adsa,adsd,error)
      IMPLICIT NONE

      DOUBLE PRECISION acca,accd
      CHARACTER*(*) adsa,adsd
      LOGICAL error

      INCLUDE 'trig.h'

      DOUBLE PRECISION accs
      INTEGER i,k,lads
      CHARACTER*10 ads

      error=.false.

      DO 1 i=1,2
      IF(i.EQ.1) THEN
          accs=acca*secrad/15.d0
      ELSE
          accs=accd*secrad
      END IF
      WRITE(ads,101) accs
 101  FORMAT(F10.4)
      CALL rmsp(ads,lads)
      DO 2 k=lads,1,-1
      IF(ads(k:k).EQ.'0') THEN
          ads(k:k)=' '
          lads=lads-1
      ELSE
          GOTO 3
      END IF
 2    CONTINUE
 3    CONTINUE
      IF(ads(k:k).EQ.'.') THEN
          ads(k:k)=' '
          lads=lads-1
      END IF
      IF(i.EQ.1) THEN
          IF(lads.GT.LEN(adsa)) THEN
              error=.true.
              WRITE(0,200) 'ADSA',ads(1:lads)
          ELSE
              adsa=ads(1:lads)
          END IF
      ELSE
          IF(lads.GT.LEN(adsd)) THEN
              error=.true.
              WRITE(0,200) 'ADSD',ads(1:lads)
          ELSE
              adsd=ads(1:lads)
          END IF
      END IF
 1    CONTINUE
 200  FORMAT('ERROR (accstr): ',A,' = "',A,'"')

      END
