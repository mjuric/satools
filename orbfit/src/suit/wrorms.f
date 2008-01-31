* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 7, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R O R M S                           *
*  *                                                               *
*  *       Writes a-priori standard deviation of observations      *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  Output file name
*           TUTM      -  Time (MJD, UTM)
*           OBSCOD    -  Observatory code
*           RMSA      -  A-priori RMS of right ascension (rad)
*           RMSD      -  A-priori RMS of declination (rad)
*           SEL       -  Selection indicator (0=don't use; 1=use for fit;
*                        2=use for fit & Gauss method)
*           N         -  Number of observations
*
      SUBROUTINE wrorms(file,tutm,obscod,rmsa,rmsd,sel,n)
      IMPLICIT NONE

      CHARACTER*(*) file
      INTEGER n,obscod(n),sel(n)
      DOUBLE PRECISION tutm(n),rmsa(n),rmsd(n)

      INCLUDE 'trig.h'
      INCLUDE 'parcmc.h'

      INTEGER unit,i,iday,month,year
      DOUBLE PRECISION day,hour

      CALL filopn(unit,file,'UNKNOWN')
      WRITE(unit,101) comcha
 101  FORMAT(A1,'YYYY MM DD.dddddd  OBS   rms(RA)"    rms(DEC)" S')

      DO 1 i=1,n
      CALL mjddat(tutm(i),iday,month,year,hour)
      day=iday+hour/24
      IF(sel(i).GT.9) STOP '**** wrorms: sel > 9 ****'
      WRITE(unit,100) year,month,day,obscod(i),
     +                rmsa(i)*secrad,rmsd(i)*secrad,sel(i)
 100  FORMAT(I5,I3,F10.6,I5,1P,2E12.4,I2)
 1    CONTINUE

      CALL filclo(unit,' ')

      END
