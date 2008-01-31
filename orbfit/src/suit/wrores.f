* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 7, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         W R O R E S                           *
*  *                                                               *
*  *       Writes a-priori standard deviation of observations      *
*  *                       and fit residuals                       *
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
*           RESA      -  Residuals in right ascension (rad)
*           RESD      -  Residuals in declination (rad)
*           N         -  Number of observations
*
      SUBROUTINE wrores(file,tutm,obscod,rmsa,rmsd,sel,resa,resd,n)
      IMPLICIT NONE

      CHARACTER*(*) file
      INTEGER n,obscod(n),sel(n)
      DOUBLE PRECISION tutm(n),rmsa(n),rmsd(n),resa(n),resd(n)

      INCLUDE 'trig.h'
      INCLUDE 'parcmc.h'

      INTEGER unit,i,iday,month,year
      DOUBLE PRECISION day,hour,ra,rd
      CALL filopn(unit,file,'UNKNOWN')
      WRITE(unit,110) comcha
 110  FORMAT(A1,'YYYY MM DD.dddddd  OBS   rms(RA)"    rms(DEC)" S',
     +          '    res(RA)"  res(DEC)"')

      DO 1 i=1,n
      CALL mjddat(tutm(i),iday,month,year,hour)
      day=iday+hour/24
      ra=resa(i)*secrad
      rd=resd(i)*secrad
      IF(sel(i).GT.9) STOP '**** wrores: sel > 9 ****'
      IF(MAX(ABS(ra),ABS(rd)).GE.999.D0) THEN
          WRITE(unit,100) year,month,day,obscod(i),
     +                    rmsa(i)*secrad,rmsd(i)*secrad,sel(i),ra,rd
      ELSE
          WRITE(unit,101) year,month,day,obscod(i),
     +                    rmsa(i)*secrad,rmsd(i)*secrad,sel(i),ra,rd
      END IF
 100  FORMAT(I5,I3,F10.6,I5,1P,2E12.4,I2,1P,2E11.3)
 101  FORMAT(I5,I3,F10.6,I5,1P,2E12.4,I2,0P,2F11.3)
 1    CONTINUE

      CALL filclo(unit,' ')

      END
