* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 3, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         O B S R M S                           *
*  *                                                               *
*  *         Computation of a-priori RMS of observations           *
*  *    from their accuracy (= number of digits in input file)     *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    IOBS      -  Observation type: 1000+x=astrometry 
*                        2000+x radar 3000+x satellite 
*           IDSTA     -  Observatory code
*           TDT       -  Time (MJD, TDT)
*           ACCT      -  Accuracy of time (d)
*           ACCA      -  Accuracy of right ascension (rad)
*           ACCD      -  Accuracy of declination (rad)
*           SMAG      -  Apparent magnitude and color (string)
*           N         -  Number of observations
*
* OUTPUT:   RMSA      -  A-priori RMS of right ascension (rad)
*           RMSD      -  A-priori RMS of declination (rad)
*           RMSMAG    -  A-priori RMS of magnitude (magn)
*
      SUBROUTINE obsrms(iobs,idsta,tdt,acct,acca,accd,smag,
     +           rmsa,rmsd,rmsmag,n)
      IMPLICIT NONE

      INTEGER n,idsta(n),iobs(n)
      CHARACTER*6 smag(n)
      DOUBLE PRECISION tdt(n),acct(n),acca(n),accd(n),
     +              rmsa(n),rmsd(n),rmsmag(n)

      INTEGER i
      DOUBLE PRECISION rmsrmi,rmsvmi
c Get default magnitude weights from a function
      DOUBLE PRECISION magrms
      CHARACTER*1 typ

c      INCLUDE 'trig.h'
      INCLUDE 'jplhdr.h'
      DO 1 i=1,n
         IF(iobs(i)/1000.eq.1)THEN
c astrometric (telescope) observations; get weight
            typ=char(iobs(i)-1000)
            CALL astrow(typ,tdt(i),idsta(i),acca(i),accd(i),
     +               rmsa(i),rmsd(i))
c magnitude weigthing
            rmsmag(i)=magrms(smag(i),tdt(i),idsta(i),typ)
         ELSEIF(iobs(i)/1000.eq.2)THEN
c weighting of radar data is given with observations, but there is 
c minimum credible (for a given acccuracy of the models)
            rmsrmi=0.0d0/au
            rmsvmi=0.0d0/au
            rmsa(i)=MAX(acca(i),rmsrmi)
            rmsd(i)=MAX(accd(i),rmsvmi)
c            rmsa(i)=acca(i)
c            rmsd(i)=accd(i)
            rmsmag(i)=-1.d0
         ELSE
            write(99,*)'obsrms: obs. type not known ', 
     +           tdt(i),' ',iobs(i),' ',i
            STOP
        ENDIF 
 1    CONTINUE
      END
