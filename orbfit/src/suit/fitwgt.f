* Copyright (C) 1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: January 15, 1998; corrected AM/ZK March 16, 1998
* Modified 2 Dec. 1998 by Steven Chesley
* Changes: make optional to set w=0, correct for spherical metric
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         F I T W G T                           *
*  *                                                               *
*  *      Computation of observation weights for orbital fit       *
*  *                   from their a-priori RMS                     *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    RMSA      -  A-priori RMS of right ascension (rad)
*           RMSD      -  A-priori RMS of declination (rad)
*           DEL       -  Declination
*           SEL       -  Selection index
*           IOBS      -  Observation type: 2002,2003 are 1-dim
*           N         -  Number of observations
*           DISCARD   -  TRUE to set w=0 if sel=0
*
* OUTPUT:   W         -  Weights
*
      SUBROUTINE fitwgt(rmsa,rmsd,del,sel,iobs,w,n,discard)
      IMPLICIT NONE

      INTEGER n,sel(n),iobs(n)
      DOUBLE PRECISION rmsa(n),rmsd(n),del(n),w(2*n)
      LOGICAL discard

      INTEGER i

      DO i=1,n
         IF(sel(i).EQ.0 .AND. discard) THEN
            w(2*i-1)=0
            w(2*i)=0
         ELSEIF(iobs(i)/1000.eq.1)THEN
            w(2*i-1)=(cos(del(i))/rmsa(i))**2
            w(2*i)=1.d0/rmsd(i)**2
         ELSEIF(iobs(i).eq.2001.or.iobs(i).eq.2101)THEN
            w(2*i-1)=1.d0/rmsa(i)**2
            w(2*i)=1.d0/rmsd(i)**2
         ELSEIF(iobs(i).eq.2002.or.iobs(i).eq.2102)THEN
            w(2*i-1)=1.d0/rmsa(i)**2
            w(2*i)=0.d0
         ELSEIF(iobs(i).eq.2003.or.iobs(i).eq.2103)THEN
            w(2*i-1)=0.d0
            w(2*i)=1.d0/rmsd(i)**2
         END IF
      ENDDO

      END
