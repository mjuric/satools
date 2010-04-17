* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 5, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D O R M S                           *
*  *                                                               *
*  *      Read a-priori standard deviation of observations         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  Input file name
* OUPUT:    TUTM      -  Time (MJD, UTM)
*           OBSCOD    -  Observatory code
*           RMSA      -  A-priori RMS of right ascension (rad)
*           RMSD      -  A-priori RMS of declination (rad)
*           SEL       -  Selection index (0=don't use; 1=use for fit;
*                        2=use for fit & Gauss method)
*           N         -  Number of observations
*
* On output, RMSA, RMSD and SEL of observations found in the file
* are updated; if the input file does not exists, returns without
* any change
*
      SUBROUTINE rdorms(file,tutm,obscod,rmsa,rmsd,sel,n)
      IMPLICIT NONE

      CHARACTER*(*) file
      INTEGER n,obscod(n),sel(n)
      DOUBLE PRECISION tutm(n),rmsa(n),rmsd(n)

      INCLUDE 'trig.h'
      INCLUDE 'parcmc.h'

      INTEGER unit,nr,lf,year,month,obsc1,iday,i,sel1
      DOUBLE PRECISION day,ra1,rd1,tutm1,eps
      CHARACTER*200 rec
      LOGICAL found

      INTEGER lench
      DOUBLE PRECISION tjm1
      EXTERNAL lench,tjm1

      DATA eps/5.D-6/

      INQUIRE(FILE=file,EXIST=found)
      IF(.NOT.found) RETURN
      CALL filopn(unit,file,'OLD')
      nr=0

 1    CONTINUE
      READ(unit,100,END=10) rec
 100  FORMAT(A)
      nr=nr+1
      IF(rec(1:1).EQ.comcha) GOTO 1
      READ(rec,*,ERR=20) year,month,day,obsc1,ra1,rd1,sel1
      iday=day
      tutm1=tjm1(iday,month,year,0.d0)+(day-iday)
      DO 3 i=1,n
      IF(obsc1.NE.obscod(i)) GOTO 3
      IF(ABS(tutm1-tutm(i)).GT.eps) GOTO 3
      rmsa(i)=ra1*radsec
      rmsd(i)=rd1*radsec
      sel(i)=sel1
      GOTO 1
 3    CONTINUE
      GOTO 1

 10   CONTINUE
      CALL filclo(unit,' ')
      RETURN

 20   CONTINUE
      lf=lench(file)
      write(99,101) file(1:lf),nr
 101  FORMAT(' Input error (file "',A,'", record',I5,')')
      STOP '**** rdorms: abnormal end ****'

      END
