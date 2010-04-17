* Copyright (C) 1997-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 10, 1998
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                          D E L T T                          *
*  *                                                             *
*  *                   Difference DT = ET - UT                   *
*  *                                                             *
*  ***************************************************************
*
* INPUT:    TJM       -  Modified Julian Day (UT1)
*
* OUTPUT:   DELTT     -  DT = ET - UT1 (in seconds)
*
      DOUBLE PRECISION FUNCTION deltt(tjm)
      IMPLICIT NONE

      DOUBLE PRECISION tjm

* Max number of entries in ET-UT table
      INTEGER nx
      PARAMETER (nx=500)

      DOUBLE PRECISION dt,c1,c2
      DOUBLE PRECISION tv(nx),dtv(nx)
      INTEGER day,month,year,n,ipos,unit,lf,nwmax,nw
      CHARACTER record*10,file*80
      LOGICAL first,found,fail1,fail,pedant,fndfil

      DOUBLE PRECISION tjm1
      INTEGER lench
      EXTERNAL tjm1,lench

      INCLUDE 'timespan.h'

      SAVE first,n,tv,dtv,ipos,file,pedant,nwmax,nw,lf

      DATA first/.true./

* Load table of ET-UT1 as a function of UT1
      IF(first) THEN
          first=.false.
          fail=.false.
          file='ET-UT.dat'
          CALL rdncha('ET-UT.','file',file,.false.,fndfil,fail1,fail)
          lf=lench(file)
          pedant=.false.
          CALL rdnlog('ET-UT.','pedantic',pedant,.false.,found,
     +                fail1,fail)
          nwmax=1
          nw=0
          CALL rdnint('ET-UT.','n_warn',nwmax,.false.,found,
     +                fail1,fail)
          IF(fail) STOP '**** deltt: abnormal end ****'
          IF(fndfil) THEN
              CALL filopn(unit,file,'old')
          ELSE
              CALL filopl(unit,file)
          END IF

 1        CONTINUE
          READ(unit,100,end=11) record
          IF(record.NE.'----------') GOTO 1

          n=0
 2        CONTINUE
          READ(unit,*,end=3) day,month,year,dt
          n=n+1
          IF(n.GT.nx) STOP '**** deltt: n > nx ****'
          tv(n)=tjm1(day,month,year,0.d0)
          dtv(n)=dt
          GOTO 2
 3        CONTINUE
          ipos=1
          CALL filclo(unit,' ')
          IF(n.LT.2) STOP '**** deltt: n < 2 ****'
          temut1=tv(1)
          temut2=tv(n)
          temute=(.NOT.pedant)
      END IF
 100  FORMAT(a)

* Trying to use previous value
      IF(tjm.GE.tv(ipos).AND.tjm.LE.tv(ipos+1)) GOTO 5

* Selecting the records of the table before and after the date
* supplied
      IF(tjm.LT.tv(1)) THEN
          IF(pedant) GOTO 12
          deltt=dtv(1)
          GOTO 30
      END IF
      IF(tjm.GT.tv(n)) THEN
          IF(pedant) GOTO 12
          deltt=dtv(n)
          GOTO 30
      END IF
      DO 4 ipos=1,n-1
          IF(tjm.GE.tv(ipos).AND.tjm.LE.tv(ipos+1)) GOTO 5
 4    CONTINUE
      STOP '**** deltt: internal error (01) ****'

* Interpolation
 5    CONTINUE
      c1=(tv(ipos+1)-tjm)/(tv(ipos+1)-tv(ipos))
      c2=1-c1
      deltt=c1*dtv(ipos)+c2*dtv(ipos+1)

      RETURN

*  Error termination
 11   CONTINUE
      STOP '**** deltt: no DATA found in input file ****'

 12   CONTINUE
      write(99,101) tjm,file(1:lf),tv(1),tv(n)
 101  FORMAT(' **** deltt: TJM out of range ****'/
     +       ' Cannot compute ET-UT at TJM =',F11.3/
     +       ' File "',A,'" starts at TJM =',F11.3/
     +       ' and ends at TJM =',F11.3)
      STOP '**** deltt: abnormal end ****'

  30  CONTINUE
      nw=nw+1
      IF(nw.GT.nwmax) RETURN
      write(99,102) tjm,file(1:lf),tv(1),tv(n)
 102  FORMAT(' WARNING (deltt): TJM =',F11.3,
     +       ' is outside the range of data'/
     +       ' contained in file "',A,'" (from',F11.3,' to',F11.3,')')

      END
