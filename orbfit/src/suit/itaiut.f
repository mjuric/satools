* Copyright (C) 1996-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 9, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         I T A I U T                           *
*  *                                                               *
*  *      Difference DAT = TAI - UTC as a function of UTC          *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    MJDC      -  Modified Julian Day (UTC)
*
* OUTPUT:   Difference DAT = TAI - UTC given as an integer number
*           of seconds
*
      INTEGER FUNCTION itaiut(mjdc)
      IMPLICIT NONE

      INCLUDE 'partai.h'

      INTEGER mjdc

      INTEGER mjdv(ntaix),idv(ntaix)
      CHARACTER rec*10,file*80
      INTEGER unit,n,day,month,year,iii,jp,jpm1,lf,j,nwmax,nw
      LOGICAL first,found,fail1,fail,pedant,fndfil

      INTEGER lench
      DOUBLE PRECISION tjm1
      EXTERNAL lench,tjm1

      SAVE mjdv,idv,first,n,jp,jpm1,file,pedant,nwmax,nw,lf

      DATA first/.true./

      IF(first) THEN
          fail=.false.
          file='TAI-UTC.dat'
          CALL rdncha('TAI-UTC.','file',file,.false.,fndfil,fail1,fail)
          lf=lench(file)
          pedant=.false.
          CALL rdnlog('TAI-UTC.','pedantic',pedant,.false.,found,
     +                fail1,fail)
          nwmax=1
          nw=0
          CALL rdnint('TAI-UTC.','n_warn',nwmax,.false.,found,
     +                fail1,fail)
          IF(fail) STOP '**** itaiut: abnormal end ****'
          IF(fndfil) THEN
              CALL filopn(unit,file,'old')
          ELSE
              CALL filopl(unit,file)
          END IF

* Skipping header
 1        READ(unit,100,end=10) rec
          IF(rec.NE.'----------') GOTO 1

          n=0
 2        CONTINUE
          READ(unit,*,END=3) day,month,year,iii
          n=n+1
          IF(n.GT.ntaix) STOP '**** itaiut: n > ntaix ****'
          idv(n)=iii
          mjdv(n)=NINT(tjm1(day,month,year,0.d0))
          IF(n.GT.1.AND.mjdv(n).LE.mjdv(n-1))
     +        STOP '**** itaiut: input file is not sorted ****'
          GOTO 2
 3        CONTINUE
          CALL filclo(unit,' ')
          IF(n.LT.2) GOTO 10

          jp=2
          jpm1=jp-1

          first=.false.
      END IF
 100  FORMAT(a)

* Trying to use previous value
      IF(mjdc.GE.mjdv(jpm1) .AND. mjdc.LT.mjdv(jp)) THEN
          itaiut=idv(jpm1)
          RETURN
      END IF

* Check limits
      IF(mjdc.LT.mjdv(1)) THEN
          IF(pedant) GOTO 20
          itaiut=idv(1)
          GOTO 30
      END IF
      IF(mjdc.GE.mjdv(n)) THEN
          IF(pedant) GOTO 20
          itaiut=idv(n)
          GOTO 30
      END IF

* Selecting new value
      DO 4 j=2,n
      IF(mjdc.LT.mjdv(j)) THEN
          jp=j
          jpm1=jp-1
          itaiut=idv(jpm1)
          RETURN
      END IF
 4    CONTINUE

      STOP '**** itaiut: internal error (01) ****'

 10   CONTINUE
      STOP '**** itaiut: input file is empty ****'

 20   CONTINUE
      write(99,101) mjdc,file(1:lf),mjdv(1),mjdv(n)-1
 101  FORMAT(' **** itaiut: mjdc out of range ****'/
     +       '      Cannot compute TAI-UTC at MJD =',i6/
     +       '      File "',a,'" starts at MJD =',i6/
     +       '      and ends at MJD =',i6)
      STOP '**** itaiut: abnormal end ****'

 30   CONTINUE
      nw=nw+1
      IF(nw.GT.nwmax) RETURN
      write(99,102) mjdc,file(1:lf),mjdv(1),mjdv(n)-1
 102  FORMAT(' WARNING (itaiut): MJD =',I6,
     +       ' is outside the range of data'/
     +       ' contained in file "',A,'" (from',I6,' to',I6,')')

      END
