* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 17, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         I E R I N I                           *
*  *                                                               *
*  *             Initialization of routine IERSTS                  *
*  *                                                               *
*  *****************************************************************
*
      SUBROUTINE ierini
      IMPLICIT NONE

      INCLUDE 'parier.h'

* Common blocks to be initialized:
      INCLUDE 'cmiers.h'

* NEEDED common blocks:
      INCLUDE 'comlib.h'

      DOUBLE PRECISION x,y,dt1,dlod,dpsi,deps,dt,tjme,du,dud,dudd,dtsec
      DOUBLE PRECISION t0,tn,c0,c1,c2
      INTEGER unilis,uniier,unicie,year,nr,lf,lr,day,mjd,mjdp,mjdc
      INTEGER month,npt,i,k,itint1,itint2,niers1,k1
      INTEGER nu,na,ni,ne1,ne2,mjd1,mjd2,kamin
      CHARACTER ierfil*80,file*150,rec*80,cm3*3,cval*50
      LOGICAL found,fail1,fail,first

      INCLUDE 'etmtai.h'

      INTEGER lench,itaiut,chmo2i
      DOUBLE PRECISION tjm1
      EXTERNAL lench,itaiut,chmo2i,tjm1

      IF(iiclib.NE.36) STOP '**** ierini: internal error (01) ****'

      npt=0
      fail=.false.

* Input of options
* Directory where input IERS EOPC04 files are located
      ieidir=libdir(1:lenld)//'eopc04/'
      CALL rdncha('IERS.eopc04.','dir',ieidir,.false.,found,fail1,fail)
      ieidil=lench(ieidir)
* File containing the list of IERS files to be used
      ieilis=libdir(1:lenld)//'eopc04.lis'
      CALL rdncha('IERS.eopc04.','list',ieilis,.false.,found,fail1,fail)
      ieilil=lench(ieilis)
* Smoothing to be used
      iutsmo=0
      CALL sv2int('IERS.','smoothing',cval,iutsmo,.false.,
     +            found,fail1,fail)
* Undersampling ratio
      isamp=1
      CALL rdnint('IERS.','sampling',isamp,.false.,found,fail1,fail)
* Apply consistency correction
      cciera=.true.
      CALL rdnlog('IERS.ccor.','use',cciera,.false.,found,fail1,fail)
* Name of consistency correction file
      IF(cciera) THEN
          flcier='eopc04.cnc'
          CALL rdncha('IERS.ccor.','file',flcier,.false.,
     +                found,fail1,fail)
      ELSE
          flcier=' '
      END IF
* Interpolation length for pcwlgi
      nlpler=15
      CALL rdnint('IERS.','lint',nlpler,.false.,found,fail1,fail)
* Length of empty zone for pcwlgi
      nvpler=3
      CALL rdnint('IERS.','lez',nvpler,.false.,found,fail1,fail)
* Length of superposition zone for pcwlgi
      nspler=3
      CALL rdnint('IERS.','lsz',nspler,.false.,found,fail1,fail)
* Order of smoothing for pcwlgi
      nsmopl=6
      CALL rdnint('IERS.','smord',nsmopl,.false.,found,fail1,fail)
* Use of Bulletin A (EOP predictions into the future)
      blause=.false.
      CALL rdnlog('IERS.bulA.','use',blause,.false.,found,fail1,fail)
      IF(blause) THEN
          blafil='bulletinA'
          CALL rdncha('IERS.bulA.','file',blafil,.false.,
     +                found,fail1,fail)
      END IF
* Allow extrapolation
      extra=.false.
      CALL rdnlog('IERS.','extrapolation',extra,.false.,
     +            found,fail1,fail)

* Check on input values
      nu=nlpler-2*(nvpler+nspler)
      na=nspler+nu
      IF(na.LT.1) THEN
          write(99,220) na
          STOP '**** ierini: abnormal end ****'
      END IF
 220  FORMAT('ERROR: bad parameter definition (IERS.eopc04.)'/
     +       '       Please supply values such that lint-2*lez-lsz > 0'/
     +       '       (present value is lint-2*lez-lsz =',I3)

      IF(fail) STOP '*** ierini: abnormal end ****'

* Reserve space for extention records before beginning of data
      IF(extra) THEN
          kamin=2+nvpler-nu
          k=kamin/na
          IF(k*na.LT.kamin) k=k+1
          IF(k*na.LT.kamin) STOP '**** ierini: internal error (02) ****'
          IF(k.LT.0) STOP '**** ierini: internal error (03) ****'
          k=MAX(2,k)
          ne1=(k+1)*na
      ELSE
          ne1=0
      END IF
      niers=ne1
      first=.true.

* Loop on IERS files
      CALL filopn(unilis,ieilis,'OLD')
 1    CONTINUE
      READ(unilis,100,END=10) ierfil
 100  FORMAT(A)
      file=ieidir(1:ieidil)//ierfil
      lf=lench(file)

* Scan IERS file
      CALL filopn(uniier,file,'OLD')
      nr=0

* Skip file header and read the year
 2    CONTINUE
      nr=nr+1
      READ(uniier,100,ERR=20) rec
      IF(rec(1:10).NE.'  YEAR ==>') GOTO 2
      READ(rec(11:),*) year
      IF(year.LT.1900 .OR. year.GT.2100) GOTO 20

* Read and store data records
 3    CONTINUE
      nr=nr+1
      READ(uniier,100,ERR=20,END=9) rec
      lr=lench(rec)
      IF(lr.LE.0) GOTO 3
      READ(rec,101,ERR=20) cm3,day,mjd,x,y,dt1,dlod,dpsi,deps
 101  FORMAT(2X,A3,1X,I3,2X,I5,2F9.5,F10.6,2X,F10.6,2X,2F9.5)
* Check on MJD value
      month=chmo2i(cm3)
      IF(month.LE.0) GOTO 20
      mjdc=NINT(tjm1(day,month,year,0.D0))
      IF(mjdc.NE.mjd) THEN
          write(99,203) mjdc,mjd
          GOTO 20
      END IF
 203  FORMAT('ERROR: inconsistent MJD:',2I8)
      IF(niers.GT.ne1) THEN
          IF(mjd-mjdp.NE.1) THEN
              write(99,202) mjd,mjdp
              GOTO 20
          END IF
      END IF
 202  FORMAT('WRONG time sequence:',2I7)
      mjdp=mjd
      npt=npt+1
      IF(MOD(npt,isamp).NE.0) GOTO 3
* TDT-UT1 = (TDT-TAI) + (TAI-UTC) + (UTC-UT1)
      dtsec=etmtai+itaiut(mjd)
      dt=dtsec-dt1
* Transformation TJM(UTC) -> TJM(TDT)
* TDT ( = ET ) = (ET-TAI) + (TAI-UTC) + UTC
      tjme=mjd+dtsec/86400.d0

* Applying required smoothing to TDT-UT1
      IF(iutsmo.eq.0) THEN
          CONTINUE
      ELSEIF(iutsmo.EQ.1) THEN
          CALL dut1r(tjme,du,dud,dudd,0)
          dt=dt+du
      ELSEIF(iutsmo.EQ.2) THEN
          CALL dut1s(tjme,du,dud,dudd,0)
          dt=dt+du
      ELSE
          STOP '**** ierini: internal error (04) ****'
      END IF
* Storing values in arrays
      niers=niers+1
      IF(niers.GT.niersx) STOP '**** ierini: niers > niersx ****'
      IF(first) THEN
          first=.false.
          mjd1=mjd
      END IF
      mjd2=mjd
      tiers(niers)=tjme
      xiers(niers,1)=x
      xiers(niers,2)=y
      xiers(niers,3)=dt
      xiers(niers,4)=dpsi
      xiers(niers,5)=deps
      GOTO 3

 9    CONTINUE
      CALL filclo(uniier,' ')

* End of loop on IERS files
      GOTO 1
 10   CONTINUE
      CALL filclo(unilis,' ')

      IF(niers.LE.0) STOP '**** ierini: empty EOPC04 files ****'
      niers1=niers
      write(99,201) niers,mjd1,mjd2
 201  FORMAT('Reading IERS time series:'/
     +       '        EOPC04 data:',I6,' points (from MJD=',
     +       I5,' to MJD=',I5,')')

* Reading predictions from IERS Bulletin A
      IF(blause) THEN
          CALL rdbula(blafil,mjd2,tiers,xiers,niers,niersx,isamp,npt,
     +                iutsmo)
      END IF

* Number of extension records after end of data
      IF(extra) THEN
          ni=(niers-ne1-nlpler)/na
          k=nlpler-nspler-nvpler+(ni-1)*na
          ne2=mjd2-mjd1-k+2*na
          ne2=MAX(2,ne2)
      ELSE
          ne2=0
      END IF

* Fill extension records
      IF(extra) THEN
          dutd=(xiers(niers,3)-xiers(ne1+1,3))/
     +                        (tiers(niers)-tiers(ne1+1))
          itint1=mjd1+(nvpler+nspler-ne1)*isamp
          dtsec=etmtai+itaiut(itint1)
          tint1=itint1+dtsec/86400.d0
          dtsec=etmtai+itaiut(mjd1)
          t0=mjd1+dtsec/86400.d0
          utd1=xiers(ne1+1,3)+dutd*(tint1-tiers(ne1+1))
          DO 11 i=1,ne1
          k=ne1+1-i
          mjd=mjd1-k*isamp
          dtsec=etmtai+itaiut(mjd)
          tjme=mjd+dtsec/86400.d0
          tiers(i)=tjme
          IF(mjd.LT.itint1) THEN
              c0=0.d0
          ELSE
              tn=(tjme-tint1)/(t0-tint1)
              CALL smoocn(tn,c0,c1,c2,2)
          END IF
          DO 13 k=1,5
          xiers(i,k)=c0*xiers(1,k)
 13       CONTINUE
          xiers(i,3)=utd1+dutd*(tjme-tint1)
 11       CONTINUE
          IF(niers+ne2.GT.niersx)
     +        STOP '**** ierini: niers > niersx ****'

          ni=(niers+ne2-ne1-nlpler)/na
          k=nlpler-nspler-nvpler+(ni-1)*na
          itint2=mjd1+k*isamp
          dtsec=etmtai+itaiut(itint2)
          tint2=itint2+dtsec/86400.d0
          dtsec=etmtai+itaiut(mjd2)
          t0=mjd2+dtsec/86400.d0
          utd2=xiers(niers,3)+dutd*(tint2-tiers(niers))

          DO 12 i=1,ne2
          mjd=mjd2+i*isamp
          dtsec=etmtai+itaiut(mjd)
          tjme=mjd+dtsec/86400.d0
          k=niers+i
          k1=niers1+i
          IF(mjd.GT.itint2) THEN
              c0=0.d0
          ELSE
              tn=(tjme-tint2)/(t0-tint2)
              CALL smoocn(tn,c0,c1,c2,2)
          END IF
          tiers(k)=tjme
          xiers(k,1)=c0*xiers(niers,1)
          xiers(k,2)=c0*xiers(niers,2)
          xiers(k,3)=utd2+dutd*(tjme-tint2)
          xiers(k1,4)=c0*xiers(niers1,4)
          xiers(k1,5)=c0*xiers(niers1,5)
 12       CONTINUE
          niers=niers+ne2
          IF(ni.LT.0) STOP '**** ierini: internal error (05) ****'
      END IF

* Read consistency corrections
      IF(cciera) THEN
          CALL filopl(unicie,flcier)
 4        READ(unicie,100) rec
          IF(rec(1:5).NE.'-----') GOTO 4
          READ(unicie,*) cncor0
          READ(unicie,*) cncor1
          READ(unicie,*) cnep0
          CALL filclo(unicie,' ')
* Unit conversion
          cncor0(1)=cncor0(1)/1.d3
          cncor0(2)=cncor0(2)/1.d3
          cncor0(3)=cncor0(3)/1.d4
          cncor0(4)=cncor0(4)/1.d3
          cncor0(5)=cncor0(5)/1.d3
          cncor1(1)=cncor1(1)/1.d3
          cncor1(2)=cncor1(2)/1.d3
          cncor1(3)=cncor1(3)/1.d4
          cncor1(4)=cncor1(4)/1.d3
          cncor1(5)=cncor1(5)/1.d3
      END IF

* Definition of max interpolation error allowed for pcwlgi
      DO 5 i=1,5
      rmserx(i)=0.d0
 5    CONTINUE

      DO 6 k=1,niers
      DO 6 i=1,5
      rmserx(i)=rmserx(i)+xiers(k,i)**2
 6    CONTINUE

      DO 7 i=1,5
      rmserx(i)=1.D-11*SQRT(rmserx(i)/niers)
 7    CONTINUE

      iicier=36

      RETURN

* Error termination
 20   CONTINUE
      write(99,200) file(1:lf),nr
 200  FORMAT('ERROR in reading file "',A,'" at record ',I5)
      STOP '**** ierini: abnormal end ****'

      END
