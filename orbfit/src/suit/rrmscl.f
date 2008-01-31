* Copyright (C) 1999-2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 28, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R R M S C L                           *
*  *                                                               *
*  *            Read from a file and store RMS classes             *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILRA     -  File name (RA classes)
*           FILDEC    -  File name (DEC classes)
*           CLONLY    -  Load only class list, not accuracy description
*                        (this option is enabled when the routine is
*                        used by stat2.f)
*
* WARNING: the correct behaviour of the routine depends strictly on the
* sorting of input data. More precisely:
*   - "per-observatory" RMS classes must be sorted in increasing order
*      of observatory code (field 3);
*   -  "mixed" (all observatories together) classes must be sorted
*      according to the following sorting keys:
*         key 1: accuracy class (field 1)
*         key 2: type of observation (field 2)
*         key 3: starting time (field 4)
*
      SUBROUTINE rrmscl(filra,fildec,clonly)
      IMPLICIT NONE

      CHARACTER*(*) filra,fildec
      LOGICAL clonly

      INCLUDE 'parobc.h'
      INCLUDE 'parrms.h'

* Common blocks to be initialized:
      INCLUDE 'rmscl.h'

      INTEGER ic,i,unit,n,obsc,obscp,nin,lf
      INTEGER n1,n2,n3
      DOUBLE PRECISION mjde1,mjde2,rms1,ave1,rms1a
      CHARACTER crmad1*5,crmod1*1,cobsc*3,file*100,rec*120
      LOGICAL mixobs,new1,new2

      INTEGER lench
      DOUBLE PRECISION tjm1
      EXTERNAL lench,tjm1

      DO 10 ic=1,2
      IF(ic.EQ.1) THEN
          file=filra
      ELSE
          file=fildec
      END IF
      lf=lench(file)
      CALL filopl(unit,file)

* Check file format (in order to avoid using files written according
* to old format, not containing bias information)
      READ(unit,102) rec
 102  FORMAT(A)
      IF(lench(rec).LT.110) THEN
          WRITE(*,200) file(1:lf)
          STOP '**** rrmscl: abnormal end ****'
      END IF
 200  FORMAT('**** ERROR: file "',A,'" is written in an obsolete ',
     +       'format: please use a more recente version ****')
      REWIND(unit)

* Initializations (section 1)
      ncrm(ic)=0
      DO 1 i=obsc1,obsc2
      crmobp(1,i,ic)=0
      crmobp(2,i,ic)=0
 1    CONTINUE
      nin=0
      n=1
      obscp=obsc1-999

* Initializations (section 2)
      n1=0
      n2=0
      n3=0

* Start reading loop
 2    CONTINUE
      IF(clonly) THEN
          READ(unit,100,END=3) crmad1,crmod1,cobsc,mjde1,mjde2
          rms1=0
          ave1=0
          rms1a=0
      ELSE
          READ(unit,100,END=3) crmad1,crmod1,cobsc,mjde1,mjde2,
     +                         rms1,ave1,rms1a
      END IF
 100  FORMAT(A5,1X,A1,1X,A3,1X,F8.1,1X,F8.1,1X,F9.3,F11.5,F9.3,
     +       E11.3,F9.3,2I8,F7.2,I9)
      nin=nin+1
      IF(crmod1.EQ.' ') crmod1='P'

      mixobs=(cobsc.EQ.'ALL')

      IF(mixobs) THEN
          n3=n3+1
          IF(n3.GT.crx3nx) STOP '**** rrmscl: n3 > crx3nx ****'

* Check for change in accuracy descriptor
          IF(n1.LE.0) THEN
              new1=.true.
          ELSE
              new1=(crmad1.NE.crx1ad(n1,ic))
          END IF
          IF(new1) THEN
              n1=n1+1
              IF(n1.GT.crx1nx) STOP '**** rrmscl: n1 > crx1nx ****'
              n2=n2+1
              IF(n2.GT.crx2nx) STOP '**** rrmscl: n2 > crx2nx ****'
              crx1ad(n1,ic)=crmad1
              crx1pt(1,n1,ic)=n2
              crx1pt(2,n1,ic)=n2
              crx2ty(n2,ic)=crmod1
              crx2pt(1,n2,ic)=n3
          ELSE

* Check for change in observation type
              IF(n2.LE.0) THEN
                  new2=.true.
              ELSE
                  new2=(crmod1.NE.crx2ty(n2,ic))
              END IF
              IF(new2) THEN
                  n2=n2+1
                  IF(n2.GT.crx2nx) STOP '**** rrmscl: n2 > crx2nx ****'
                  crx1pt(2,n1,ic)=n2
                  crx2ty(n2,ic)=crmod1
                  crx2pt(1,n2,ic)=n3
              END IF
          END IF
          crx2pt(2,n2,ic)=n3
          crx3t(1,n3,ic)=mjde1
          crx3t(2,n3,ic)=mjde2
          crx3r(n3,ic)=rms1
          crx3a(n3,ic)=ave1
          crx3ra(n3,ic)=rms1a
      ELSE
          IF(n.GT.ncrmx) STOP '**** rrmscl: ncrm > ncrmx ****'
          READ(cobsc,101) obsc
          IF(obsc.LT.obsc1 .OR. obsc.GT.obsc2)
     +        STOP '**** rrmscl: input error (01) ****'
          crmad(n,ic)=crmad1
          crmotd(n,ic)=crmod1

* Integer codification of type descriptor
          crmoti(n,ic)=1000+ICHAR(crmotd(n,ic))
          crmot1(n,ic)=mjde1
          crmot2(n,ic)=mjde2
          crmrms(n,ic)=rms1
          crmave(n,ic)=ave1
          crmrma(n,ic)=rms1a

          IF(obsc.NE.obscp) THEN
              IF(crmobp(1,obsc,ic).NE.0)
     +            STOP '**** rrmscl: input error (02) ****'
              crmobp(1,obsc,ic)=n
              obscp=obsc
          END IF
          crmobp(2,obsc,ic)=n
          n=n+1
      END IF
 101  FORMAT(I3)

      GOTO 2

 3    CONTINUE
      ncrm(ic)=n-1
      crx1n(ic)=n1
      crx2n(ic)=n2
      crx3n(ic)=n3
      CALL filclo(unit,' ')
 10   CONTINUE

      iiccrm=36

      END
