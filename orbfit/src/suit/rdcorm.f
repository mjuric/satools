* Copyright (C) 2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 28, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D C O R M                           *
*  *                                                               *
*  *   Read the autocorrelation models for all the observatories   *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  Input file
*
* OUTPUT:   
*
      SUBROUTINE rdcorm(file)
      IMPLICIT NONE

      CHARACTER*(*) file

      INCLUDE 'parobc.h'
      INCLUDE 'parcor.h'
* Common blocks to be initialized:
* Autocorrelation models
      INCLUDE 'comcor.h'

      INCLUDE 'trig.h'

      INTEGER unit,io,line,ic,lf,ip,ivers
      CHARACTER rec*80

      INTEGER lench
      EXTERNAL lench

* Initialization
      DO 1 io=obsc1,obsc2
      pto2f(io,1,1)=0
      pto2f(io,2,1)=0
 1    CONTINUE
      nfunt=0
      npart=0
      line=0

* Reading model
      lf=lench(file)
      CALL filopl(unit,file)
      READ(unit,*) ivers
      IF(ivers.NE.1) THEN
          WRITE(0,201) file(1:lf)
          STOP '**** Abnormal end ****'
      END IF
 201  FORMAT('ERROR(rdcorm): unsupported version of file "',A,'"')
      READ(unit,*) aprmx
      READ(unit,*) maxdst
      minapw=1.D0/((aprmx*radsec)**2)

 2    CONTINUE
* Read a new observatory/coordinate
      READ(unit,100,END=10) rec
 100  FORMAT(A)
      line=line+1
* IC = coordinate (1=RA, 2=DEC)
      IF(rec(1:8).EQ.'TIME RA ') THEN
          ic=1
      ELSEIF(rec(1:8).EQ.'TIME DEC') THEN
          ic=2
      ELSE
          GOTO 20
      END IF
* IO = observatory code
      READ(rec(9:),*,ERR=20) io
      IF(io.LT.obsc1) STOP '**** rdcorm: obsc < obsc1 ****'
      IF(io.GT.obsc2) STOP '**** rdcorm: obsc > obsc2 ****'
      pto2f(io,ic,1)=nfunt+1
      nfo(io,ic)=0
      nparo(io,ic)=0

 3    CONTINUE
* Read the model for one coordinate (IC) of one observatory (IO)
      READ(unit,100,ERR=20) rec
      line=line+1
      IF(rec.EQ.'END') THEN
          IF(nfo(io,ic).LE.0) GOTO 20
          GOTO 2
      END IF
      nfunt=nfunt+1
      IF(nfunt.GT.nparx) STOP '**** rdcorm: nfunt > nparx ****'
      pto2f(io,ic,2)=nfunt
      nfo(io,ic)=nfo(io,ic)+1
* Determine function integer code (KP1) and number of parameters (NP1)
      CALL fcsfun(rec,kfun(nfunt),nparf(nfunt))
      IF(kfun(nfunt).LE.0) GOTO 20
      nparo(io,ic)=nparo(io,ic)+nparf(nfunt)
* Reading and storing parameter values and properties
      IF(npart+nparf(nfunt).GT.nparx)
     +    STOP '**** rdcorm: npart > nparx ****'
      ptf2p(1,nfunt)=npart+1
      DO 4 ip=1,nparf(nfunt)
      npart=npart+1
      READ(unit,*,ERR=20) par(npart)
 4    CONTINUE
      ptf2p(2,nfunt)=npart
      GOTO 3

* Regular end
 10   CONTINUE
      CALL filclo(unit,' ')
      iiccor=36
      RETURN

* Error termination
 20   CONTINUE
      WRITE(0,200) file(1:lf),line
 200  FORMAT('rdcorm: INPUT ERROR from file "',A,'" at record',I5)
      STOP '**** rdcorm: abnormal end ****'

      END
