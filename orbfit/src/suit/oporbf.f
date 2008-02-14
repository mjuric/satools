* Copyright (C) 1997-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 21, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         O P O R B F                           *
*  *                                                               *
*  *                Open an orbital element file                   *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  File name
*
      SUBROUTINE oporbf(file)
      IMPLICIT NONE

      CHARACTER*(*) file

      INTEGER kr,lf,mjd,mjde,nn
      LOGICAL first,found,end
      CHARACTER form*20,scale*3,rec*200
      DOUBLE PRECISION sec,sece

      SAVE first

      INCLUDE 'comorb.h'

      INTEGER lench,nitchs
      EXTERNAL lench,nitchs

      DATA first/.true./

	  write(0,*) "Doing shit..."

      IF(first) THEN
          orbunt=0
          orbfn=' '
          first=.false.
      END IF

      IF(orbunt.NE.0) STOP '**** oporbf: internal error (01) ****'

      orbfn=file
      CALL filopn(orbunt,orbfn,'OLD')
      CALL rdfnam(orbunt,orbfn,orbnr)
	  write(0,*) "Doing shit..."
      lf=lench(orbfn)

* Format
      CALL rdfcha(orbunt,'format',.true.,form,found,kr)
      IF(form.NE.'OEF1.1') THEN
          WRITE(0,100) orbfn(1:lf)
          STOP '**** oporbf: abnormal end ****'
      END IF
 100  FORMAT('ERROR: unsupported format in file ',A)

* Record type and default orbital element type
      CALL rdfcha(orbunt,'rectype',.false.,rectyp,found,kr)
      IF(.NOT.found) THEN
 1        CONTINUE
          CALL getrsc(orbunt,rec,orbnr,end)
          IF(end) THEN
              WRITE(0,104) orbfn(1:lf)
              STOP '**** oporbf: abnormal end ****'
          END IF
          orbnr=orbnr-1
          BACKSPACE(orbunt)
          nn=nitchs(rec)
          IF(nn.EQ.1) THEN
              rectyp='ML'
          ELSEIF(nn.GE.7) THEN
              rectyp='1L'
          ELSE
              orbnr=orbnr+1
              GOTO 10
          END IF
      END IF
 104  FORMAT(' ERROR: file ',A,' is empty')
      IF(rectyp.EQ.'1L') THEN
          CALL rdfcha(orbunt,'elem',.true.,deltyp,found,kr)
      ELSEIF(rectyp.EQ.'ML') THEN
          deltyp=' '
      ELSE
          WRITE(0,101) orbfn(1:lf)
          STOP '**** oporbf: abnormal end ****'
      END IF
 101  FORMAT('ERROR: unsupported record type in file ',A)

* Default reference system
      CALL rdfref(orbunt,'refsys',.false.,dfrsty,dfrsep,found,kr)
      IF(.NOT.found) THEN
          IF(rectyp.EQ.'1L') THEN
              WRITE(0,105) orbfn(1:lf)
              STOP '**** oporbf: abnormal end ****'
          END IF
          dfrsty=' '
          dfrsep=' '
      END IF
 105  FORMAT(' ERROR: missing keyword "refsys" in file ',A)

* Default epoch for orbital elements
      CALL rdftim(orbunt,'epoch',.false.,depstr,mjd,sec,scale,deft0,kr)
      IF(deft0) THEN
          CALL cnvtim(mjd,sec,scale,mjde,sece,'TDT')
          dept0=mjde+sece/86400.d0
      END IF

      nxtend=.false.
      iicorb=36
      RETURN

 10   CONTINUE
      WRITE(0,102) orbfn(1:lf),orbnr
 102  FORMAT(' FORMAT ERROR in file ',A,' at line',I5)
      STOP '**** oporbf: abnormal end ****'

      END
