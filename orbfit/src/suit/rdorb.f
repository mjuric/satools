* Copyright (C) 1997-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 21, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          R D O R B                            *
*  *                                                               *
*  *     Read orbital elements from a file opened with OPORBF      *
*  *                                                               *
*  *****************************************************************
*
* The routine operates in a sequential way, returning each time
* the orbital elements of the next object contained in the file
*
* OUTPUT:   NAME      -  Name of planet/asteroid/comet
*           ELEM(6)   -  Orbital element vector
*           ELTYPE    -  Type of orbital elements (KEP/EQU/CAR)
*           T0        -  Epoch of orbital elements (MJD, TDT)
*           COVE      -  Covariance matrix of orbital elements
*           DEFCOV    -  Tells whether the covariance matrix is defined
*           NORE      -  Normal matrix of orbital elements
*           DEFNOR    -  Tells whether the normal matrix is defined
*           H         -  H absolute magnitude (if <-100, missing)
*           G         -  G slope parameter
*           MASS      -  Mass (solar masses)
*           RSYS      -  Reference system type (EQUM/EQUT/ECLM)
*           EPOCH     -  Epoch specification (J2000/OFDATE)
*           KR        -  Record number at which object is found
*           EOF       -  End-of-file flag
*
      SUBROUTINE rdorb(name,elem,eltype,t0,cove,defcov,nore,defnor,
     +                 h,g,mass,rsys,epoch,kr,eof)
      IMPLICIT NONE

      DOUBLE PRECISION elem(6),t0,h,g,cove(6,6),nore(6,6),mass
      CHARACTER*(*) name,eltype,rsys,epoch
      LOGICAL defcov,defnor,eof
      INTEGER kr

      INCLUDE 'trig.h'

      CHARACTER rec*200,rest*200,scale*3
      INTEGER lf,nit,i,k,mjd,mjde,ik
      DOUBLE PRECISION sec,sece,tmp(21),cnv(6)
      LOGICAL error,end1,noep


      INCLUDE 'comorb.h'

      INTEGER lench,nitchs
      EXTERNAL lench,nitchs

      IF(iicorb.NE.36) STOP '**** rdorb: internal error (01) ****'
      IF(orbunt.LE.0) STOP '**** rdorb: internal error (02) ****'

      rsys=dfrsty
      epoch=dfrsep
      mass=0.d0
      DO 1 i=1,6
      DO 2 k=1,6
      cove(i,k)=0
      nore(i,k)=0
 2    CONTINUE
 1    CONTINUE
      defcov=.false.
      defnor=.false.
      h=-1.d9
      g=-1.d9

      IF(rectyp.EQ.'1L') THEN
          CALL getrsc(orbunt,rec,orbnr,eof)
          IF(eof) RETURN
          CALL strcnt(rec,name,rest,error)
          IF(error) GOTO 20
          kr=orbnr
          nit=nitchs(rest)
          IF(deft0) THEN
              t0=dept0
              IF(nit.LT.8) THEN
                  READ(rest,*,ERR=20) elem
              ELSE
                  READ(rest,*,ERR=20) elem,h,g
              END IF
          ELSE
              IF(nit.LT.9) THEN
                  READ(rest,*,ERR=20) t0,elem
              ELSE
                  READ(rest,*,ERR=20) t0,elem,h,g
              END IF
          END IF
          eltype=deltyp
      ELSEIF(rectyp.EQ.'ML') THEN
          IF(nxtend) THEN
              eof=.true.
              RETURN
          END IF
          noep=.true.
* Name
          CALL getrsc(orbunt,name,orbnr,eof)
          IF(eof) RETURN
          kr=orbnr
* Orbital elements (mandatory, immediately after the name)
          CALL getrsc(orbunt,rec,orbnr,end1)
          IF(end1) GOTO 20
          IF(rec(1:4).EQ.' KEP') THEN
              READ(rec(5:),*,ERR=20) elem
              eltype='KEP'
          ELSEIF(rec(1:4).EQ.' EQU') THEN
              READ(rec(5:),*,ERR=20) elem
              eltype='EQU'
          ELSEIF(rec(1:4).EQ.' CAR') THEN
              READ(rec(5:),*,ERR=20) elem
              eltype='CAR'
          ELSE
              GOTO 20
          END IF

* Other keywords
 3        CONTINUE
          CALL getrsc(orbunt,rec,orbnr,end1)
          IF(end1) THEN
              nxtend=.true.
              GOTO 4
          END IF
          IF(rec(1:1).NE.' ') THEN
              BACKSPACE(orbunt)
              orbnr=orbnr-1
              GOTO 4
          END IF
* Epoch of elements
          IF(rec(1:4).EQ.' MJD' .OR. rec(1:4).EQ.' JD ' .OR.
     +       rec(1:4).EQ.' CAL') THEN
              CALL ch2tim(rec,mjd,sec,scale,error)
              IF(error) GOTO 20
              CALL cnvtim(mjd,sec,scale,mjde,sece,'TDT')
              t0=mjde+sece/86400.d0
              noep=.false.
          ELSEIF(rec(1:4).EQ.' MAG') THEN
              READ(rec(5:),*,ERR=20) h,g
          ELSEIF(rec(1:4).EQ.' MAS') THEN
              READ(rec(5:),*,ERR=20) mass
          ELSEIF(rec(1:4).EQ.' COV') THEN
              READ(rec(5:),*,ERR=20) (tmp(i),i=1,3)
              DO 17 k=1,6
              CALL getrsc(orbunt,rec,orbnr,end1)
              IF(end1) GOTO 20
              IF(rec(1:4).NE.' COV') GOTO 20
              READ(rec(5:),*,ERR=20) (tmp(i),i=3*k+1,3*k+3)
 17           CONTINUE
              ik=0
              DO 8 i=1,6
              DO 7 k=i,6
              ik=ik+1
              cove(i,k)=tmp(ik)
 7            CONTINUE
 8            CONTINUE
              IF(ik.NE.21) STOP '**** rdorb: internal error (03) ****'
              defcov=.true.
          ELSEIF(rec(1:4).EQ.' NOR') THEN
              READ(rec(5:),*,ERR=20) (tmp(i),i=1,3)
              DO 27 k=1,6
              CALL getrsc(orbunt,rec,orbnr,end1)
              IF(end1) GOTO 20
              IF(rec(1:4).NE.' NOR') GOTO 20
              READ(rec(5:),*,ERR=20) (tmp(i),i=3*k+1,3*k+3)
 27           CONTINUE
              ik=0
              DO 38 i=1,6
              DO 37 k=i,6
              ik=ik+1
              nore(i,k)=tmp(ik)
 37           CONTINUE
 38           CONTINUE
              IF(ik.NE.21) STOP '**** rdorb: internal error (04) ****'
              defnor=.true.
          ELSE
              GOTO 20
          END IF
          GOTO 3
 4        CONTINUE
          IF(noep) THEN
              IF(deft0) THEN
                  t0=dept0
              ELSE
                  GOTO 20
              END IF
          END IF
      ELSE
          STOP '**** rdorb: internal error (05) ****'
      END IF

* Transformation of angles in orbital elements
* and covariance matrix
      DO 9 i=1,6
      cnv(i)=1
 9    CONTINUE
      IF(eltype.EQ.'KEP') THEN
          DO 10 i=3,6
          cnv(i)=radeg
 10       CONTINUE
      ELSEIF(eltype.EQ.'EQU') THEN
          cnv(6)=radeg
      ELSEIF(eltype.EQ.'CAR') THEN
          CONTINUE
      ELSE
          STOP '**** rdorb: internal error (06) ****'
      END IF
      DO 11 i=1,6
      elem(i)=elem(i)*cnv(i)
 11   CONTINUE
      IF(defcov) THEN
          DO 13 i=1,6
          cove(i,i)=cove(i,i)*(cnv(i)**2)
          DO 12 k=i+1,6
          cove(i,k)=cove(i,k)*cnv(i)*cnv(k)
          cove(k,i)=cove(i,k)
 12       CONTINUE
 13       CONTINUE
      END IF
      IF(defnor) THEN
          DO 15 i=1,6
          nore(i,i)=nore(i,i)/(cnv(i)**2)
          DO 14 k=i+1,6
          nore(i,k)=nore(i,k)/(cnv(i)*cnv(k))
          nore(k,i)=nore(i,k)
 14       CONTINUE
 15       CONTINUE
      END IF

      eof=.false.
      RETURN

 20   CONTINUE
      lf=lench(orbfn)
      WRITE(*,200) orbfn(1:lf),orbnr
 200  FORMAT(' ERROR in file ',A,' at line',I6)
      STOP '**** rdorb: abnormal end ****'

      END
