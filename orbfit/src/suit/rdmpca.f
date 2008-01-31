* Copyright (C) 1997-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 16, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D M P C A                           *
*  *                                                               *
*  *          Read orbital elements for a list of objects          *
*  *        from a file written in MPC format for asteroids        *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Input FORTRAN unit (must be already opened)
*           FILNAM    -  Input file name (for error messages)
*           OBJNAM    -  Object names
*           NOBJ      -  Number of objects
*
* OUTPUT:   DEFORB    -  Tells whether orbital elements are defined
*           DEFCN     -  Tells whether covariance/normal matrices
*                            are defined
*           ELTYPE    -  Type of orbital elements (EQU/KEP/CAR)
*           TELEM     -  Epoch of orbital elements (MJD, TDT)
*           ELEM      -  Orbital elements (ECLM J2000)
*           COVE      -  Covariance matrix of orbital elements
*           NORE      -  Normal matrix of orbital elements
*           MASS      -  Mass (solar masses)
*           H         -  H absolute magnitude (if <-100, missing)
*           G         -  G slope parameter
*           COMELE    -  Comment on orbital elements
*
* WARNING: the routine assumes that objects having DEFORB=.true.
*          have already orbital elements defined (possibly from another
*          input file) and does not overwrite them
*
      SUBROUTINE rdmpca(unit,filnam,objnam,nobj,deforb,defcn,
     +                  eltype,telem,elem,cove,nore,mass,h,g,comele)
      IMPLICIT NONE

      INTEGER unit,nobj
      DOUBLE PRECISION telem(nobj),elem(6,nobj),cove(6,6,nobj)
      DOUBLE PRECISION nore(6,6,nobj),mass(nobj),h(nobj),g(nobj)
      CHARACTER*(*) filnam,objnam(nobj),eltype(nobj),comele(nobj)
      LOGICAL deforb(nobj),defcn(nobj)

      INCLUDE 'trig.h'

      INTEGER nobjx
      PARAMETER (nobjx=10)

      INTEGER nrem,k,lf,nr,ln,lc
      DOUBLE PRECISION el1(6)
      CHARACTER*7 nmpc(nobjx),nmpc1
      CHARACTER hc*5,ep5*5,krc*10,gc*5
      LOGICAL error

      INTEGER lench
      EXTERNAL lench

      IF(nobj.GT.nobjx) STOP '**** rdmpca: nobj > nobjx ****'

* Number of remaining object (orbit not yet found)
      nrem=0
      DO 10 k=1,nobj
      IF(deforb(k)) GOTO 10
      nrem=nrem+1
      CALL mpcpds(objnam(k),nmpc(k),error)
      IF(error) THEN
          ln=lench(objnam(k))
          WRITE(*,110) objnam(k)(1:ln)
      END IF
 110  FORMAT('rdmpca: cannot understand asteroid code "',A,'"')
 10   CONTINUE
      IF(nrem.LE.0) RETURN
      lf=lench(filnam)

      nr=0
 1    CONTINUE
      READ(unit,100,END=2) nmpc1
 100  FORMAT(A7,A5,2X,A5,1X,A5,1X,F9.5,2X,F9.5,2X,F9.5,2X,F9.5,
     +       2X,F9.7,13X,F11.7)
      nr=nr+1

      DO 3 k=1,nobj
      IF(deforb(k)) GOTO 3
      IF(nmpc1.EQ.nmpc(k)) THEN
          BACKSPACE(unit)
          READ(unit,100) nmpc1,hc,gc,ep5,el1
          deforb(k)=.true.
          defcn(k)=.false.
          eltype(k)='KEP'
          CALL mpcdat(ep5,telem(k),error)
          IF(error) THEN
              WRITE(*,111) nr,filnam(1:lf)
              STOP '**** rdmpca: abnormal end ****'
          END IF
          elem(1,k)=el1(6)
          elem(2,k)=el1(5)
          elem(3,k)=el1(4)*radeg
          elem(4,k)=el1(3)*radeg
          elem(5,k)=el1(2)*radeg
          elem(6,k)=el1(1)*radeg
          mass(k)=0.d0
          IF(hc.EQ.'     ') THEN
              h(k)=-1.D9
          ELSE
              READ(hc,101) h(k)
          END IF
          IF(gc.EQ.'     ') THEN
              g(k)=-1.D9
          ELSE
              READ(gc,101) g(k)
          END IF
          WRITE(krc,107) nr
          CALL rmsp(krc,lc)
          comele(k)='read from file "'//filnam(1:lf)//
     +              '" at record '//krc(1:lc)

          nrem=nrem-1
          IF(nrem.LE.0) RETURN
      END IF
 101  FORMAT(F5.2)
 107  FORMAT(I6)
 111  FORMAT('INPUT ERROR: illegal date code at record',I6,
     +       ' of file "',A,'"')

 3    CONTINUE

      GOTO 1
 2    CONTINUE

      END
