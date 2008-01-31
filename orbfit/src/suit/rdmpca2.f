* Copyright (C) 1997-1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: 2.1.1 April 29, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D M P C A 2                         *
*  *                                                               *
*  *          Read orbital elements of all objects                 *
*  *        as given in a file written in MPC format for asteroids *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Input FORTRAN unit (must be already opened)
*           FILNAM    -  Input file name (for error messages)
*           NOBJX      -  Number of objects (maximum)
*
* OUTPUT:   OBJNAM    -  Object names
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
*           NOBJ      -  Number of objects, actual number found
*
*
*
      SUBROUTINE rdmpca2(unit,filnam,objnam,nobjx,nobj,defcn,
     +                  eltype,telem,elem,cove,nore,mass,h,g,comele)
      IMPLICIT NONE

      INTEGER unit,nobj,nobjx
      DOUBLE PRECISION telem(nobjx),elem(6,nobjx),cove(6,6,nobjx)
      DOUBLE PRECISION nore(6,6,nobjx),mass(nobjx),h(nobjx),g(nobjx)
      CHARACTER*(*) filnam,objnam(nobjx),eltype(nobjx),comele(nobjx)
      LOGICAL defcn(nobjx)

      INCLUDE 'trig.h'

      INTEGER nrem,k,lf,nr,ln,lc
      DOUBLE PRECISION el1(6)
      CHARACTER*7 nmpc1
      CHARACTER hc*5,ep5*5,krc*10,gc*5
      LOGICAL error

      INTEGER lench
      EXTERNAL lench
      CALL rmsp(filnam,lf)

      nr=0
      DO 3 k=1,nobjx
         READ(unit,100,END=2) nmpc1,hc,gc,ep5,el1
 100     FORMAT(A7,A5,2X,A5,1X,A5,1X,F9.5,2X,F9.5,2X,F9.5,2X,F9.5,
     +        2X,F9.7,13X,F11.7)
         nr=nr+1
         nobj=nr
c name conversion
         CALL iaucod2(nmpc1,objnam(k),error)
         IF(error) THEN
            WRITE(*,112) nr,filnam(1:lf)
 112        FORMAT('INPUT ERROR: illegal name code at record',I6,
     +       ' of file "',A,'"')
            STOP '**** rdmpca2: name conversion error ****'
         END IF
         defcn(k)=.false.
         eltype(k)='KEP'
         CALL mpcdat(ep5,telem(k),error)
         IF(error) THEN
            WRITE(*,111) nr,filnam(1:lf)
 111        FORMAT('INPUT ERROR: illegal date code at record',I6,
     +       ' of file "',A,'"')
            STOP '**** rdmpca2: date conversion error ****'
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
 101        FORMAT(F5.2)
         END IF
         IF(gc.EQ.'     ') THEN
            g(k)=-1.D9
         ELSE
            READ(gc,101) g(k)
         END IF
         WRITE(krc,107) nr
 107     FORMAT(I6)
         CALL rmsp(krc,lc)
         comele(k)='read from file "'//filnam(1:lf)//
     +              '" at record '//krc(1:lc)
 3    ENDDO
      WRITE(*,*)' file not completely read, record ',nr
 2    CONTINUE
      RETURN
      END




