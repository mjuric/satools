* Copyright (C) 1997-1999 by Mario Carpino (carpino@brera.mi.astro.it),
*                            Andrea Milani (milani@dm.unipi.it),
*                            Zoran Knezevic (zoran@aob.aob.bg.ac.yu)
* Version: February 12, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R D E L E M                           *
*  *                                                               *
*  *          Read orbital elements for a list of objects          *
*  *                     from a list of files                      *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Output FORTRAN unit (report file)
*           OBJNAM    -  Object names
*           NOBJ      -  Number of objects
*           INFILS    -  Input files
*           NFIL      -  Number of input files
*           DEFORB    -  Is the orbit already defined?
*
* OUTPUT:   DEFORB    -  Was the orbit found?
*           DEFCN     -  Tells whether covariance/normal matrices
*                            are defined
*           ELTYPE    -  Type of orbital elements (EQU/KEP/CAR)
*           TELEM     -  Epoch of orbital elements (MJD, TDT)
*           ELEM      -  Orbital elements (ECLM J2000)
*           COVE      -  Covariance matrix of orbital elements
*           NORE      -  Normal matrix of orbital elements
*           MASS      -  Mass (solar masses)
*           HMAG      -  H absolute magnitude (if <-100, missing)
*           GMAG      -  G slope parameter
*           COMELE    -  Comment on orbital elements
*
* WARNING: the routine is designed to allow multiple calls on
*          different lists of files/objects: for this reason,
*          orbital elements of objects which have already an
*          orbit defined (DEFORB=.true.) ARE NOT modified.
*          For this reason, the user must define suitably DEFORB
*          BEFORE calling RDELEM
*
      SUBROUTINE rdelem(unit,objnam,nobj,infils,nfil,deforb,defcn,
     +                  eltype,telem,elem,cove,nore,
     +                  mass,h,g,comele)
      IMPLICIT NONE
      INTEGER nfilx
      PARAMETER(nfilx=20)
      INTEGER unit,nobj,nfil
      DOUBLE PRECISION telem(nobj),elem(6,nobj),cove(6,6,nobj)
      DOUBLE PRECISION nore(6,6,nobj),mass(nobj),h(nobj),g(nobj)
      CHARACTER*(*) objnam(nobj),infils(nfil),eltype(nobj),comele(nobj)
      LOGICAL deforb(nobj),defcn(nobj)

      INTEGER i,lf,is1,lfo,uniin,n
      CHARACTER form*10,infil1*100
      LOGICAL opened

      INTEGER lench
      EXTERNAL lench

* Number of input files
      IF(nfil.GT.nfilx) STOP '**** rdelem: nfil > nfilx ****'

* Nothing to do
      IF(nfil.LE.0) RETURN

* Loop on files
      DO 2 i=1,nfil
      lf=lench(infils(i))
      opened=.false.

* Understand file format
      is1=index(infils(i)(1:lf),'[')
      IF(is1.GT.0 .AND. infils(i)(lf:lf).EQ.']') THEN
          form=infils(i)(is1+1:lf-1)
          lf=is1-1
          infil1=infils(i)(1:lf)
      ELSE
          infil1=infils(i)
          form=' '
          CALL filopn(uniin,infil1,'OLD')
          opened=.true.
          CALL oefdet(uniin,infil1,form)
          REWIND(uniin)
      END IF
      lfo=lench(form)
      IF(form.NE.' ') WRITE(*,102) infil1(1:lf),form(1:lfo)
 102  FORMAT('Scanning file "',A,'" (format: ',A,')')

* Reading file
      IF(form.EQ.'OEF') THEN
          IF(opened) CALL filclo(uniin,' ')
          CALL rdoef(infil1,objnam,nobj,deforb,defcn,eltype,telem,
     +               elem,cove,nore,mass,h,g,comele)
      ELSEIF(form.EQ.'BA1') THEN
          IF(.NOT.opened) CALL filopn(uniin,infil1,'OLD')
          CALL rdast1(uniin,infil1,objnam,nobj,deforb,defcn,
     +                eltype,telem,elem,cove,nore,mass,h,g,comele)
          CALL filclo(uniin,' ')
      ELSEIF(form.EQ.'BA2') THEN
          IF(.NOT.opened) CALL filopn(uniin,infil1,'OLD')
          CALL rdast2(uniin,infil1,objnam,nobj,deforb,defcn,
     +                eltype,telem,elem,cove,nore,mass,h,g,comele)
          CALL filclo(uniin,' ')
      ELSEIF(form.EQ.'MPC-A') THEN
          IF(.NOT.opened) CALL filopn(uniin,infil1,'OLD')
          CALL rdmpca(uniin,infil1,objnam,nobj,deforb,defcn,
     +                eltype,telem,elem,cove,nore,mass,h,g,comele)
          CALL filclo(uniin,' ')
      ELSEIF(form.EQ.' ') THEN
          WRITE(*,120) infil1(1:lf)
          STOP '**** rdelem: abnormal end ****'
      ELSE
          WRITE(*,121) form(1:lfo),infil1(1:lf)
          STOP '**** rdelem: abnormal end ****'
      END IF
 120  FORMAT('ERROR: unknown format type for file "',A,'"')
 121  FORMAT('ERROR: unsupported format "',A,'" for file "',A,'"')
c set default for magnitude data
      DO n=1,nobj
        IF(h(n).lt.-1.d6)THEN
c leave it            
        ENDIF
        IF(g(n).lt.-1.d6)THEN
           g(n)=0.15d0
        ENDIF
      ENDDO
* End of loop on files
 2    CONTINUE

 30   CONTINUE

      END



