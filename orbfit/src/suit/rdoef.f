* Copyright (C) 1998 by Mario Carpino (carpino@brera.mi.astro.it),
* Version: June 19, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          R D O E F                            *
*  *                                                               *
*  *          Read orbital elements for a list of objects          *
*  *         from a file written in internal ORBFIT format         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  Input file name
*           OBJNAM    -  Object names (without embedded blanks)
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
      SUBROUTINE rdoef(file,objnam,nobj,deforb,defcn,eltype,telem,
     +                 elem,cove,nore,mass,h,g,comele)
      IMPLICIT NONE

      INTEGER nobj
      DOUBLE PRECISION telem(nobj),elem(6,nobj),cove(6,6,nobj)
      DOUBLE PRECISION nore(6,6,nobj),mass(nobj),h(nobj),g(nobj)
      CHARACTER*(*) file,objnam(nobj),eltype(nobj),comele(nobj)
      LOGICAL deforb(nobj),defcn(nobj)

      INTEGER kr,k,j1,j2,lf,ln,lc,lc1,nrem
      DOUBLE PRECISION t1,gmsun,gma,gma1,enne,h1,g1,m1
      DOUBLE PRECISION rot(3,3)
      DOUBLE PRECISION elem1(6),xv(6),cove1(6,6),cove2(6,6)
      DOUBLE PRECISION de(6,6),nore1(6,6),nore2(6,6)
      CHARACTER name1*80,eltyp1*3,rsys*10,epoch*10,krc*10,nc1*80
      LOGICAL defcov,defnor,end,error

      INTEGER lench
      EXTERNAL lench

      gmsun=0.01720209895d0**2

* Number of remaining object (orbit not yet found)
      nrem=0
      DO 1 k=1,nobj
      IF(.NOT.deforb(k)) nrem=nrem+1
 1    CONTINUE
      IF(nrem.LE.0) RETURN

      CALL oporbf(file)
      lf=lench(file)

 3    CONTINUE
      CALL rdorb(name1,elem1,eltyp1,t1,cove1,defcov,nore1,defnor,
     +           h1,g1,m1,rsys,epoch,kr,end)
      IF(end) GOTO 20
* Name match is performed disregarding embedded blanks
      nc1=name1
      CALL rmsp(nc1,lc1)
      IF(lc1.LE.0) GOTO 3

      DO 4 k=1,nobj
      IF(deforb(k)) GOTO 4
      IF(nc1(1:lc1).EQ.objnam(k)) THEN
          deforb(k)=.true.
           IF(rsys.EQ.'ECLM' .AND. epoch.EQ.'J2000') THEN
              DO 14 j1=1,6
              elem(j1,k)=elem1(j1)
              DO 14 j2=1,6
              cove(j1,j2,k)=cove1(j1,j2)
              nore(j1,j2,k)=nore1(j1,j2)
 14           CONTINUE
              eltype(k)=eltyp1
          ELSE
              gma=gmsun*m1
              gma1=gma+gmsun
              IF(defcov.OR.defnor) THEN
* Transformation in cartesian coordinates
                  CALL cooder(elem1,eltyp1,gma1,xv,'CAR',enne,de)
                  IF(defcov) CALL covprs(cove1,de,6,cove2)
                  IF(defnor) THEN
                      CALL norprs(nore1,de,6,nore2,error)
                      IF(error) THEN
                          ln=lench(name1)
                          write(99,120) file(1:lf),name1(1:ln)
                          defnor=.false.
                      END IF
                  END IF
* Transformation of reference system
                  CALL rotpn(rot,rsys,epoch,t1,'ECLM','J2000',0.d0)
                  CALL prodmv(elem(1,k),rot,xv(1))
                  CALL prodmv(elem(4,k),rot,xv(4))
                  DO 15 j1=1,3
                  DO 15 j2=1,3
                  de(j1,j2)=rot(j1,j2)
                  de(j1+3,j2)=0
                  de(j1,j2+3)=0
                  de(j1+3,j2+3)=rot(j1,j2)
 15               CONTINUE
                  IF(defcov) CALL covprs(cove2,de,6,cove(1,1,k))
                  IF(defnor) THEN
                      CALL norprs(nore2,de,6,nore(1,1,k),error)
                      IF(error) THEN
                          ln=lench(name1)
                          write(99,120) file(1:lf),name1(1:ln)
                          defnor=.false.
                      END IF
                  END IF
              ELSE
                  CALL coocha(elem1,eltyp1,gma1,xv,'CAR',enne)
                  CALL rotpn(rot,rsys,epoch,t1,'ECLM','J2000',0.d0)
                  CALL prodmv(elem(1,k),rot,xv(1))
                  CALL prodmv(elem(4,k),rot,xv(4))
              END IF
              eltype(k)='CAR'
          END IF
          telem(k)=t1
          CALL fixcnm(defcov,defnor,defcn(k),cove(1,1,k),nore(1,1,k))
          mass(k)=m1
          h(k)=h1
          g(k)=g1
          WRITE(krc,101) kr
          CALL rmsp(krc,lc)
          comele(k)='read from file "'//file(1:lf)//
     +              '" at record '//krc(1:lc)
          nrem=nrem-1
          IF(nrem.LE.0) GOTO 20
      END IF
 101  FORMAT(I6)
 120  FORMAT(' rdoef: error in transforming normal matrix'/
     +       '        (file "',A,'", object "',A,'")')
 4    CONTINUE
      GOTO 3
 20   CONTINUE

      CALL clorbf

      END
