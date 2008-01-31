* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 20, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                          R D B E P                            *
*  *                                                               *
*  *         Reads and interpolates binary ephemeris files         *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    T         -  Time (MJD, TDT)
*           NB        -  Number of bodies
*           ID        -  Identity (order number) of requested bodies
*
* OUTPUT:   X,V       -  Position and velocity vectors of requested
*                        bodies (refsys: ECLM J2000)
*
* EXAMPLE:
*     NB=2, ID(1)=3, ID(2)=1 means that the third and first bodies
*           contained in the binary ephemeris files are requested
*     on output, X(i,1) will contain the position vector of body
*     number 3 and X(i,2) the position vector of body number 1
*
      SUBROUTINE rdbep(t,nb,id,x,v)
      IMPLICIT NONE

      INCLUDE 'parbep.h'
      INCLUDE 'combep.h'

      CHARACTER*4 kind
      PARAMETER (kind='AM  ')

      INTEGER nb
      INTEGER id(nb)
      DOUBLE PRECISION t,x(3,nb),v(3,nb)

      INTEGER unit,nhl,vers,n,recl,i,k,ib,lf
      DOUBLE PRECISION t1,t2,dt,gma(nbepx),gma1(nbepx)
      DOUBLE PRECISION c1,c2
      DOUBLE PRECISION xv1(6),xv2(6),enne
      LOGICAL first

      SAVE first,unit,nhl,n,t1,t2,dt,gma,gma1

* Buffer
      INTEGER ipt1,ipt2
      DOUBLE PRECISION tb1,tb2,elem1(6,nbepx),elem2(6,nbepx)
      DOUBLE PRECISION enne1(nbepx),enne2(nbepx),m01(nbepx),m02(nbepx)

      SAVE ipt1,ipt2,tb1,tb2,elem1,elem2,enne1,enne2,m01,m02

      INTEGER lench
      EXTERNAL lench

      DATA first/.true./

      IF(first) THEN
          CALL filass(unit,filbep)

* Open with a small record length (just to read the number of bodies)
          OPEN(unit,FILE=filbep,ACCESS='DIRECT',RECL=24,STATUS='OLD')
          READ(unit,REC=1) vers,nbep,n
          IF(vers.NE.102) THEN
              lf=lench(filbep)
              WRITE(*,120) filbep(1:lf),vers
              STOP '**** rdbep: abnormal end ****'
          END IF
          CLOSE(unit)
          CALL chkpdf(nbep,nbepx,'nbepx')
          IF(n.LT.2) STOP '**** rdbep: n < 2 ****'
          nhl=2+2*nbep

* Open again with the correct record length
          recl=8*6*nbep
          OPEN(unit,FILE=filbep,ACCESS='DIRECT',RECL=recl,STATUS='OLD')
          READ(unit,REC=2) t1,t2,dt

* Read masses
          DO 1 i=1,nbep
          READ(unit,REC=2+i) masbep(i),gma(i),gma1(i)
 1        CONTINUE

* Initialization of buffer
          ipt1=1
          ipt2=2
          READ(unit,REC=nhl+ipt1) ((elem1(i,k),i=1,6),k=1,nbep)
          READ(unit,REC=nhl+ipt2) ((elem2(i,k),i=1,6),k=1,nbep)
          tb1=t1+dt*(ipt1-1)
          tb2=t1+dt*(ipt2-1)
          DO 4 k=1,nbep
          enne1(k)=SQRT(gma1(k)/elem1(1,k)**3)
          enne2(k)=SQRT(gma1(k)/elem2(1,k)**3)
          m01(k)=elem1(6,k)
          m02(k)=elem2(6,k)
 4        CONTINUE

          first=.false.
      END IF
 120  FORMAT(' ERROR: file ',A,' has unsupported version',I4)

      IF(t.LT.tb1 .OR. t.GT.tb2) THEN
          IF(t.LT.t1 .OR. t.GT.t2)
     +        STOP '**** rdbep: T is out of bounds ****'
          ipt1=(t-t1)/dt+1
          ipt2=ipt1+1
          IF(ipt1.LT.1) STOP '**** rdbep: internal error (01) ****'
          IF(ipt2.GT.n) STOP '**** rdbep: internal error (02) ****'
          READ(unit,REC=nhl+ipt1) ((elem1(i,k),i=1,6),k=1,nbep)
          READ(unit,REC=nhl+ipt2) ((elem2(i,k),i=1,6),k=1,nbep)
          tb1=t1+dt*(ipt1-1)
          tb2=t1+dt*(ipt2-1)
          IF(t.LT.tb1 .OR. t.GT.tb2)
     +        STOP '**** rdbep: internal error (03) ****'
          DO 5 k=1,nbep
          enne1(k)=SQRT(gma1(k)/elem1(1,k)**3)
          enne2(k)=SQRT(gma1(k)/elem2(1,k)**3)
          m01(k)=elem1(6,k)
          m02(k)=elem2(6,k)
 5        CONTINUE
      END IF

* Coefficients for interpolation
      c1=(tb2-t)/dt
      c2=(t-tb1)/dt

      DO 2 ib=1,nb
      k=id(ib)
      elem1(6,k)=m01(k)+enne1(k)*(t-tb1)
      elem2(6,k)=m02(k)+enne2(k)*(t-tb2)
      CALL coocha(elem1(1,k),'KEP',gma1(k),xv1,'CAR',enne)
      CALL coocha(elem2(1,k),'KEP',gma1(k),xv2,'CAR',enne)
      DO 3 i=1,3
      x(i,ib)=c1*xv1(i)+c2*xv2(i)
      v(i,ib)=c1*xv1(i+3)+c2*xv2(i+3)
 3    CONTINUE
 2    CONTINUE

      END
