* Copyright (C) 1997 by Mario Carpino
* Version: February 24, 1997
*
*  *****************************************************************
*  *                                                               *
*  *                         R D A N G A                           *
*  *                                                               *
*  *    Read an angle with its accuracy from a character string    *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    STRING    -  Character string
*
* OUTPUT:   ANGLE     -  Angle (no unit conversion is performed)
*           ACC       -  Angle accuracy
*           ERROR     -  Conversion error
*
      SUBROUTINE rdanga(string,angle,acc,error)
      IMPLICIT NONE
      CHARACTER*(*) string
      DOUBLE PRECISION angle,acc
      LOGICAL error

* Max string length
      INTEGER lx
      PARAMETER (lx=200)

      CHARACTER*(lx) c1,c,field
      INTEGER l,isig,nf,i,pp,iv,ll
      LOGICAL nospli
      DOUBLE PRECISION fact,rv


      INTEGER lench,nitchs
      EXTERNAL lench,nitchs

      error=.true.
      IF(lench(string).GT.lx) STOP '**** rdanga: LEN(string) > lx ****'
      c1=string
      CALL norstr(c1,l)

* The sign may be separated from the value by blank space
      isig=1
      IF(c1(1:1).EQ.'+') THEN
          c=c1(2:)
          CALL norstr(c,l)
      ELSEIF(c1(1:1).EQ.'-') THEN
          isig=-1
          c=c1(2:)
          CALL norstr(c,l)
      ELSE
          c=c1
      END IF

      nf=nitchs(c)
      IF(nf.LT.1.OR.nf.GT.3) RETURN
      angle=0
      fact=1
      DO 1 i=1,nf
          CALL stspli(c,' ',field,nospli)
          IF(nospli) RETURN
          pp=INDEX(field,'.')
          IF(pp.GT.0) THEN
              IF(i.NE.nf) RETURN
              READ(field,*,err=10,end=10) rv
          ELSE
              READ(field,*,err=10,end=10) iv
              rv=iv
          END IF
          angle=angle+fact*rv
          IF(i.EQ.nf) THEN
              CALL norstr(field,ll)
              pp=INDEX(field,'.')
              IF(pp.EQ.0) THEN
                  acc=1
              ELSE
                  ll=lench(field)
                  acc=10.0d0**(pp-ll)
              END IF
              acc=acc*fact
          ELSE
              fact=fact/60.d0
          END IF
 1    CONTINUE
      angle=angle*isig
      error=.false.

 10   CONTINUE
      END
