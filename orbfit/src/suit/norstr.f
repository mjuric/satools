* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 10, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         N O R S T R                           *
*  *                                                               *
*  *              Normal form of a character string                *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    C         -  Character string
*
* OUTPUT:   C         -  Character string in normal form:
*                          1) leading spaces are removed
*                          2) tabs are substituted by blanks
*                          3) multiple spaces are substituted by
*                             a single blank
*                          4) quoted strings are preserved
*           L         -  Length of the output character string
*
      SUBROUTINE norstr(c,l)
      IMPLICIT NONE

      INCLUDE 'parch.h'
      INCLUDE 'spaces1.h'

      CHARACTER*(*) c
      CHARACTER*(lchx) c1
      INTEGER l,i1,icc,k,l1,nsp,i
      LOGICAL isspac,quoted

      INTEGER lench
      EXTERNAL lench

      INCLUDE 'spaces2.h'

      l=lench(c)
      CALL chkpdf(l,lchx,'lchx')
      IF(l.LE.0) THEN
          c=' '
          RETURN
      END IF

      c1=' '

      DO 1 i1=1,l
      icc=ICHAR(c(i1:i1))
      DO 12 k=1,nspac
      IF(icc.EQ.icspac(k)) GOTO 1
 12   CONTINUE
      GOTO 2
 1    CONTINUE
 2    CONTINUE

      l1=0
      nsp=0
      quoted=.false.

      DO 3 i=i1,l

      IF(c(i:i).EQ.'''') THEN
          IF(quoted) THEN
              quoted=.false.
              nsp=0
          ELSE
              quoted=.true.
          END IF
      END IF
      IF(quoted) GOTO 4

      isspac=.false.
      icc=ICHAR(c(i:i))
      DO 13 k=1,nspac
      IF(icc.EQ.icspac(k)) THEN
          isspac=.true.
          GOTO 14
      END IF
 13   CONTINUE
 14   CONTINUE

      IF(isspac) THEN
          nsp=nsp+1
          IF(nsp.EQ.1) THEN
              l1=l1+1
              c1(l1:l1)=' '
          END IF
          GOTO 3
      END IF

 4    l1=l1+1
      c1(l1:l1)=c(i:i)
      nsp=0

 3    CONTINUE

      l=l1
      IF(l.LE.0) THEN
          c=' '
          RETURN
      END IF

      c=c1(1:l1)

      END
