* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 28, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                           R M S P                             *
*  *                                                               *
*  *            Remove spaces from a character string              *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    C         -  Character string
*
* OUTPUT:   C         -  Character string without spaces
*           L         -  Length of the output character string
*
      SUBROUTINE rmsp(c,l)
      IMPLICIT NONE

      INCLUDE 'parch.h'
      INCLUDE 'spaces1.h'

      INTEGER l,i,icc,k,l1
      CHARACTER c*(*)
      CHARACTER*(lchx) c1

      INCLUDE 'spaces2.h'

      l=LEN(c)
      CALL chkpdf(l,lchx,'lchx')
      IF(l.LE.0) STOP '**** rmsp: internal error (01) ****'

      c1=' '
      l1=0

      DO 1 i=1,l
         icc=ICHAR(c(i:i))
         DO 2 k=1,nspac
            IF(icc.EQ.icspac(k)) GOTO 1
 2       CONTINUE
         l1=l1+1
         c1(l1:l1)=c(i:i)
 1    CONTINUE
      
      l=l1
      IF(l.LE.0) THEN
         c=' '
         RETURN
      END IF
      
      c=c1(1:l1)

      END
