* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: October 13, 1997
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                         S P F L D S                         *
*  *                                                             *
*  *       Split fields (separated by a comma) in a string       *
*  *                                                             *
*  ***************************************************************
*
* INPUT:    STRING    -  Input string
*           NFX       -  Max dimension of FIELD vector
*
* OUTPUT:   FIELD     -  Fields
*           NF        -  Number of fields
*
      SUBROUTINE spflds(string,field,nf,nfx)
      IMPLICIT NONE

      INTEGER nf,nfx
      CHARACTER*(*) string,field(nfx)

      INTEGER nlx
      PARAMETER (nlx=200)

      INTEGER l
      CHARACTER*(nlx) c,item1
      LOGICAL nospli

      INTEGER lench
      EXTERNAL lench

      l=lench(string)
      IF(l.GT.nlx) STOP '**** spflds: l > nlx ****'

      nf=0
      c=string

 1    CONTINUE
      CALL stspli(c,',',item1,nospli)
      IF(nospli) RETURN
      nf=nf+1
      IF(nf.GT.nfx) STOP '**** spflds: nf > nfx ****'
      field(nf)=item1
      GOTO 1

      END
