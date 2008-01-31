c ====================================================
c TITAST handles asteroid names to form title string
c ====================================================
      subroutine titast(iarc,astna0,astnap,titnam,filnam,let)
      implicit none
      integer iarc
      character*(*) astna0,astnap
      character*40 nam0,namp
      character*(*) titnam
      character*(*) filnam
      integer le,le1,let,lench
c ====================================================
c  shorten and remove blanks from name
      nam0=astna0
      CALL rmsp(nam0,le)
      IF(lench(astnap).gt.0)THEN
         namp=astnap
         CALL rmsp(namp,le1)
      ENDIF
      if(iarc.eq.1)then
         titnam=astna0
         filnam=nam0
      elseif(iarc.eq.2)then
         titnam=astnap
         filnam=namp
      elseif(iarc.eq.3)then
         filnam=nam0(1:le)//'='//namp(1:le1)
         let=le+le1+1
         le=lench(astna0)
         le1=lench(astnap)
         titnam=astna0(1:le)//'='//astnap(1:le1)
      else
         write(*,*)'titast: this should not happen, iarc=',iarc
      endif
      return
      end
