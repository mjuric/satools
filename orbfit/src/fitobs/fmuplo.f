c =======================================
c  FMUPLO
c =======================================
c graphic output of multiple orbits
c ===============INTERFACE========================
      SUBROUTINE fmuplo(eqm,tc,numb,eqc,titnam,sigma)
      IMPLICIT NONE
      INTEGER numb
      CHARACTER*80 titnam
      DOUBLE PRECISION tc,sigma,eqc(6),eqm(6,numb)
c ============END INTERFACE==================
c number of alternate solutions, maximum
      INCLUDE 'parmul.h'
      DOUBLE PRECISION a(mulx),e(mulx),aa,ee
      INTEGER i
c ===========================================
c a-e plot of multiple solutions
      DO i=1,numb
        a(i)=eqm(1,i)
        e(i)=sqrt(eqm(2,i)**2+eqm(3,i)**2)
      ENDDO
      aa=eqc(1)
      ee=sqrt(eqc(2)**2+eqc(3)**2)
      CALL ploae(tc,a,e,aa,ee,sigma,numb,titnam)
      RETURN
      END





