      DOUBLE PRECISION FUNCTION meanti(tau,rmsa,rmsd,m)
      IMPLICIT NONE
      INTEGER m
      DOUBLE PRECISION tau(m),rmsa(m),rmsd(m)
      INTEGER i
      DOUBLE PRECISION tw,w
      meanti=0.d0
      IF(m.le.0)THEN
         WRITE(*,*)'meanti: no data '
         RETURN
      ENDIF
      tw=0.d0
      DO i=1,m
        w=1.d0/(rmsa(i)**2+rmsd(i)**2)
        meanti=meanti+tau(i)*w
        tw=tw+w
      ENDDO
      meanti=meanti/tw
      RETURN
      END
