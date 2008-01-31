c ===========================================
c SNORMD RMS norm of vector v with diagonal metric a
c warning: if a weight is zero, the component is not
c counted in the vector length
      DOUBLE PRECISION FUNCTION snormd(v,a,n,nused)
      IMPLICIT NONE
      INTEGER n,i,nused
      DOUBLE PRECISION v(n),a(n)
      snormd=0.d0
      nused=0
      DO  i=1,n
        snormd=snormd+(v(i)**2)*a(i)
        IF(a(i).gt.0.d0)nused=nused+1
      ENDDO
      snormd=sqrt(snormd/nused)
      RETURN
      END
