c ==============================================
c SNORM norm of vector v according to metric a
      DOUBLE PRECISION FUNCTION snorm(v,a,n,nnx)
      IMPLICIT NONE
      INTEGER nnx,n,i,k
      DOUBLE PRECISION  v(nnx),a(nnx,nnx)
      snorm=0.d0
      DO  i=1,n
        DO  k=1,n
          snorm=snorm+v(i)*v(k)*a(i,k)
        ENDDO
      ENDDO
      snorm=sqrt(snorm/n)
      RETURN
      END
