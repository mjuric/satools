c =======================================================
c  genneral scalar product of n-vectors
      double precision function prscag(n,a,b)
      implicit none
      integer n,i
      double precision a(n),b(n)
      prscag=0.d0
      do  i=1,n
        prscag=prscag+a(i)*b(i)
      enddo
      return
      end
