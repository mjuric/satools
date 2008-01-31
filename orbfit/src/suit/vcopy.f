c ======================================
c VCOPY - vector copy for n-vector
c
      subroutine vcopy(n,a,b)
      implicit none
      integer n,i
      double precision a(n),b(n)
      do  i=1,n
        b(i)=a(i)
      enddo
      return
      end
