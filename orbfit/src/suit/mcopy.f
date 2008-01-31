c ======================================
c MCOPY - matrix copy for n x m matrix
c
      subroutine mcopy(n,m,a,b)
      implicit none
      integer n,m,i,j
      double precision a(n,m),b(n,m)
      do   i=1,n
        do   j=1,m
          b(i,j)=a(i,j)
        enddo
      enddo
      return
      end
