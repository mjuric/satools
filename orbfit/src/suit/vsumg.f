c =======================================================
c  Computes the sum vector d between the n-vectors v,w 
c
      subroutine vsumg(n,v,w,d)
      implicit none
      integer n,i
      double precision v(n),w(n),d(n)
      do 3 i=1,n
 3       d(i)=v(i)+w(i)
      return
      end
