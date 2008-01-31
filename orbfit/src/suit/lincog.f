c =======================================================
c linear combination of general n vectors
      subroutine lincog(n,v1,a,v2,b,v3)
c
c    Computes the linear combination vector v3 of the 
c    n-elements vectors v1,v2 with coefficients a,b
c
      implicit none
      integer n,i
      double precision v1(n),v2(n),v3(n),a,b
      do 40 i=1,n
        v3(i)=a*v1(i)+b*v2(i)
40    continue
      return
      end
