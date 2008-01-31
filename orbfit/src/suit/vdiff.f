c
      subroutine vdiff(v,w,d)
c
c  Computes the difference vector d between the vectors v,w 
c
      implicit double precision (a-h,o-z)
      dimension v(3),w(3),d(3)
      do 3 i=1,3
 3       d(i)=v(i)-w(i)
      return
      end
