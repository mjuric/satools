*
*  ***************************************************************
*  *                                                             *
*  *                          P R V E C                          *
*  *                                                             *
*  *                 Vector product:   C = A x B                 *
*  *                                                             *
*  ***************************************************************
*
      subroutine prvec(a,b,c)
      implicit double precision (a-h,o-z)
      dimension a(3),b(3),c(3)
      c(1)=a(2)*b(3)-a(3)*b(2)
      c(2)=a(3)*b(1)-a(1)*b(3)
      c(3)=a(1)*b(2)-a(2)*b(1)
      return
      end
