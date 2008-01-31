c =====================================================================
c inverse of a 2x2 matrix
      subroutine inv22(a,b,deta)
      implicit none
      double precision a(2,2),b(2,2),deta
      deta=a(1,1)*a(2,2)-a(1,2)*a(2,1)
      if(deta.eq.0.d0)then
         write(*,*)' deta is zero '
      endif
      b(1,1)=a(2,2)/deta
      b(2,2)=a(1,1)/deta
      b(1,2)=-a(1,2)/deta
      b(2,1)=-a(2,1)/deta
      return
      end
