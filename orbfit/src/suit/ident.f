c==============================================
c identitiy matrix ndimxndim in output a
         subroutine ident(ndim,a)
         implicit none
         integer ndim,i,j
         double precision a(ndim,ndim)
         do  i=1,ndim
           do  j=1,ndim
             if(i.eq.j)then
                a(i,j)=1.d0
             else
                a(i,j)=0.d0
             endif
           enddo
         enddo
         return
         end
