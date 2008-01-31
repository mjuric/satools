c ====================================================================
c Graham- Schmidt procedure to generate an orthonormal basis v
c starting from 2  n-vectors a
c The new basis must be such that the first 2 vectors are a basis
c for the space spanned by the 2 columns of a
      subroutine graha(a,n,v)
      implicit none
      integer n,nx
      parameter (nx=10)
      double precision a(n,2),v(n,n)
      integer j,jok,jj
      double precision prscag,cc,cc1,cc2,epsi,vl
      double precision ws(nx)
      logical ize
c dimension check
      if(n.gt.nx)then
         write(*,*)'n =',n,' larger than nx=',nx,' in graha'
         stop
      endif 
c selection of the control for "zero" vectors
      cc1=sqrt(prscag(n,a(1,1),a(1,1)))
      cc2=sqrt(prscag(n,a(1,2),a(1,2)))
      epsi=1.d-12*min(cc1,cc2)
      if(epsi.eq.0.d0)then
         write(*,*)' a has rank zero'
c        stop
      endif
c start by orthonormalisation of the space spanned by the columns of a
c
c V1 is the versor of A1
      call versor(n,a(1,1),epsi,v(1,1),vl,ize)
      if(ize)then
         write(*,*)' first vector of a is too small'
c        stop
      endif 
c the following vectors are obtained
c by removing the components along the previous ones
      cc=-prscag(n,v(1,1),a(1,2))
      call lincog(n,a(1,2),1.d0,v(1,1),cc,v(1,2))
      call versor(n,v(1,2),epsi,v(1,2),vl,ize)
      if(ize)then
         write(*,*)' a has practically rank one'
c        stop
      endif
c we now use the vectors of the canonic basis to supplement the span of A1,A2
      jok=0
      do 1 j=1,n
c remove the components along span(A), that is along V1 and V2
        cc1=-v(j,1)
        cc2=-v(j,2)
        call lincog(n,v(1,1),cc1,v(1,2),cc2,ws)
        ws(j)=ws(j)+1.d0
        call versor(n,ws,epsi,v(1,3+jok),vl,ize)
        if(.not.ize)then
c now V(3+jok) is orthogonal to span(A); remove the components along
c the previous ones (unless it is the first)
           if(jok.gt.0)then
              do  jj=1,jok
                cc=-prscag(n,v(1,3+jok),v(1,2+jj))
                call lincog(n,v(1,3+jok),1.d0,v(1,2+jj),cc,v(1,3+jok))
              enddo
              call versor(n,v(1,3+jok),epsi,v(1,3+jok),vl,ize)
              if(ize)then
                 goto 1
              endif
           endif
c the new versor is a good one
           jok=jok+1
           if(jok.eq.n-2)then
              goto 2
           endif
        endif
 1    continue
 2    continue
      if(jok.lt.n-2)then
         write(*,*)' something went wrong, jok=',jok
      endif
      return
      end
      subroutine versor(n,a,epsi,b,vl,ize)
      implicit none
      integer n,i
      logical ize
      double precision a(n),b(n),epsi,prscag,vl
      vl=sqrt(prscag(n,a,a))
      if(vl.lt.epsi)then
         ize=.true.
      else
         ize=.false.
         do  i=1,n
           b(i)=a(i)/vl
         enddo
      endif
      return
      end
