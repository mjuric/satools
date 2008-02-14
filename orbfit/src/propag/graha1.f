c ====================================================================
c Graham- Schmidt procedure to generate an orthonormal basis v
c starting from 1  n-vector a
c The new basis must be such that the first  vector is a
      subroutine graha1(a,n,v)
      implicit none
      integer n,nx
      parameter (nx=10)
      double precision a(n),v(n,n)
      integer j,jok,jj,i
      double precision prscag,cc,cc1,epsi,vl
      double precision ws(nx)
      logical ize
c dimension check
      if(n.gt.nx)then
         write(0,*)'n =',n,' larger than nx=',nx,' in graha'
         stop
      endif 
c selection of the control for "zero" vectors
      cc1=sqrt(prscag(n,a,a))
      epsi=1.d-12*cc1
      if(epsi.eq.0.d0)then
         write(0,*)' a has rank zero'
c        stop
      endif
c
c V1 is the versor of A
      call versor(n,a,epsi,v(1,1),vl,ize)
      if(ize)then
         write(0,*)' first vector of a is too small'
c        stop
      endif 
c we now use the vectors of the canonic basis to supplement the span of A1,A2
      jok=0
      do 1 j=1,n
c remove the components along span(A), that is along V1 
        cc1=-v(j,1)
        do  i=1,n
          ws(i)=cc1*v(i,1)
        enddo
        ws(j)=ws(j)+1.d0
        call versor(n,ws,epsi,v(1,2+jok),vl,ize)
        if(.not.ize)then
c now V(3+jok) is orthogonal to span(A); remove the components along
c the previous ones (unless it is the first)
           if(jok.gt.0)then
              do  jj=1,jok
                cc=-prscag(n,v(1,2+jok),v(1,1+jj))
                call lincog(n,v(1,2+jok),1.d0,v(1,1+jj),cc,v(1,2+jok))
              enddo
              call versor(n,v(1,2+jok),epsi,v(1,2+jok),vl,ize)
              if(ize)then
                 goto 1
              endif
           endif
c the new versor is a good one
           jok=jok+1
           if(jok.eq.n-1)then
              goto 2
           endif
        endif
 1    continue
 2    continue
      if(jok.lt.n-1)then
         write(0,*)' something went wrong, jok=',jok
      endif
      return
      end









