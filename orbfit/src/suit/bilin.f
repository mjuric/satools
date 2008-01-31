c =======================================================
c computation of a bilinear form
      double precision function bilin(v1,v2,nv,w,nx,n)
      implicit none
      integer nv,nx,n
      double precision v1(nv),v2(nv),w(nx,n)
      integer i,j
c  control on dimensions
      if(nv.ne.n)write(*,*)' dimension quadr, nv=',nv,' ne n=',n
      if(n.gt.nx)write(*,*)' dimension quadr, n=',n,' gt nx=',nx
      bilin=0.d0
      do 10 i=1,n
      do 10 j=1,n
 10   bilin=bilin+v1(i)*w(i,j)*v2(j)
      return
      end
