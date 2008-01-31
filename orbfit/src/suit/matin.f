c ================================================
*
*  ***************************************************************
*  *                                                             *
*  *                          M A T I N                          *
*  *                                                             *
*  *              Gauss' method for matrix inversion             *
*  *         and solution of systems of linear equations         *
*  *                                                             *
*  ***************************************************************
*
* INPUT:    A(i,j)    -   Matrix of coefficients of the system
*           N         -   Order of A(i,j)
*           L         -   Number of linear systems to be solved (the
*                         right hand  sides are stored in A(i,N+j),
*                         j=1,L)
*           NL        -   First dimension of A(i,j)
*           INVOP     -   If =1 the inverse of A(i,j) is computed
*                         explicitly,
*                         if =0 only the linear systems are solved
*
* OUTPUT:   A(i,j)    -   If INVOP=1, contains the inverse of the input matrix;
*                         if INVOP=0, contains the triangular
*                         factorization of the input matrix.
*                         In both cases A(i,N+j), j=1,L contain the
*                         solutions of the input systems.
*           DET       -   Determinant of the input matrix A(i,j)
*           ISING     -   If =0 the algorithm was completed
*                         successfully,
*                         if =1 the input matrix was singular
*
      subroutine matin(a,det,n,l,nl,ising,invop)
      implicit double precision (a-h,o-z)
      parameter (nmax=1000)
      dimension a(nl,n+l),vet(nmax)
      integer p(nmax)
      ising=1
      det=0.d0
      if(n.gt.nmax)stop ' **** matin: n > nmax ****'
      do 5 j=1,n
 5    p(j)=j
      do 4 j=1,n-1
      amax=0.d0
      do 1 i=j,n
      if(amax.ge.dabs(a(i,j)))goto 1
      npiv=i
      amax=dabs(a(i,j))
 1    continue
      if(amax.eq.0.d0)return
      if(npiv.eq.j)goto 6
      nsc=p(npiv)
      p(npiv)=p(j)
      p(j)=nsc
      do 2 i=1,n+l
      sc=a(npiv,i)
      a(npiv,i)=a(j,i)
 2    a(j,i)=sc
 6    do 3 i=j+1,n
 3    a(i,j)=a(i,j)/a(j,j)
      do 4 i=j+1,n
      do 4 k=j+1,n+l
 4    a(i,k)=a(i,k)-a(i,j)*a(j,k)
      if(l.eq.0)goto 7
      do 9 i=1,l
      do 9 j=n,1,-1
      if(j.eq.n)goto 9
      do 8 k=j+1,n
 8    a(j,n+i)=a(j,n+i)-a(j,k)*a(k,n+i)
 9    a(j,n+i)=a(j,n+i)/a(j,j)
 7    det=1.d0
      do 10 j=1,n
 10   det=det*a(j,j)
      ising=0
      if(invop.eq.0)return
      if(n.ne.1)goto 20
      a(1,1)=1.d0/a(1,1)
      return
 20   do 12 i=2,n
      do 12 j=1,i-1
      a(i,j)=-a(i,j)
      if(j+2.gt.i)goto 12
      do 11 k=j+1,i-1
 11   a(i,j)=a(i,j)-a(i,k)*a(k,j)
 12   continue
      do 15 k=n,1,-1
      do 14 i=1,n
      vet(i)=0.d0
      if(i.eq.k)vet(i)=1.d0
      if(k.gt.i)vet(i)=a(k,i)
      if(k.eq.n)goto 14
      do 13 j=k+1,n
 13   vet(i)=vet(i)-a(k,j)*a(j,i)
 14   continue
      sc=a(k,k)
      do 15 i=1,n
 15   a(k,i)=vet(i)/sc
 18   nc=0
      do 16 j=1,n
      if(j.eq.p(j))goto 16
      nsc=p(j)
      p(j)=p(nsc)
      p(nsc)=nsc
      do 17 i=1,n
      sc=a(i,nsc)
      a(i,nsc)=a(i,j)
 17   a(i,j)=sc
      nc=1
 16   continue
      if(nc.eq.1)goto 18
      return
      end
