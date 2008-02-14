c ================================================
c  INVMAT3  3X3 matrix inversion 
c in this version it is assumed that the input matrix a is symmetric,
c definite positive,
c and the output matrix is symmetric, definite positive
c If this is not the case, a warning is issued on the standard ouput
c ===========INTERFACE=============================
      subroutine invmat3(c,a)
      implicit none
      integer ndim
      parameter (ndim=3)
      double precision a(ndim,ndim),c(ndim,ndim)
c ========END INTERFACE=============================
      double precision an(ndim),v(ndim)
      integer i,j,indp
      double precision err,omax,omin,cond
      logical sym
c check that the matrix is indeed symmetric
      sym=.true.
      do  i=1,ndim
        do  j=1,i-1
          if(a(i,j).ne.a(j,i))sym=.false.
        enddo
      enddo
      if(.not.sym)write(0,*)'invmat: input matrix not symmetric'
c ==========================================================
c Tcholewski
c ==========================================================
c normalisation of columns of matrix to be inverted
      do  i=1,ndim
        an(i)=sqrt(abs(a(i,i)))
      enddo
      do  i=1,ndim
        do  j=1,ndim
          c(i,j)=a(i,j)/(an(i)*an(j))
        enddo
      enddo
c first Cholewsky factorisation
      err=1.d-18
      call tchol(c,ndim,ndim,indp,err)
      if(indp.eq.0)then
c Control of conditioning number of the inverted matrix
        omax=c(1,1)
        omin=c(1,1)
        do  i=2,ndim
          if (c(i,i).gt.omax) then
             omax=c(i,i)
          endif
          if (c(i,i).lt.omin) then
              omin=c(i,i)
          endif
        enddo
        cond=(omax/omin)**2
        write(0,100)cond
 100    format(' invmat: condition number ',d10.3)
        call inver(c,v,ndim,ndim)
c unnormalize the matrix by norm of columns
        do  i=1,ndim
        do  j=1,ndim
          c(i,j)=c(i,j)/(an(i)*an(j))
        enddo
        enddo
      else
        write(0,*)' matrix is not positive definite'
        write(0,*)' pivot number ',indp,' is ',c(indp,indp)
      endif
      return
      end
c ===========================================================
c                                                               
c                            t c h o l                          
c                                                               
c      Tcholesky factorization of a positive-defined matrix     
c                                                               
c             originally written by prof. Luigi Mussio                
c         Istituto di Topografia del Politecnico di Milano     
c                                                               
c ===========================================================
c
c    warning: only the upper triangle of the matrix is processed
c
c
c input:    a(nmax,nmax)   - matrix to be processed
c           nmax           - first dimension of a as declared in the dim
c                            statement of the calling program
c           n              - actual dimension of a
c           err            - control on pivot (if <0, control is automatic)
c
c
c output:   a              - contains the tcholesky factorization of the
c                            matrix (to be supplied to subroutine inver)
c           indp           - if it is different from zero, the input mat
c                            was not positive defined and the algorithm
c                            does not work
c
      subroutine tchol(a,nmax,n,indp,err)
      implicit none
      integer n,nmax,indp
      double precision a(nmax,n),err
c ========END INTERFACE=========================
      double precision as,errl,s
      integer i,k,ii,l,j
      errl=err
      as=0.d0
      do 1 i=1,n
      do 1 k=1,i
        as=max(as,abs(a(i,k)))
 1    continue
c in this version we are not checking the roundoff value
c of the machine, beacuse the routine roff is too easily machine dependent
c thus err must be positive 
      errl=errl*as 
      do 40 i=1,n
        l=i-1
        s=0.d0
        if(l.eq.0) goto 15
        do  k=1,l
          s=s+a(k,i)*a(k,i)
        enddo
   15   a(i,i)=a(i,i)-s
        if(a(i,i).le.errl)then
           indp=i
           return
        endif
        a(i,i)=sqrt(a(i,i))
        if(i.eq.n) goto 40
        ii=i+1
        do 30 j=ii,n
          s=0.d0
          if(l.eq.0)goto 25
          do  k=1,l
            s=s+a(k,i)*a(k,j)
          enddo
   25     a(i,j)=(a(i,j)-s)/a(i,i)
   30   continue
   40 continue
      indp=0
      return
      end
c ===========================================================
c                                                               
c                            i n v e r                          
c                                                               
c              inversion of a positive-defined matrix           
c                                                               
c                  written by prof. luigi mussio                
c         istituto di topografia del politecnico di milano      
c                                                               
c
c input:    a(nmax,nmax)   - input matrix, previously factorized by
c                            subroutine tchol
c           v(nmax)        - work area
c           nmax           - first dimension of a as declared in the dim
c                            statement of the calling program
c           n              - actual dimension of a
c
c
c output:   a              - inverse of input matrix
c 
c
      subroutine inver(a,v,nmax,n)
      implicit none
      integer nmax,n
      double precision a(nmax,n),v(n)
c ==========END INTERFACE=========
      integer i,l,k,j
      double precision s
c ================================
      do 8 i=n,1,-1
        l=i+1
        if(i.eq.n)goto 4
        do 3 j=n,l,-1
          s=0.d0
          do 2 k=n,l,-1
            if(j.lt.k)goto 1
            s=s+a(i,k)*a(k,j)
            goto 2
    1       s=s+a(i,k)*a(j,k)
 2        continue
          v(j)=-s/a(i,i)
    3   continue
    4   s=0.d0
        if(i.eq.n)goto 7
        do  k=n,l,-1
          s=s+a(i,k)*v(k)
        enddo
        do  j=n,l,-1
          a(i,j)=v(j)
        enddo
    7   a(i,i)=(1.d0/a(i,i)-s)/a(i,i)
    8 continue
      do 9 i=2,n
        do 9 j=1,i-1
    9     a(i,j)=a(j,i)
      return
      end




