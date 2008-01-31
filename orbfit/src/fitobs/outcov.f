c =====================================================================
c OUTCOV
c =====================================================================
c  output of covariance and normal matrix, computation of eigenvalues
c   input: iun   = output unit
c          icor  = flags >0 solved for varaiable
c          gamma = covariance matrix
c          c     = normal matrix
c WARNING: if not all the variables are solved for, the corresponding
c          rows and columns of both gamma and c are set to zero.
c          These null lines are removed in the output.
      subroutine outcov(iun,icor,gamma,c)
      implicit none
c output unit, error flag
      integer iun,ierr
c which variables have been solved for
      integer icor(6),ncor
c loop indexes
      integer i,j,ii,jj
c covariance and normal matrices
      double precision gamma(6,6),c(6,6)
      double precision gam(36),cc(36),eigv(36)
c eigenvalues, eigenvectors
      double precision eigval(6),fv1(6),fv2(6)
c packing of matrices: no. lines and columns
      ncor=0
      do 1 i=1,6
        if(icor(i).gt.0)ncor=ncor+1
 1    continue
      if(ncor.eq.0)then
         write(*,*)' no correction, icor=',icor
         return
      endif
      write(iun,*)
      write(iun,*)'no. solve for=',ncor
      write(iun,*)icor
c packing of matrices:
      ii=0
      do 10 i=1,6
        jj=0
        if(icor(i).gt.0)then
          ii=ii+1
          do 11 j=1,6
            if(icor(j).gt.0)then
               jj=jj+1
               gam(ii+(jj-1)*ncor)=gamma(i,j)
               cc(ii+(jj-1)*ncor)=c(i,j)
            endif
 11       continue
        endif
 10   continue
c output covariance
      write(iun,*)
      write(iun,*) 'COVARIANCE MATRIX'
      do 2 j=1,ncor
        write(iun,109) (gam(i+(j-1)*ncor),i=1,ncor)
 109    format(6e24.16)
 2    continue
c eigenvalues
      call rs(ncor,ncor,gam,eigval,1,eigv,fv1,fv2,ierr)
      write(iun,*)
      write(iun,*) 'EIGENVALUES '
      write(iun,109) (eigval(i),i=1,ncor)
      write(iun,*)
      write(iun,*) 'EIGENVECTORS'
      do 3 j=1,ncor
        write(iun,109) (eigv(i+(j-1)*ncor),i=1,ncor)
 3    continue
c normal matrix
      write(iun,*)
      write(iun,*) 'NORMAL MATRIX'
      do 4 j=1,ncor
        write(iun,109) (cc(i+(j-1)*ncor),i=1,ncor)
 4    continue
      write(iun,*)
      return 
      end
c =====================================================================
c OUTCO
c =====================================================================
c  output of covariance, computation of eigenvalues
c   input: iun   = output unit
c          gamma = covariance matrix
c          c     = normal matrix
      subroutine outco(iun,gamma,c)
      implicit none
c output unit, error flag
      integer iun,ierr
c loop indexes
      integer i,j
c covariance, normal matrix
      double precision gamma(6,6),c(6,6)
c eigenvalues, eigenvectors
      double precision eigvec(6,6),eigval(6),fv1(6),fv2(6)
c output covariance
      write(iun,*)
      write(iun,*) 'COVARIANCE MATRIX'
      do j=1,6
        write(iun,109) (gamma(i,j),i=1,6)
 109    format(6e24.16)
      enddo
c eigenvalues
      call rs(6,6,gamma,eigval,1,eigvec,fv1,fv2,ierr)
      write(iun,*)
      write(iun,*) 'EIGENVALUES '
      write(iun,109) (eigval(i),i=1,6)
      write(iun,*)
      write(iun,*) 'EIGENVECTORS'
      do 3 j=1,6
c by columns (check)
        write(iun,109) (eigvec(i,j),i=1,6)
 3    continue
      write(iun,*)
      write(iun,*) 'NORMAL MATRIX'
      do j=1,6
        write(iun,109) (c(i,j),i=1,6)
      enddo
      return 
      end
