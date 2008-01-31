c =====================================================================
c WEAKDI
c =====================================================================
c  weak direction and sigma
c   input:       gamma = covariance matrix
c                iun8  = output unit (if positive)
c   output:      wdir  = weak direction vector
c                sdir  = sigma
c ============ INTERFACE====================
      subroutine weakdi(gamma,wdir,sdir,iun8)
      implicit none
      double precision gamma(6,6), wdir(6),sdir
      integer iun8
c ========END INTERFACE====================
c error flag
      integer ierr
c loop index
      integer i
c eigenvalues, eigenvectors, workspace
      double precision eigvec(6,6),eigval(6),fv1(6),fv2(6)
c eigenvalues
      call rs(6,6,gamma,eigval,1,eigvec,fv1,fv2,ierr)
      call vcopy(6,eigvec(1,6),wdir)
      sdir=sqrt(eigval(6))
      if(iun8.gt.0)then
          write(iun8,*)
          call tee(iun8,'WEAK DIRECTION =')
          write(iun8,109) (wdir(i),i=1,6)
 109      format(6e24.16)
          write(*,110) (wdir(i),i=1,6)
 110      format(1p,6e12.4)
          write(iun8,*)
          write(iun8,111) sdir
          write(*,111) sdir
 111      format(' WEAK SIGMA ',1p,e12.4)
          write(iun8,*)
      endif
      return 
      end
