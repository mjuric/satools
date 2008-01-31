c ===========================================================
c
c                     M I N S O L
c version 1.3, A. Milani, 28/6/1997
c
c least squares solver, including optional handling of second
c derivatives and normal matrix normalisation
c      INPUT :   csi   =  vector of residuals (of alpha and delta)
c                no    =  double of observations number
c                w     =  weight matrix 
c                g     =  matrix of first derivatives of residuals
c                          multiplied by -1
c                h     =  matrix of second derivatives of residuals
c                          multiplied by -1
c                inew  = if inew=2, h is not used; if inew=1, h is used
c                icor(k)=  .ne.0 to correct this element, 0 to let it as it was
c                iunf   = unit for output file
c      OUTPUT:   gtwg  =  normal matrix GtWG 
c                dx0   =  corrections vector
c                gamma =  inverse matrix
c                cond  =  conditioning number of gamma
c           warning: if only some elements are corrected, only the 
c              corresponding entries in gamma and gtwg are nonzero in output
c ===========================================================
      subroutine minsol(csi,no,w,g,h,inew,icor,iunf,
     +                  gtwg,dx0,gamma,cond)
c ===========================================================
      implicit none
c ===========================================================
      include 'parobx.h'
c dimension of elements vector
      INTEGER nd
      parameter (nd=6)
c unit for output file
      INTEGER iunf
c controls for corrections, no. of solve for variables
      INTEGER icor(nd),ndc
c number of (scalar) observations
      INTEGER no
c residuals, weights,derivatives
      DOUBLE PRECISION csi(no),w(no),g(nob2x,nd),h(nob2x,nd,nd)
c output: normal matrix, differential correction, covariance matrix
      DOUBLE PRECISION gtwg(nd,nd),dx0(nd),gamma(nd,nd)
c reduced normal matrix and covariance matrix
      DOUBLE PRECISION cr(nd,nd),gr(nd,nd)
c right hand side of the normal equation, normalisation factors
      DOUBLE PRECISION gtwcsi(nd)
c condition number, control for positive-definite
      DOUBLE PRECISION cond
c loop indexes: j,k=1,nd; i=1,no; jj,kk=1,ndc
      INTEGER j,k,i,jj,kk
c controls: newton/pseudo newton 
      INTEGER inew
c temporary variables and workspace 
      DOUBLE PRECISION tmp
c ===========================================================
c control on dimensioning
      if (no.gt.nob2x) THEN
         write(*,*) 'no>nob2x in minsol',no,nob2x
         stop
      ENDIF
c ===========================================================
c normal matrix GtWG of the pseudo-Newton method
      DO 1 j=1,nd
        DO 2 k=1,nd
          gtwg(j,k)=0.d0
          IF(icor(k).ne.0.and.icor(j).ne.0)THEN
             DO  i=1,no
               gtwg(j,k)=gtwg(j,k)+g(i,j)*w(i)*g(i,k)
             ENDDO
          ENDIF
 2      CONTINUE
 1    CONTINUE
c ===========================================================
c Adding second derivatives for full Newton's method
      IF(inew.eq.1)THEN
         DO 24 j=1,nd
           DO 26 k=1,nd
             tmp=0.d0
             IF(icor(k).ne.0.and.icor(j).ne.0)THEN
                DO  i=1,no
                  tmp=tmp-csi(i)*w(i)*h(i,j,k)
                ENDDO
             ENDIF
             gtwg(j,k)=gtwg(j,k)+tmp
 26        CONTINUE
 24      CONTINUE
      ENDIF
c ===========================================================
c simplyfied version if 6 elements to be determined
      ndc=0
      DO  j=1,nd
        IF(icor(j).gt.0) ndc=ndc+1
      ENDDO
      IF(ndc.eq.6)THEN
         CALL invmat(gamma,nd,nd,gtwg,cond)
         goto 78
      ENDIF
c ===========================================================
c squeeze normal matrix into a smaller one
      jj=0
      DO 7 j=1,nd
        IF(icor(j).ne.0)THEN
           jj=jj+1
           kk=0
           DO 8 k=1,nd
             IF(icor(k).ne.0)THEN
                kk=kk+1
                cr(jj,kk)=gtwg(j,k)
             ENDIF
 8         CONTINUE
        ENDIF
 7    CONTINUE
      IF(jj.eq.kk.and.jj.ne.0)THEN
         ndc=jj
      ELSE
         write(*,*)' minsol, this should not happen ',jj,kk
         stop
      ENDIF        
c ===========================================================
c Cholewski inversion
      CALL invmat(gr,nd,ndc,cr,cond)
c ===========================================================
c  covariance matrix
c  warning: rows and columns not involved in the correction
c  are set to zero (should be infinite!)
      kk=0
      DO 14 k=1,nd
        IF(icor(k).ne.0)THEN
           kk=kk+1
           jj=0
           DO 16 j=1,nd
             IF(icor(j).ne.0)THEN
                jj=jj+1
                gamma(k,j)=gr(kk,jj)
             ELSE
                gamma(k,j)=0.d0
             ENDIF
 16        CONTINUE
        ELSE
           DO  j=1,nd
             gamma(k,j)=0.d0
           ENDDO
        ENDIF
 14   CONTINUE
c =======================
 78   continue
      if(cond.gt.1.d12)write(iunf,100)cond
 100  format('Conditioning number: ',1p,d12.5)
c ===========================================================
c Computation of vectors GtWcsi and DX0
c   warning: correction is zero automatically for non solved-for variables 
       DO 17 j=1,nd
         gtwcsi(j)=0.d0
         DO  i=1,no
           gtwcsi(j)=gtwcsi(j)+g(i,j)*w(i)*csi(i)
         ENDDO
 17    CONTINUE
       DO 18 k=1,nd
         dx0(k)=0.d0
         DO j=1,nd
           dx0(k)=dx0(k)+gamma(k,j)*gtwcsi(j)
         ENDDO
 18    CONTINUE
       return
       end
c ================================================
c  INVMAT  ndim x ndim  matrix inversion 
c in this version it is assumed that the input matrix a is symmetric,
c definite positive;
c and the output matrix is symmetric, definite positive
c If this is not the case, a warning is issued on the standard ouput
c ===========INTERFACE=============================
      subroutine invmat(c,nx,ndim,a,cond)
      implicit none
      INTEGER nx,ndim
      DOUBLE PRECISION a(nx,ndim),c(nx,ndim),cond
c ========END INTERFACE=============================
      INTEGER nxx
c warning: here it is assumed that ndim never exceeds 6; 
c to be changed for bigger matrices
      parameter(nxx=6)
      DOUBLE PRECISION an(nxx),v(nxx)
      INTEGER i,j,indp,nb
      DOUBLE PRECISION err,omax,omin,da,eps
      DOUBLE PRECISION roff
      logical sym
c ==================================================
c check that the matrix is indeed symmetric
      sym=.true.
* Changed to 1.d4 by S. Chesley February 1999
      eps=1.d2*roff(nb)
      DO 1 i=1,ndim
        DO 2 j=1,i-1
          da=abs((a(i,j)-a(j,i))/(a(i,j)+a(j,i)))
          IF(da.gt.100*eps)THEN
             write(*,*)'invmat: ',i,j,a(i,j),a(j,i),da,100*eps
             sym=.false.
          ENDIF
 2      CONTINUE
 1    CONTINUE
      IF(.not.sym)write(*,*)'invmat: input matrix not symmetric'
c ==========================================================
c Tcholewski
c ==========================================================
c normalisation of columns of matrix to be inverted
      DO  i=1,ndim
        an(i)=sqrt(abs(a(i,i)))
      ENDDO
      DO 88 i=1,ndim
        DO  j=1,ndim
          c(i,j)=a(i,j)/(an(i)*an(j))
        ENDDO
 88   CONTINUE
      DO 86 i=ndim+1,nx
        DO  j=1,ndim
          c(i,j)=0.d0
        ENDDO
 86   CONTINUE
c first Cholewsky factorisation
      err=eps
      CALL tchol(c,nx,ndim,indp,err)
      IF(indp.eq.0)THEN
c Control of conditioning number of the inverted matrix
        omax=c(1,1)
        omin=c(1,1)
        DO 5 i=2,ndim
          if (c(i,i).gt.omax) THEN
             omax=c(i,i)
          ENDIF
          if (c(i,i).lt.omin) THEN
              omin=c(i,i)
          ENDIF
 5      CONTINUE
        cond=(omax/omin)**2
        CALL inver(c,v,nx,ndim)
c unnormalize the matrix by norm of columns
        DO  i=1,ndim
          DO  j=1,ndim
            c(i,j)=c(i,j)/(an(i)*an(j))
          ENDDO
        ENDDO
      ELSE
        write(*,*)' matrix is not positive definite'
        write(*,*)' pivot number ',indp,' is ',c(indp,indp)
      ENDIF
      RETURN
      END

