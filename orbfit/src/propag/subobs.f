c ===========================================================
c common subroutines for preobn and fclan
c patch 1.6.1, A. Milani, May 2, 1998
c ===========================================================
c LINOBS defines line of changes in orbital elements to be used for
c confidence boundary/variations line 
c ===========================================================
      SUBROUTINE linobs(ibv,npo,eq,axes,sig,b,v,sigma,ceicel,elm,npo1)
      IMPLICIT NONE
c ====================INPUT==================================
      INTEGER ibv,npo
      DOUBLE PRECISION eq(6)
      DOUBLE PRECISION axes(2,2),sig(2),b(2,2),sigma
c matrix defining the plane of the ellipse,new orthonormal reference
      DOUBLE PRECISION ceicel(4,2),v(6,6)
c ===================OUTPUT==================================
      INTEGER npo1
      DOUBLE PRECISION elm(6,npo)
c ==================END INTERFACE============================
      INTEGER nn,n,i
      DOUBLE PRECISION s,x,y,vad(2),xv,yv,dn,dth,theta,xa,yd
      DOUBLE PRECISION eqnew(6)
      DOUBLE PRECISION alde(2),ecc
      INCLUDE 'trig.h'
c =====================================================================
c line of maximum variation: in the alpha-delta plane
      DO i=1,2
        vad(i)=axes(i,2)*sig(2)
      ENDDO
c in the elements space
      xv=(b(1,1)*vad(1)+b(1,2)*vad(2))
      yv=(b(2,1)*vad(1)+b(2,2)*vad(2))
c direction not used any more 
c     theta0=atan2(yv,xv)
c linear step for variation axis parametrisation
      dn=2.d0/float(npo-1)
c angular step for ellipse parametrisation
      dth=dpig/float(npo)
c ===========================================================
c main loop on the number of output points
      nn=0
      DO 7 n=1,npo
c ===========================================================
c choice between two output options
        IF(ibv.eq.2)THEN
c ===========================================================
c line of maximum variation in the elements space
           s=(n-1)*dn-1.d0
           x=sigma*s*xv
           y=sigma*s*yv
        ELSEIF(ibv.eq.1)THEN
c =====================================================================
c parametrisation of the ellipse in the subspace of elements, based upon the
c parametrisation of the ellipse in the alpha-delta plane
c WARNING: npo must be divisible by 2, otherwise one tip of the
c banana would be missed
           theta=(n-1)*dth
           xa=sig(1)*cos(theta)*sigma
           yd=sig(2)*sin(theta)*sigma
           CALL lincog(2,axes(1,1),xa,axes(1,2),yd,alde)
c transfer of parametrisation in the V1,V2 plane
           x=(b(1,1)*alde(1)+b(1,2)*alde(2))
           y=(b(2,1)*alde(1)+b(2,2)*alde(2))
        ELSE
           write(0,*)' linobs: this should not happen,ibv=',ibv
        ENDIF
c compute displacement on the confidence ellipsoid corresponding to x,y
        nn=nn+1    
        CALL elemov(x,y,v,ceicel,elm(1,nn))
c add to the original center of the ellipsoid of confidence
        CALL vsumg(6,eq,elm(1,nn),eqnew)
        ecc=sqrt(eqnew(2)**2+eqnew(3)**2)
        IF(ecc.ge.1.d0.or.eqnew(1).le.0.d0)THEN
           write(0,*)' Hyperbolic, ecc=',ecc,' a=',eqnew(1)
           nn=nn-1
        ELSEIF(ecc.ge.0.99d0)THEN
           write(0,*)' Almost Hyperbolic, ecc=',ecc,' a=',eqnew(1)
           nn=nn-1
        ENDIF
 7    continue
c final count of non hyperbolic orbits
      npo1=nn  
      RETURN
      END
c =====================================================================
c ELLIPS
c compute covariance ellipse of two observables
c =====================================================================
      SUBROUTINE ellips(daddet,gamm0,sig,axes,gamad)
      IMPLICIT NONE
c input covariance matrix
      DOUBLE PRECISION gamm0(6,6)
c input partial derivatives of alpha, delta, w.r. to elements (by columns)
      DOUBLE PRECISION daddet(6,2)
c output covariance
      DOUBLE PRECISION gamad(2,2),axes(2,2),sig(2)
c ==============END INTERFACE==========================================
c eigenvalues, workspace, transposed
      DOUBLE PRECISION eigval(2),tmp26(2,6),fv1(2),fv2(2),dadde(2,6)
c loop indexes
      INTEGER i
c error flag
      INTEGER ierr
c =====================================================================
      CALL transp(daddet,6,2,dadde)
      CALL mulmat(dadde,2,6,gamm0,6,6,tmp26)
      CALL mulmat(tmp26,2,6,daddet,6,2,gamad)
c =====================================================================
c compute ellipse of confidence
c eigenvalues
      CALL rs(2,2,gamad,eigval,1,axes,fv1,fv2,ierr)
      DO  i=1,2
        IF(eigval(i).gt.0.d0)THEN
           sig(i)=sqrt(eigval(i))
        ELSE
           write(0,*) 'non positive eigenvalue'
           sig(i)=0.d0
        ENDIF
      ENDDO
      RETURN
      END
c =====================================================================
c ELEMOV
c compute displacement on the confidence ellipsoid corresponding to x,y
c on the plane of the gradients of alpha-delta
c =====================================================================
      SUBROUTINE elemov(x,y,v,ceicel,del)
      IMPLICIT NONE
c inout/output
      DOUBLE PRECISION x,y,v(6,6),del(6)
      DOUBLE PRECISION ceicel(4,2)
c workspace
      DOUBLE PRECISION dee(4),deel(6)
c ===================
      CALL lincog(6,v(1,1),x,v(1,2),y,del)
      CALL lincog(4,ceicel(1,1),-x,ceicel(1,2),-y,dee)
      CALL mulmav(v(1,3),6,4,dee,4,deel)
      CALL vsumg(6,del,deel,del)
      RETURN
      END
c ===========================================================
c SLINEL
c semilinear boundary ellipse computation
c =========================================================== 
      SUBROUTINE slinel(dtpdet,gc,cc,ceicel,b,v)
      IMPLICIT NONE
c 6 by 2 matrix with columns= gradients
      DOUBLE PRECISION dtpdet(6,2)
c normal and covariance matrices
      DOUBLE PRECISION gc(6,6),cc(6,6)
c orthonormal basis
      DOUBLE PRECISION v(6,6),vt(6,6),gamv(6,6),cv(6,6),tmp(6,6)
c partial matrices
      DOUBLE PRECISION c4(4,4),cinv(4,4),c42(4,2),ceicel(4,2)
c line of maximum variation
      DOUBLE PRECISION a(2,2),b(2,2),deta
c loop indexes ii=1,2, ij,ijj=1,4
      INTEGER ii, ij, ijj
c for inversion with tcholevski: workspace, error flag
      DOUBLE PRECISION ws(4)
      INTEGER ierr
      DOUBLE PRECISION prscag
c =====================================================================
c adapted orthonormal basis, covariance and normal matrix in the new basis
      CALL graha(dtpdet,6,v)
      CALL transp(v,6,6,vt)
      CALL mulmat(vt,6,6,gc,6,6,tmp)
      CALL mulmat(tmp,6,6,v,6,6,gamv)
      CALL mulmat(vt,6,6,cc,6,6,tmp)
      CALL mulmat(tmp,6,6,v,6,6,cv)
c =====================================================================
c 4x4 and 4x2 submatrices of normal matrix
      do 15 ijj=1,4
        DO ij=1,4
          c4(ijj,ij)=cv(ijj+2,ij+2)
        ENDDO
        DO  ii=1,2
          c42(ijj,ii)=cv(ijj+2,ii)
        ENDDO
 15   continue
c ===========================================================
c Cholewski method for inversion
      CALL tchinv(c4,4,cinv,ws,ierr)
      IF(ierr.ne.0)THEN
         write(0,*)' decide what to do, ierr=',ierr
      ENDIF
c ===========================================================
c matrix to be used for out of plane component
      CALL mulmat(cinv,4,4,c42,4,2,ceicel)
c ===========================================================
c linear map from the elements space (with base V) and the alpha-delta plane 
      a(1,1)=prscag(6,dtpdet(1,1),v(1,1))
      a(1,2)=prscag(6,dtpdet(1,1),v(1,2))
      a(2,1)=prscag(6,dtpdet(1,2),v(1,1))
      a(2,2)=prscag(6,dtpdet(1,2),v(1,2))
      CALL inv22(a,b,deta)
      RETURN
      END
c **********************************************************
c  ANGUPD
c   given the principal value ang of an angle, and an angle vang 
c   with ng revolutions, the routine 
c   finds a new new ng in the assumption that
c   less than half a revolution has occurred; ang is modified to 
c   include the multiples of dpig
c   in case ang is not between -pig and pig, e.g. because it is 
c   between 0 and dpig, it is first reduced to principal value
      SUBROUTINE angupd(ang,vang,ng)
      IMPLICIT NONE
      DOUBLE PRECISION ang,vang,d
      INTEGER ng,ig
      INCLUDE 'trig.h'
c first reduce to principal value
      if(ang.gt.pig)ang=ang-dpig
      if(ang.lt.-pig)ang=ang+dpig
c count revolutions
      ig=nint((vang-ang)/dpig)
      ang=ang+dpig*ig
      d=ang-vang
c correct by one revolution if necessary
      if(d.gt.pig)then
         ng=ng-1
         ang=ang-dpig
      elseif(d.lt.-pig)then
         ng=ng+1
         ang=ang+dpig
      endif
      return
      end
