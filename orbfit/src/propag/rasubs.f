c ================================================
c rasust
c
c iterative convergence of the right hand sides at intermediate points
c ================================================
      SUBROUTINE rasust(m,t,t2,tm,tini,x,v,b,f1,nv,ncl,npq
     +        ,g,epsi,nf)
      IMPLICIT NONE
c iteration number
      INTEGER m
c control to be used for convergence
      DOUBLE PRECISION epsi
c dimension of state vector
      INTEGER nv
c logical flag for first order diff. eq., for second order diff.eq.
      LOGICAL ncl,npq

      DOUBLE PRECISION x(nv),v(nv)
c right hand side at begining of step
      DOUBLE PRECISION f1(nv)
c storage arrays
      DOUBLE PRECISION b(7,nv),g(7,nv)
c times
      DOUBLE PRECISION t,t2,tm,tini      
c =====================END INTERFACE===========================
c constants of the integration method
      DOUBLE PRECISION h(8),w(7),u(7),c(21),d(21),r(21),w1
      COMMON/raconst/h,w,u,c,d,r,w1
c state variables 
      INTEGER ndx,nvx
      PARAMETER (ndx=60,nvx=2*ndx)
c x,v temporary
      DOUBLE PRECISION y(nvx),z(nvx)
c error control: convergence
      INCLUDE 'parint.h'
      INCLUDE 'comint.h'
      DOUBLE PRECISION ck(nvx,8),fj(nvx)
c total number of function calls
      INTEGER nf
c loop indexes
      INTEGER j,k,jd
c scalar temporaries
      DOUBLE PRECISION q,s,temp,gk
c close approach flags: detected, which planet
      INTEGER idc
c positon and velocities of planet which has close-encounter
c whith the asteroid
      DOUBLE PRECISION xpla(6)
c memory model static
      SAVE
c initialize previous iteration array to zero if first iteration
      IF(m.eq.1)THEN
         DO  k=1,nv
            DO j=2,8
               ck(k,j)=0.d0
            ENDDO
         ENDDO
      ENDIF
      epsi=0.d0 
c =========do on substep========================= 
c  loop 174 is for each substep within a sequence.
      do 174 j=2,8
          jd=j-1
c         jdm=j-2
          s=h(j)
          q=s
          IF(ncl) q=1.d0
c ===============================================================
c  use eqs. (2.9) and (2.10) of text to predict positions at each substep.
c  these collapsed series are broken into two parts because an otherwise
c  excellent  compiler could not handle the complicated expression.
c ===============================================================
          do 130 k=1,nv
            temp=w(3)*b(3,k)+s*(w(4)*b(4,k)+s*(w(5)*b(5,k)
     +        +s*(w(6)*b(6,k)+s*w(7)*b(7,k))))
            y(k)=x(k)+q*(t*v(k)+t2*s*(f1(k)*w1+s*(w(1)*b(1,k)
     +          +s*(w(2)*b(2,k)+s*temp))))
            IF(.not.npq)then
c  next are calculated the velocity predictors need for general class ii.
               temp=u(3)*b(3,k)+s*(u(4)*b(4,k)+s*(u(5)*b(5,k)
     +              +s*(u(6)*b(6,k)+s*u(7)*b(7,k))))
               z(k)=v(k)+s*t*(f1(k)+s*(u(1)*b(1,k)+s*(u(2)*b(2,k)
     +             +s*temp)))
            ENDIF
 130      continue
c ==================================================================
c  find forces at each substep.
c ==================================================================
          CALL force(y,z,tm+s*t+tini,fj,nv,idc,xpla)
          do  k=1,nv
            epsi=epsi+dabs(fj(k)-ck(k,j))
          enddo
          do  k=1,nv
            ck(k,j)=fj(k)
          enddo
          nf=nf+1
c ================do on components===================
          do 171 k=1,nv
c ===============================================================
c  find g-value for the force fj found at the current substep. this
c  section, including the many-branched goto, uses eq. (2.4) of text.
c ===============================================================
            temp=g(jd,k)
            gk=(fj(k)-f1(k))/s
            IF(j.le.2)then
               g(1,k)=gk
            ELSEIF(j.eq.3)then
                g(2,k)=(gk-g(1,k))*r(1)
            ELSEIF(j.eq.4)then
                g(3,k)=((gk-g(1,k))*r(2)-g(2,k))*r(3)
            ELSEIF(j.eq.5)then
                g(4,k)=(((gk-g(1,k))*r(4)-g(2,k))*r(5)-g(3,k))*r(6)
            ELSEIF(j.eq.6)then
                g(5,k)=((((gk-g(1,k))*r(7)-g(2,k))*r(8)-g(3,k))*r(9)-
     +                    g(4,k))*r(10)
            ELSEIF(j.eq.7)then
                g(6,k)=(((((gk-g(1,k))*r(11)-g(2,k))*r(12)
     +                -g(3,k))*r(13)-g(4,k))*r(14)-g(5,k))*r(15)
            ELSEIF(j.eq.8)then
                g(7,k)=((((((gk-g(1,k))*r(16)-g(2,k))*r(17)
     +                -g(3,k))*r(18)-g(4,k))*r(19)
     +                -g(5,k))*r(20)-g(6,k))*r(21)
            ENDIF
c ===============================================================
c  upgrade all b-values
c ===============================================================
            temp=g(jd,k)-temp
            b(jd,k)=b(jd,k)+temp
c ===============================================================
c  temp is now the improvement on g(jd,k) over its former value.
c  now we upgrade the b-value using this dfference in the one term.
c  this section is based on eq. (2.5).
c ===============================================================
            IF(j.eq.3)then
                b(1,k)=b(1,k)+c(1)*temp
            ELSEIF(j.eq.4)then
                b(1,k)=b(1,k)+c(2)*temp
                b(2,k)=b(2,k)+c(3)*temp
            ELSEIF(j.eq.5)then
                b(1,k)=b(1,k)+c(4)*temp
                b(2,k)=b(2,k)+c(5)*temp
                b(3,k)=b(3,k)+c(6)*temp
            ELSEIF(j.eq.6)then
                b(1,k)=b(1,k)+c(7)*temp
                b(2,k)=b(2,k)+c(8)*temp
                b(3,k)=b(3,k)+c(9)*temp
                b(4,k)=b(4,k)+c(10)*temp
            ELSEIF(j.eq.7)then
                b(1,k)=b(1,k)+c(11)*temp
                b(2,k)=b(2,k)+c(12)*temp
                b(3,k)=b(3,k)+c(13)*temp
                b(4,k)=b(4,k)+c(14)*temp
                b(5,k)=b(5,k)+c(15)*temp
            ELSEIF(j.eq.8)then
                b(1,k)=b(1,k)+c(16)*temp
                b(2,k)=b(2,k)+c(17)*temp
                b(3,k)=b(3,k)+c(18)*temp
                b(4,k)=b(4,k)+c(19)*temp
                b(5,k)=b(5,k)+c(20)*temp
                b(6,k)=b(6,k)+c(21)*temp
            ENDIF
c =========end do on components========================= 
 171      continue
c =========end do on substeps========================= 
 174    continue
        RETURN
        END
c =====================================================
c rapred
c
c finds new x and v values at end of sequence using eqs. (2.11),(2.12)
c =====================================================
      SUBROUTINE rapred(ncl,x,v,t,t2,f1,b,nv)
      IMPLICIT NONE
c flag for first order equations
      LOGICAL  ncl
c actual dimension
      INTEGER nv 
c stepsize, squared for second order eq.
      DOUBLE PRECISION t,t2
c state variables 
      DOUBLE PRECISION x(nv),v(nv)
c right hand side at beginning of step
      DOUBLE PRECISION f1(nv)
c storage arrays
      DOUBLE PRECISION b(7,nv)
c end interface
c loop indexes
      INTEGER k
c ========================================================
c constants of the integration method
      DOUBLE PRECISION h(8),w(7),u(7),c(21),d(21),r(21),w1
      COMMON/raconst/h,w,u,c,d,r,w1
c ============================================
      DO k=1,nv
        x(k)=x(k)+v(k)*t+t2*(f1(k)*w1+b(1,k)*w(1)+b(2,k)*w(2)
     +   +b(3,k)*w(3)+b(4,k)*w(4)+b(5,k)*w(5)+b(6,k)*w(6)+b(7,k)*w(7))
        IF(.not.ncl)THEN
           v(k)=v(k)+t*(f1(k)+b(1,k)*u(1)+b(2,k)*u(2)+b(3,k)*u(3)
     +       +b(4,k)*u(4)+b(5,k)*u(5)+b(6,k)*u(6)+b(7,k)*u(7))
        ENDIF
      ENDDO
      RETURN
      END
c ===============================================================
c rabeta
c
c  find new beta-values from the predicted b-values, 
c  following eq. (2.7) in text.
c ===============================================================
      SUBROUTINE rabeta(nv,b,d,g)
      IMPLICIT NONE
c number of equations
      INTEGER nv
c storage arrays
      DOUBLE PRECISION b(7,nv),g(7,nv),d(21)
c loop indexes
      INTEGER k
c =========================
      DO k=1,nv
        g(1,k)=b(1,k)+d(1)*b(2,k)+d(2)*b(3,k)+
     +             d(4)*b(4,k)+d( 7)*b(5,k)+d(11)*b(6,k)+d(16)*b(7,k)
        g(2,k)=            b(2,k)+d(3)*b(3,k)+
     +             d(5)*b(4,k)+d( 8)*b(5,k)+d(12)*b(6,k)+d(17)*b(7,k)
        g(3,k)=            b(3,k)+
     +             d(6)*b(4,k)+d( 9)*b(5,k)+d(13)*b(6,k)+d(18)*b(7,k)
        g(4,k)=         b(4,k)+d(10)*b(5,k)+d(14)*b(6,k)+d(19)*b(7,k)
        g(5,k)=                      b(5,k)+d(15)*b(6,k)+d(20)*b(7,k)
        g(6,k)=                                   b(6,k)+d(21)*b(7,k)
        g(7,k)=                                                b(7,k)
      ENDDO
      RETURN
      END  
c ===============================================================
c  bintrp
c
c  now predict b-values for next step. the predicted values from the preceding
c  sequence were saved in the e-matrix. te correction bd between the actual
c  b-values found and these predicted values is applied in advance to the
c  next sequence. the gain in accuracy is significant. using eqs. (2.13):
c ===============================================================
      SUBROUTINE bintrp(q,b,e,bd,nv,ns)
      IMPLICIT NONE
c =============INPUT===============
c rescaled time
      DOUBLE PRECISION q
c dimension of state vector, number of steps completed
      INTEGER nv,ns
c =============INPUT AND OUTPUT=============
c storage arrays
      DOUBLE PRECISION e(7,nv),b(7,nv),bd(7,nv)
c =============END INTERFACE================
      INTEGER k,j,l
c ===============================================================
      DO 39 k=1,nv
        IF(ns.ne.1)then
           DO  j=1,7
             bd(j,k)=b(j,k)-e(j,k)
           ENDDO

        ENDIF
        e(1,k)=      q*(b(1,k)+ 2.d0*b(2,k)+ 3.d0*b(3,k)+
     x           4.d0*b(4,k)+ 5.d0*b(5,k)+ 6.d0*b(6,k)+ 7.d0*b(7,k))
        e(2,k)=                q**2*(b(2,k)+ 3.d0*b(3,k)+
     y           6.d0*b(4,k)+10.d0*b(5,k)+15.d0*b(6,k)+21.d0*b(7,k))
        e(3,k)=                             q**3*(b(3,k)+
     z           4.d0*b(4,k)+10.d0*b(5,k)+20.d0*b(6,k)+35.d0*b(7,k))
        e(4,k)= q**4*(b(4,k)+ 5.d0*b(5,k)+15.d0*b(6,k)+35.d0*b(7,k))
        e(5,k)=              q**5*(b(5,k)+ 6.d0*b(6,k)+21.d0*b(7,k))
        e(6,k)=                           q**6*(b(6,k)+ 7.d0*b(7,k))
        e(7,k)=                                         q**7*b(7,k)
        DO  l=1,7
          b(l,k)=e(l,k)+bd(l,k)
        ENDDO
 39   ENDDO
      RETURN
      END
c ===============================================================
c  RADCON assigns h spacings and
c  evaluates the constants in the w-, u-, c-, d-, and r-vectors
c ===========begin interface=========================================
      SUBROUTINE radcon(ncl)
      IMPLICIT NONE
      LOGICAL ncl
      DOUBLE PRECISION h(8),w(7),u(7),c(21),d(21),r(21),w1
      COMMON/raconst/h,w,u,c,d,r,w1
c ===========end interface===========================================
c loop indexes
      INTEGER n,k,la,lc,lb,ld,le,l
c scalar real
      DOUBLE PRECISION ww
      DOUBLE PRECISION half,one
c integers
      INTEGER nw(8)
      DATA half, one/0.5d0, 1.0d0/
      DATA nw/0,0,1,3,6,10,15,21/
c ===================================================================
c  these h values are the gauss-radau spacings, scaled to the range 0 to 1,
c  for integrating to order 15.
      h(1)=0.d0
      h(2)= .05626256053692215d0
      h(3)= .18024069173689236d0
      h(4)=.35262471711316964d0 
      h(5)=.54715362633055538d0
      h(6)= .73421017721541053d0
      h(7)=.88532094683909577d0
      h(8)= .97752061356128750d0
c  the sum of the h-values should be 3.73333333333333333
c ====================================================================
c  evaluate the constants in the w-, u-, c-, d-, and r-vectors
      do 14 n=2,8
        ww=n+n*n
        IF(ncl) ww=n
        w(n-1)=one/ww
        ww=n
  14    u(n-1)=one/ww
      w1=half
      IF(ncl) w1=one
      c(1)=-h(2)
      d(1)=h(2)
      r(1)=one/(h(3)-h(2))
      la=1
      lc=1
      do 73 k=3,7
        lb=la
        la=lc+1
        lc=nw(k+1)
        c(la)=-h(k)*c(lb)
        c(lc)=c(la-1)-h(k)
        d(la)=h(2)*d(lb)
        d(lc)=-c(lc)
        r(la)=one/(h(k+1)-h(2))
        r(lc)=one/(h(k+1)-h(k))
        IF(k.eq.3) go to 73
        do 72 l=4,k
          ld=la+l-3
          le=lb+l-4
          c(ld)=c(le)-h(k)*c(le+1)
          d(ld)=d(le)+h(l-1)*d(le+1)
  72      r(ld)=one/(h(k+1)-h(l-1))
  73  continue
      return 
      end
c ================================
c invaxv
c
c initialise the  variation matrix as the 6 X 6 identity
c ================================
      SUBROUTINE invaxv(x,v,nvar2)
      IMPLICIT NONE
      INTEGER nvar2
      DOUBLE PRECISION x(nvar2),v(nvar2)
c end interfface
      INTEGER i,j,iii,ij        
      iii=3
      do 7 j=1,6
         do  i=1,3
            ij=i+3*(j-1)
            x(iii+ij)=0.d0
            v(iii+ij)=0.d0
            if(i.eq.j)then
               x(iii+ij)=1.d0
            elseif(j.eq.i+3)then
               v(iii+ij)=1.d0
            endif
         enddo
 7    continue
      RETURN
      END   
c ====================================================
c vawrxv
c
c First parzial derivatives: rewrap vector into 6x6 matrix
c ====================================================
      SUBROUTINE vawrxv(x,v,dxdx0,nvar2)
      IMPLICIT NONE
      INTEGER nvar2
      DOUBLE PRECISION dxdx0(6,6),x(nvar2),v(nvar2)
      INTEGER i,j,ij
c ====================================================
      DO j=1,3
         DO  i=1,3
            ij=i+3*(j-1)+3
            dxdx0(i,j)=x(ij)
            dxdx0(i,j+3)=x(ij+9)
            dxdx0(i+3,j)=v(ij)
            dxdx0(i+3,j+3)=v(ij+9)
         ENDDO 
      ENDDO
      RETURN
      END
c ====================================================
c varunw
c
c First parzial derivatives: unwrap 6x6 matrix into vector
c ====================================================
      SUBROUTINE varunw(dxdx0,x,v,nvar2)
      IMPLICIT NONE
      INTEGER nvar2
      DOUBLE PRECISION dxdx0(6,6),x(nvar2),v(nvar2)
      INTEGER i,j,ij
c ====================================================
      DO j=1,3
         DO  i=1,3
            ij=i+3*(j-1)+3
            x(ij)=dxdx0(i,j)
            x(ij+9)=dxdx0(i,j+3)
            v(ij)=dxdx0(i+3,j)
            v(ij+9)=dxdx0(i+3,j+3)
         ENDDO 
      ENDDO
      RETURN
      END
