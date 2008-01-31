c genrel
c ******************************************************************* c
c this routine treates PN terms in the kinematically                  c
c nonrotating frame according to the scheme given by Damour,          c
c Soffel and Xu (DSX IV, Phys Rev D, Jan 1994).                       c
c                                                                     c
c only the Sun monopole term is accepted 
c output format: 1) drgr(i,1) ... components of the acceleration      c
c                2) drgr(i,j+1) ... derivatives with respect to the   c
c                                 initial conditions                  c
c                                                                     c
c                                    D. Vokrouhlick\'y, 1/3/94        c
c  Modified for ORBFIT, Milani & Baccili 1997                         c
c     computation of derivatives disabled                             c
c ******************************************************************* c
      subroutine genrel(x,vs,drgr)
      implicit none
c position, velocity, relativistic effects
      double precision x(3),vs(3),drgr(3,7)
c intermediate for partials
c     double precision dmgrx(3,6)
c asteroid maximum number
      include 'parbep.h'
c planetary masses and other model parameters, vlight is required
      include 'masses.h'
      include 'vlight.h'
c scalar temporaries
      double precision rsate2,rsate,xv,v2,c2,eafac,fac,brac
c loop indexes
      integer i
c     integer j
c ---------------------------------------------------------------------
      c2=vlight*vlight
c
      rsate2=x(1)*x(1)+x(2)*x(2)+x(3)*x(3)
      rsate=dsqrt(rsate2)
      xv=x(1)*vs(1)+x(2)*vs(2)+x(3)*vs(3)
      v2=vs(1)*vs(1)+vs(2)*vs(2)+vs(3)*vs(3)
c  this refer to the Sun, not the earth
      eafac=gm0/rsate
c
c  Internal terms:
c     --  monopole (DSX IV, 3.13)
c         (rem. also accepted as the IERS Standard)
      brac=4.d0*eafac-v2
      fac=eafac/rsate2/c2
c  acceleration and partial derivatives with respect to the X and V
      do  i=1,3
        drgr(i,1)=fac*(brac*x(i)+4.d0*xv*vs(i))
      enddo
c ---------------------------------------------------------------------
c  partial derivatives available for later use
c     do 21 i=1,3
c       do 22 j=1,3
c         dmgrx(i,j)=fac*(4.d0*vs(i)*(vs(j)-3.d0*xv*x(j)/rsate2)-
c    .       (3.d0*brac+4.d0*eafac)*x(i)*x(j)/rsate2)
c         dmgrx(i,j+3)=2.d0*fac*(2.d0*vs(i)*x(j)-x(i)*vs(j))
c         if (i.eq.j) then 
c            dmgrx(i,j)=dmgrx(i,j)+brac*fac
c            dmgrx(i,j+3)=dmgrx(i,j+3)+4.d0*xv*fac
c         endif
c22     enddo
c21   enddo
c
c     do 31 i=1,3 
c       drgr(i,2)=dmgrx(i,1)
c       drgr(i,3)=dmgrx(i,2)
c       drgr(i,4)=dmgrx(i,3)
c       do 30 j=4,6
c         drgr(i,j+1)=dmgrx(i,j)
c30     enddo
c31   enddo
c ---------------------------------------------------------------------
c
      return
      end


