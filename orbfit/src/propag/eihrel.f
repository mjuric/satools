c ***************************************************************
c EIHREL
c ***************************************************************
c This subroutine computes heliocentric acceleration of the
c asteroid on the (1/c^2)(= PN) level. Both the direct solar term
c (~Schwarzschild contribution) and the planetary contributions
c are included. The monopole terms considered only (= EIH
c approximation).
c
c Written by D Vokrouhlicky and S Chesley, Nov 3, 1999
c ***************************************************************
      subroutine eihrel(x,vs,xpla,d,rpla,xsun,drgr)
      implicit none
c planetary masses and other model parameters, vlight is required
      include 'parbep.h'
      include 'masses.h'
      include 'vlight.h'
c position, velocity, relativistic effects
      double precision x(3),vs(3),xpla(6,nmassx),d(nmassx),drgr(3)
      double precision rpla(nmassx)
c scalar temporaries
      double precision temp,rplapla,strqua,potall,potalls,gmall,tempp
      double precision xsun,xsun3,xdsun2,vast2,vsun2,rplapla2,scals
      double precision vlight2
c loop indexes
      integer i,ii,j,ib
      double precision g(3),vsun(3),vast(3),vpla(3,nmassx),tbtvec(3)
      double precision xastpla(3),nastpla(3),vastpla(3)
      double precision rplavec(3),tmpvec(3),rpla3(nmassx)
c ---------------------------------------------------------------------
c initialization
      do i=1,3
         drgr(i)=0.d0
      enddo
c compute potential of all bodies on asteroid, planets on Sun
c and heliocentric distances of planets and the total mass of the
c system
      potall=gm0/xsun
      gmall=gm0
      potalls=0.d0
      do i=1,npla
         potall=potall+gm(i)/d(i)
         rpla3(i)=rpla(i)**3
         potalls=potalls+gm(i)/rpla(i)
         gmall=gmall+gm(i)
      enddo
c compute barycentric velocities of sun, asteroid, and planets
      do j=1,3
         vsun(j)=0.d0
         do i=1,npla
            vsun(j)=vsun(j)-gm(i)*xpla(j+3,i)/gmall
         enddo
         vast(j)=vsun(j)+vs(j)
      enddo
      vsun2=vsun(1)*vsun(1)+vsun(2)*vsun(2)+vsun(3)*vsun(3)
      vast2=vast(1)*vast(1)+vast(2)*vast(2)+vast(3)*vast(3)
      do i=1,npla
         do j=1,3
            vpla(j,i)=vsun(j)+xpla(3+j,i)
         enddo
      enddo
c ................................................................
c compute indirect (1/c^2) planetary accelerations:
      do ib=1,npla
         do j=1,3
            g(j)=-gm(ib)*xpla(j,ib)/rpla3(ib)
         enddo
c        strange quantity
         strqua=1.5d0*gm0/rpla(ib)
         do ii=1,npla
            if(ii.ne.ib) then
               do j=1,3
                  rplavec(j)=xpla(j,ii)-xpla(j,ib)
               enddo
               rplapla2=rplavec(1)**2+rplavec(2)**2+rplavec(3)**2
               rplapla=dsqrt(rplapla2)
               scals=xpla(1,ib)*rplavec(1)+xpla(2,ib)*rplavec(2)+
     +               xpla(3,ib)*rplavec(3)
               strqua=strqua+gm(ii)*(1.d0-0.5d0*scals/rplapla2)/rplapla
            endif
         enddo
         tempp=(xpla(1,ib)*vpla(1,ib)+xpla(2,ib)*vpla(2,ib)+
     +          xpla(3,ib)*vpla(3,ib))/rpla(ib)
         temp=2.d0*(xpla(4,ib)*xpla(4,ib)+xpla(5,ib)*xpla(5,ib)+
     +        xpla(6,ib)*xpla(6,ib))-vsun2-1.5d0*tempp*tempp-
     +        4.d0*potalls-strqua
c add "g*temp" terms
         do i=1,3
            drgr(i)=drgr(i)+temp*g(i)
         enddo
c add "velocity-dependent" terms
         do i=1,3
            tmpvec(i)=-4.d0*xpla(3+i,ib)+vpla(i,ib)
         enddo
         temp=g(1)*tmpvec(1)+g(2)*tmpvec(2)+g(3)*tmpvec(3)
         do i=1,3
            drgr(i)=drgr(i)+temp*xpla(3+i,ib)
         enddo
c "third-body" term (tbt)
         do i=1,3
            tbtvec(i)=gm0*xpla(i,ib)/rpla3(ib)
         enddo
         do ii=1,npla
            if(ii.ne.ib) then
               do j=1,3
                  rplavec(j)=xpla(j,ib)-xpla(j,ii)
               enddo
               rplapla=dsqrt(rplavec(1)**2+rplavec(2)**2+rplavec(3)**2)
               temp=gm(ii)/(rplapla**3)
               do i=1,3
                  tbtvec(i)=tbtvec(i)+temp*rplavec(i)
               enddo
            endif
         enddo
         temp=3.5d0*gm(ib)/rpla(ib)
         do i=1,3
            drgr(i)=drgr(i)+temp*tbtvec(i)
         enddo
      enddo
c compute direct (1/c^2) accelerations:
c -- planetary terms
      do ib=1,npla
         do j=1,3
            g(j)=gm(ib)*(x(j)-xpla(j,ib))/(d(ib)**3)
         enddo
c        pos rel to planet (unit vector)
         do j=1,3
            xastpla(j)=x(j)-xpla(j,ib)
            nastpla(j)=xastpla(j)/d(ib)
         enddo
c        vast rel to planet
         do j=1,3
            vastpla(j)=vast(j)-vpla(j,ib)
         enddo
c        strange quantity
         scals=xastpla(1)*xpla(1,ib)+xastpla(2)*xpla(2,ib)+
     +         xastpla(3)*xpla(3,ib)
         strqua=gm0*(1.d0-0.5d0*scals/(rpla(ib)**2))/rpla(ib)
         do ii=1,npla
            if(ii.ne.ib)then
               do j=1,3
                  rplavec(j)=xpla(j,ii)-xpla(j,ib)
               enddo
               rplapla2=rplavec(1)**2+rplavec(2)**2+rplavec(3)**2
               rplapla=dsqrt(rplapla2)
               scals=xastpla(1)*rplavec(1)+xastpla(2)*rplavec(2)+
     +               xastpla(3)*rplavec(3)
               strqua=strqua+gm(ii)/rplapla*(1.d0+0.5d0*scals/rplapla2)
            endif
         enddo
         tempp=nastpla(1)*vpla(1,ib)+nastpla(2)*vpla(2,ib)+
     +         nastpla(3)*vpla(3,ib)
         temp=2.d0*(vastpla(1)**2+vastpla(2)**2+vastpla(3)**2)-
     +        vast2-1.5d0*tempp*tempp-4.d0*potall-strqua
c        add g*temp to force
         do i=1,3
            drgr(i)=drgr(i)-g(i)*temp
         enddo
c add "velocity-dependent" terms
         do i=1,3
            tmpvec(i)=4.d0*vastpla(i)+vpla(i,ib)
         enddo
         temp=g(1)*tmpvec(1)+g(2)*tmpvec(2)+g(3)*tmpvec(3)
         do i=1,3
            drgr(i)=drgr(i)+vastpla(i)*temp
         enddo
c "third-body" term (tbt)
         do i=1,3
            tbtvec(i)=gm0*xpla(i,ib)/rpla3(ib)
         enddo
         do ii=1,npla
            if(ii.ne.ib)then
               do j=1,3
                  rplavec(j)=xpla(j,ib)-xpla(j,ii)
               enddo
               rplapla=dsqrt(rplavec(1)**2+rplavec(2)**2+rplavec(3)**2)
               temp=gm(ii)/(rplapla**3)
               do i=1,3
                  tbtvec(i)=tbtvec(i)+temp*rplavec(i)
               enddo
            endif
         enddo
         temp=-3.5d0*gm(ib)/d(ib)
         do i=1,3
            drgr(i)=drgr(i)+temp*tbtvec(i)
         enddo
      enddo
c compute direct (1/c^2) accelerations:
c -- solar (~ Schwarzschild) term
      xsun3=xsun**3
      do i=1,3
         g(i)=gm0*x(i)/xsun3
      enddo
      xdsun2=vs(1)*vs(1)+vs(2)*vs(2)+vs(3)*vs(3)
c     strange quantity
      strqua=0.d0
      do i=1,npla
         scals=x(1)*xpla(1,i)+x(2)*xpla(2,i)+x(3)*xpla(3,i)
         strqua=strqua+gm(i)*(1.d0+0.5d0*scals/(rpla(i)**2))/rpla(i)
      enddo
      tempp=(x(1)*vsun(1)+x(2)*vsun(2)+x(3)*vsun(3))/xsun
      temp=2.d0*xdsun2-vast2-1.5d0*tempp*tempp-4.d0*potall-strqua
c add g*temp to force
      do i=1,3
        drgr(i)=drgr(i)-g(i)*temp
      enddo
c add velocity terms ...
      do i=1,3
         tmpvec(i)=4.d0*vs(i)+vsun(i)
      enddo
      temp=g(1)*tmpvec(1)+g(2)*tmpvec(2)+g(3)*tmpvec(3)
      do i=1,3
         drgr(i)=drgr(i)+temp*vs(i)
      enddo
c "third-body" term (tbt)
      do i=1,3
         tbtvec(i)=0.d0
      enddo
      do ii=1,npla
         temp=gm(ii)/rpla3(ii)
         do i=1,3
            tbtvec(i)=tbtvec(i)+temp*xpla(i,ii)
         enddo
      enddo
      temp=3.5d0*gm0/xsun
      do i=1,3
         drgr(i)=drgr(i)+temp*tbtvec(i)
      enddo
c ..................................................................
c scale by 1/c^2
      vlight2=vlight**2
      do i=1,3
         drgr(i)=drgr(i)/vlight2
      enddo
      return
      end


