c ===========================================================
c FORCE : accelerations acting on a massless asteroid
c ===========================================================
c version 1.9.1; A. Milani, March 11, 1999
c Accelerations on the asteroid, computed by using
c JPL ephemerides as source for the position of the planets
c (variational equation second part)
c close approach control
c asteroid perturbations with trick to avoid self-perturbations
c ===========================================================
      subroutine force(x,v,t0,f,nd,idc,xxpla)
      implicit none
c model controls
      include 'model.h'
      include 'iclap.h'
c asteroid maximum number
      include 'parbep.h'
c planetary masses and other model parameters, including asteroid masses
      include 'masses.h'
c controls and physical parameters for nongravitational forces
      INCLUDE 'yarkom.h'
      INCLUDE 'yarkov.h' 
c ======INPUT===================
c dimension of position vector
      integer nd
c Position, velocity,  time
      double precision x(nd), v(nd) ,t0
c ======OUTPUT===================
c acceleration
      double precision f(nd)
c Positions and vel of the planet involved in  close-app
c stored only if idc.ne.0
      integer idc
      double precision xxpla(6)
c ======END INTERFACE==============
c JPL ephemerides
c Workspace for state call
      double precision et0(2),rot(3,3),pv(6,11),pnut(4),xx(3),xxp(3)
c flags to control units and center
      double precision pvsun(6)
      logical km,bary
      common/stcomx/km,bary,pvsun
c JPL header
      include 'jplhdr.h'
c flag for interpolation of position only or pos and vel.
      integer istate
      common/cstate/istate
c ================================================
c Distances 
      double precision r(nmassx), d(nmassx),rast
      double precision derf(3,3),dfb(3,3),dfc(3,3)
c Positions of the planets also vel.; space also for asteroids
      double precision xpla(6,nmassx)
c positions of the massive asteroids
      double precision xast(3,nbepx),vast(3,nbepx)

c asteroid requests to rdbep
c list of asteroid indexes in the asteroid ephemerides arrays
      INCLUDE 'selast.h'
c Relativistic perturbations
      double precision drgr(3,7),frel(3)
c Yarkovsky force
      double precision yarkv(21)
c control of derivatives
      integer ide
      common/deriv/ide
c loop indexes
      integer i,iii,ia,j,k,ir,ic,icqfw
c scalar temporaries
      double precision sum,var1
c initial call flag
      integer lflag
c****************
c   static memory only for 
      save rot,lflag
c****************
      data lflag/0/
c ===========================================================
c reference system rotation matrix
      if(lflag.eq.0)then 
         call rotpn(rot,'EQUM','J2000',0.d0,'ECLM','J2000',0.d0)
         lflag=1
      endif
c Read planetary positions from JPL files
      et0(1)=2400000.5d0
      et0(2)=t0
      bary=.false.
      call state(et0,listpl,pv,pnut,istate)
c reorder data (see pleph)
      do 9 i=1,npla
         if(itarg(i).ne.3.and.itarg(i).ne.10.and.itarg(i).ne.13)then 
            do 5 iii=1,3
              if(istate.eq.2)xxp(iii)=pv(iii+3,itarg(i))
              xx(iii)=pv(iii,itarg(i))
 5          continue
          elseif(itarg(i).eq.13)then
            do 8 iii=1,3
              if(istate.eq.2)xxp(iii)=pv(iii+3,3)
              xx(iii)=pv(iii,3)
 8         continue
         elseif(itarg(i).eq.10)then
            do 6 iii=1,3
              if(istate.eq.2)
     +   xxp(iii)=pv(iii+3,10)*emrat/(1.d0+emrat)+pv(iii+3,3)
              xx(iii)=pv(iii,10)*emrat/(1.d0+emrat)+pv(iii,3)
 6          continue
        elseif(itarg(i).eq.3)then
            do 7 iii=1,3
              if(istate.eq.2)xxp(iii)=pv(iii+3,3)-pv(iii+3,10)/
     +               (1.d0+emrat)
              xx(iii)=pv(iii,3)-pv(iii,10)/(1.d0+emrat)
 7         continue
         endif
c Change of reference system EQUM00 ---> ECLM00
c        call prodmv(xpla(1,i),rot,xx)
c        if(istate.eq.2)call prodmv(xpla(4,i),rot,xxp)
         do 21 j=1,3
           xpla(j,i)=0.d0
           xpla(j+3,i)=0.d0
           do 22 k=1,3
             xpla(j,i)=xpla(j,i)+rot(j,k)*xx(k)
             if(istate.eq.2)xpla(j+3,i)=xpla(j+3,i)+rot(j,k)*xxp(k)
 22        continue
 21      continue
c         write(*,*)(xpla(iii,i),iii=1,3)
 9    continue
c ===========================================================
      IF(iatrue.gt.0)THEN
c read asteroid positions from binary ephemerides
         CALL rdbep(t0,iatrue,astid,xast,vast)
c stacking selected asteroids in the planets array
         DO ia=1,iatrue
            DO j=1,3
               xpla(j,npla+ia)=xast(j,ia)
               IF(istate.eq.2)xpla(j+3,npla+ia)=vast(j,ia)
            ENDDO
         ENDDO
      ENDIF
c ===========================================================
c Computation of planet vector lengths
      do 20 i=1,nmass
         r(i)=sqrt(xpla(1,i)**2+xpla(2,i)**2+xpla(3,i)**2)
 20   continue
      rast=sqrt(x(1)**2+x(2)**2+x(3)**2)
c ===========================================================
c Computation of planets-asteroid distances
      idc=0
      do 30 i=1,nmass
c       d(i)=vsize(x-xpla(1:3,i))
        d(i)=sqrt((x(1)-xpla(1,i))**2+(x(2)-xpla(2,i))**2+
     +             (x(3)-xpla(3,i))**2)
        if(iclap.gt.0)then
           if(d(i).lt.dmin(i))then
              if(idc.eq.0)then
                 idc=i
                 DO  icqfw=1,3
                   xxpla(icqfw)=xpla(icqfw,i)
                   IF(istate.eq.2)xxpla(icqfw+3)=xpla(icqfw+3,i)
                 ENDDO
              else
                 write(*,*)' force: this should not happen',t0, idc,i
                 write(*,*)nmass,(d(iii),dmin(iii),iii=1,nmass)
                 stop
              endif
           endif
        endif
30    continue
c ===========================================================
c initialize force
      do 59 j=1,3
        f(j)=0.d0
 59   continue
c ===========================================================
c general relativistic correction
      if(icrel.eq.1)then
         call genrel(x,v,drgr)
         do j=1,3
            f(j)=f(j)+drgr(j,1)
         enddo
      elseif(icrel.eq.2)then
         call eihrel(x,v,xpla,d,r,rast,frel)
c     write(*,'(a4,3(1x,g22.16))')'frel',frel(1),frel(2),frel(3)
c     write(*,'(a4,3(1x,g22.16))')'drgr',drgr(1,1),drgr(2,1),drgr(3,1)
         do j=1,3
            f(j)=f(j)+frel(j)
         enddo
c if we need the refined relativity then also include J_2 for the sun
         call j2sun(x,frel)
         do j=1,3
            f(j)=f(j)+frel(j)
         enddo
      endif
c ===========================================================
c Sitarski force (Acta Astronomica vol. 48 (1998), pp. 547-561)     
c         do j=1,3
c            f(j)=f(j)+(-0.15987d-10/2d0/2.5119760)*v(j)
c         enddo
c ===========================================================
c yarkovsky effect, if required and data are avilable
      if(iyark.ge.1.and.yarfil)then
         IF(.not.yarini)THEN
            WRITE(*,*)' contradiction in non gravitational parameters'
            WRITE(*,*)'iyark=',iyark,' yarfil=',yarfil,' yarini=',yarini
            STOP 
         ENDIF
c diurnal
         call yarkdi(x,yarkv,iyarpt)
c         write(*,'(a,3(1x,d18.12))') 'diur ',yarkv(1),yarkv(2),yarkv(3)
         do j=1,3
            f(j)=f(j)+yarkv(j)
         enddo
c seasonal
         if(iyark.gt.1)then
            call yarkse(x,v,yarkv,iyarpt)
c         write(*,'(a,3(1x,d18.12))')'seas ',yarkv(1),yarkv(2),yarkv(3)
            do j=1,3
               f(j)=f(j)+yarkv(j)
            enddo
         endif
      endif
c ===========================================================
c Computation of indirect force FI
      do 61 i=1,nmass
        do 60 j=1,3
          f(j)=f(j)-gm(i)/r(i)**3*xpla(j,i)
 60     continue
 61   continue
c ===========================================================
c Adding planets-asteroid attractions
      do 70 i=1,nmass
        do 80 j=1,3
            f(j)=f(j)+gm(i)/d(i)**3*(xpla(j,i)-x(j))
80      continue
70    continue
c ===========================================================
c Adding solar attraction
      do 65 j=1,3
        f(j)=f(j)-gm0*x(j)/rast**3
 65   continue
c ===========================================================
      if(ide.lt.1)return
c Computation of partial derivatives matrix
      do 1 ir=1,3
         do 2 ic=ir,3
           var1=3.d0*gm0*x(ir)*x(ic)/rast**5
           if(ir.eq.ic)var1=var1-(gm0/(rast**3))
           sum=0.d0
           do  3 i=1,nmass
             sum=sum+3.d0*gm(i)*
     +             (xpla(ir,i)-x(ir))*(xpla(ic,i)-x(ic))/(d(i)**5)
             if(ir.eq.ic)sum=sum-gm(i)/(d(i)**3)
 3         continue
           derf(ir,ic)=var1+sum
           if(ir.ne.ic) then
              derf(ic,ir)=derf(ir,ic)
           endif
 2       continue
 1     continue
c ===========================================================
c Computation of variational equations second part
c           ndf=3+1
c           call vetmat(x(ndf),9,b,3,3)
c           call vetmat(x(ndf+9),9,c,3,3)
c           call prodmm(dfb,derf,b)
c           call prodmm(dfc,derf,c)
            do 24 j=1,3
              do 25 k=1,3
                dfb(j,k)=0.d0
                dfc(j,k)=0.d0
                do 26 i=1,3
                  dfb(j,k)=dfb(j,k)+derf(j,i)*x(i+3*k)
                  dfc(j,k)=dfc(j,k)+derf(j,i)*x(i+3*k+9)
 26             continue
 25           continue
 24         continue
c           call matvet(dfb,3,3,vecdfb)
c           call matvet(dfc,3,3,vecdfc)
c           do  4 ind=1,9
c               nind=ndf-1+ind
c               f(nind)=vecdfb(ind)
c               f(nind+9)=vecdfc(ind)
c4          continue
            do 27 j=1,3
              do 28 k=1,3
                f(j+3*k)=dfb(j,k)
                f(j+3*k+9)=dfc(j,k)
 28           continue
 27         continue
      return
      end





