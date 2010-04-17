c =================================================
c  minimum distance with respect to both u and up
c  prepares first guess, using output from nodedi
      double precision function dista(eq0,eq1,iconv,f0,f1,incl)
      implicit none
c INPUT 
c elements
      double precision eq0(6),eq1(6)
c true anomalies at mutual node, mutual inclination
      DOUBLE PRECISION f0,f1,incl,incmin
c OUTPUT
      integer iconv
c END INTERFACE
c data to be shared with lower levels
      INCLUDE 'moidhe2.h'
c dimensions
c indexes
      integer i,imin
c functions
      double precision dfals, dmin
c anomalies
      double precision f,fp
c local minima
      double precision dist1,dist2
      INCLUDE 'trig.h'
c ============================================================
c controls
      eps=1d-7
      dd=1e-5
      nmax=20
      deltau=2.d0*radeg
      incmin=5.d0*radeg
c store elements in common
      DO i=1,5
        eq(i)=eq0(i)
        eqp(i)=eq1(i)
      ENDDO
      IF(incl.ge.incmin)THEN
         write(99,*)' inclination', incl,incmin
      ELSE
c other guess is
      ENDIF 
c first guess is mutual node 1
      f=f0
      fp=f1
      write(99,*)' first node at ',f,fp
      dist1=dmin(f,fp,iconv)
      write(99,*)' first node at ',f,fp,dist1
c second guess is mutual node 2
      f=f0+pig
      fp=f1+pig
      write(99,*)' second node at ',f,fp
      dist2=dmin(f,fp,iconv)
      write(99,*)' second node at ',f,fp,dist2
      dista=min(dist1,dist2)
c third guess is
      RETURN
      END
c =================================================
c  minimum distance with respect to both u and up
c  in input u,up are first guess
      double precision function dmin(u,up,icon)
      implicit none
      double precision u,up
      integer icon
      double precision up1,up2,up1d,up2d,upd,primea,princ,pridif
      integer icon1,icon2,icon1p,icon2p,iconv,iconvp
c END INTERFACE
      double precision x,x1,x2,f,f1,f2
c data to be shared with lower levels
      INCLUDE 'moidhe2.h'
c trigonometric constants
      INCLUDE 'trig.h'
c functions
      DOUBLE PRECISION dfals
c first guess is given; stay on both sides.
      x1=princ(u-deltau)
      up1=princ(up-deltau)
      x2=princ(u+deltau)
      up2=princ(up+deltau)      
      write(99,*)x1,x2,deltau

      f1=(dfals(x1,up1,icon1)-dfals(x1-dd,up1,icon1p))/dd
      f2=(dfals(x2,up2,icon2)-dfals(x2-dd,up2,icon2p))/dd
c     write(99,*)x1,x2,f1,f2
c regula falsi
      do 1 icon=1,nmax
         if(abs(f1-f2).lt.eps)then
            x=primea(x1,x2)
         elseif(f1*f2.lt.0.d0)then
            x=princ(x1+pridif(x1,x2)*f1/(f2-f1))
         elseif(abs(f1).lt.abs(f2))then
            x=princ(x2+2.d0*pridif(x1,x2))
         elseif(abs(f1).gt.abs(f2))then
            x=princ(x1+2.d0*pridif(x2,x1))
         endif
c recomputation of derivative of distance
         x=princ(x)
         up=primea(up1,up2)
         dmin=dfals(x,up,iconv)
         f=(dmin-dfals(x-dd,up,iconvp))/dd
         write(99,*)icon,x1,x2,x,f1,f2,f,dmin
c test 
         if(abs(f).lt.eps)then
c convergence
            u=x
            write(99,*)' convergent at ',u,up,dmin
            return
c if there is only one change of sign, chose the two with oppos. sign
         elseif(f1*f.lt.0.d0.and.f2*f.gt.0.d0)then
            x2=x
            f2=f
         elseif(f2*f.lt.0.d0.and.f1*f.gt.0.d0)then
            x1=x
            f1=f
c otherwise, continue with f and the lower value
         elseif(f1*f2.gt.0.d0)then
            if(abs(f1).lt.abs(f2))then
               x2=x
               f2=f
            else
               x1=x
               f1=f
            endif
         endif
 1    continue
c  non convergent case: give a value anyway
      u=x
      write(99,*)' divergent at ', u,up,dmin
      icon=-1
      return
      end
c =================================================
c  minimum distance with respect to up, u is fixed
c in input up is a first guess
      double precision function dfals(u,up,iconv)
      implicit none
      double precision u,up
      integer iconv
c data to be shared with lower levels
      INCLUDE 'moidhe2.h'

      double precision x1,x2,f1,f2,f,x,xstep,uu
      double precision dis,princ,primea,pridif

c trigonometric constants
      INCLUDE 'trig.h'
c u is fixed
      uu=u
c first guess is up
      x1=princ(up-deltau)
      f1=(dis(uu,x1)-dis(uu,x1-dd))/dd
      x2=princ(up+deltau)
      f2=(dis(uu,x2)-dis(uu,x2-dd))/dd
c regula falsi
      do 1 iconv=1,nmax
         if(abs(f1-f2).lt.eps)then
            x=primea(x1,x2)
         elseif(f1*f2.lt.0.d0)then
            x=princ(x1+pridif(x1,x2)*f1/(f2-f1))
         elseif(abs(f1).lt.abs(f2))then
            x=princ(x2+2.d0*pridif(x1,x2))
         elseif(abs(f1).gt.abs(f2))then
            x=princ(x1+2.d0*pridif(x2,x1))
         endif
         x=princ(x)
         dfals=dis(uu,x)
         f=(dfals-dis(uu,x-dd))/dd
c         write(99,*)x1,x2,x,f1,f2,f,dfals
         if(abs(f).lt.eps)then
c convergence
            up=x
            write(99,*)' convp ',uu,up,dfals,f
            return
c if there is only one change of sign, chose the two with oppos. sign
         elseif(f1*f.lt.0.d0.and.f2*f.gt.0.d0)then
            x2=x
            f2=f
         elseif(f2*f.lt.0.d0.and.f1*f.gt.0.d0)then
            x1=x
            f1=f
c otherwise, continue with f and the lower value
         elseif(f1*f2.gt.0.d0)then
            if(abs(f1).lt.abs(f2))then
               x2=x
               f2=f
            else
               x1=x
               f1=f
            endif
         endif
 1    continue
c  non convergent case: give a value anywa
      write(99,111)iconv,x,f,dfals
 111  format('divergence, iter=',i2,' x=',f12.7,' f=',1p,d12.4,0p,
     +       ' distance=',f10.7)
      up=x
      iconv=-1
      return
      end








