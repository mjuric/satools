****************************************************************
*
*  {\bf INASTC}
*
*  INPUT  : iun  = unit file
*           nasx = maximum number of asteroids in the file 
*           nam = asteroid name (shortened to 6 characters) 
*
*  OUTPUT : eq   = vector of equinoctial elements of the asteroid
*           ta   = epoch time of the elements  
*           ini  = .true. if found
*****************************************************************
      subroutine inastc(iun,nam,eq,h,ta,ini)
      implicit none
      integer nasx
      parameter (nasx=40000)
      double precision eq(6),el(6),t0,tmjd,ta,h,conv,hour
      integer lflag,ivar,i,iun,iqco,iyear,month,iday,j,lnam,lnam1
      double precision tjm1
      character*6 nam,nam1
      logical ini
c this version reads the file from scratch every time
c     data lflag /0/
c     save
c     if(lflag.eq.0)then
* input header from catalogue in MPC format:
          read(iun,101)t0
 101      format(4x,f9.1/)
* modification of ta to Modified Julian Date
          if(t0.eq.0.d0)then
             ivar=1
          else
             tmjd=t0-2.4000005d6
             ivar=0
          endif
          lflag=1
c      endif
       write(*,*)nam
       call rmsp(nam,lnam)
       do 10 i=1,nasx
         if(ivar.eq.0)then
            ta=tmjd
            read(iun,107,end=2)nam1,el(6),el(5),el(4),el(3),el(2),el(1),
     +      iqco,h
 107        format(a6,3(1x,f9.5),1x,f8.5,1x,f10.8,1x,f12.8,1x,
     +    i2,1x,f5.2)
         else
            read(iun,108,end=2)nam1,el(6),el(5),el(4),el(3),el(2),el(1),
     +      iqco,iyear,month,iday,h
 108        format(a6,3(1x,f9.5),1x,f8.5,1x,f10.8,1x,f12.8,
     +      4(i3),1x,f5.2)
c           write(*,108)nam1,el(6),el(5),el(4),el(3),el(2),el(1),
c    +      iqco,iyear,month,iday,h
            iyear=iyear+1900
            hour=0.d0
            ta=tjm1(iday,month,iyear,hour)
         endif
         call rmsp(nam1,lnam1)
         if(nam(1:lnam).eq.nam1(1:lnam1))goto 3
 10    continue
 2     write(*,*)'asteroid ',nam,' not found'
       ini=.false.
       return
 3     continue
* asteroid found; change to equinoctal elements
       conv=atan(1.d0)/45.d0
       do 6 j=3,6
 6       el(j)=el(j)*conv
       call kepequ(el,eq)
c       write(*,220)eq
c 220   format('starting values'/6f13.7)
       ini=.true.
       return
       end
c =====================================================================
c WRIEQU (write initial conditions, equinoctal)
c =====================================================================
      subroutine wriequ(iun,astna0,t0,eq0)
      implicit none
      double precision t0,eq0(6)
      integer iun
      character*18 astna0
c initial conditions found
      write(*,108)astna0,t0
 108  format(1x,a18,' initial elem (a,h,k,p,q,lam), epoch=',f8.1)
      write(iun,104) eq0
      write(*,104) eq0
 104  format(6f13.7)
      write(iun,*)' '
      return
      end







