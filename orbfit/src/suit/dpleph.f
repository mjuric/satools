c++++++++++++++++++++++++++
c
      subroutine dpleph(et2z,ntarg,ncent,rrd,istate)
c                 pleph ( et, ntarg, ncent, rrd, istate )
c
c  note: (A. Milani, March 11, 1998)
c  we have added a new fifth argument to control the interpolation of 
c  derivatives
c
c     this subroutine reads the jpl planetary ephemeris
c     and gives the position and velocity of the point 'ntarg'
c     with respect to 'ncent'.
c
c     calling sequence parameters:
c
c       et = d.p. julian ephemeris date at which interpolation
c            is wanted.
c
c       ** note the entry dpleph for a doubly-dimensioned time **
c          the reason for this option is discussed in the 
c          subroutine state
c
c     ntarg = integer number of 'target' point.
c
c     ncent = integer number of center point.
c
c            the numbering convention for 'ntarg' and 'ncent' is:
c
c                1 = mercury           8 = neptune
c                2 = venus             9 = pluto
c                3 = earth            10 = moon
c                4 = mars             11 = sun
c                5 = jupiter          12 = solar-system barycenter
c                6 = saturn           13 = earth-moon barycenter
c                7 = uranus           14 = nutations (longitude and obliq)
c                            15 = librations, if on eph file
c
c             (if nutations are wanted, set ntarg = 14. for librations,
c              set ntarg = 15. set ncent=0.)
c
c      rrd = output 6-word d.p. array containing position and velocity
c            of point 'ntarg' relative to 'ncent'. the units are au and
c            au/day. for librations the units are radians and radians
c            per day. in the case of nutations the first four words of
c            rrd will be set to nutations and rates, having units of
c            radians and radians/day.
c
c            the option is available to have the units in km and km/sec.
c            for this, set km=.true. in the stcomx common block.
c
      implicit double precision (a-h,o-z)
      dimension rrd(6),et2z(2),et2(2),pv(6,13)
      dimension pvsun(6)
c header
      logical bsave,km,bary
      common/stcomx/km,bary,pvsun
c standard header  
      include 'jplhdr.h'
      integer list(12)
* memory model static
      save

      et2(1)=et2z(1)
      et2(2)=et2z(2)

  11  ettot=et2(1)+et2(2)

      do i=1,6
        rrd(i)=0.d0
      enddo

  96  if(ntarg .eq. ncent) return

      do i=1,12
        list(i)=0
      enddo
c
c     check for nutation call
c
      if(ntarg.eq.14)then
        if(ipt(2,12).gt.0) then
          list(11)=2
          call state(et2,list,pv,rrd,istate)
          return
        else
          write(6,297)
  297     format(' *****  no nutations on the ephemeris file  *****')
          stop
        endif
      endif
c
c     check for librations
c
      if(ntarg.eq.15)then
        if(lpt(2).gt.0) then
          list(12)=2
          call state(et2,list,pv,rrd,istate)
          do i=1,6
          rrd(i)=pv(i,11)
          enddo
          return
        else
          write(6,298)
  298     format(' *****  no librations on the ephemeris file  *****')
          stop
        endif
      endif
c
c       force barycentric output by 'state'
      bsave=bary
      bary=.true.
c
c       set up proper entries in 'list' array for state call
c
      do i=1,2
        k=ntarg
        if(i .eq. 2) k=ncent
        if(k .le. 10) list(k)=2
        if(k .eq. 10) list(3)=2
        if(k .eq. 3) list(10)=2
        if(k .eq. 13) list(3)=2
      enddo
c
c       make call to state
c
      call state(et2,list,pv,rrd,istate)
c
      if(ntarg .eq. 11 .or. ncent .eq. 11) then
         do i=1,6
           pv(i,11)=pvsun(i)
         enddo
      endif
c
      if(ntarg .eq. 12 .or. ncent .eq. 12) then
         do i=1,6
           pv(i,12)=0.d0
         enddo
      endif

      if(ntarg .eq. 13 .or. ncent .eq. 13) then
         do i=1,6
           pv(i,13)=pv(i,3)
         enddo
      endif

      if(ntarg*ncent .eq. 30 .and. ntarg+ncent .eq. 13) then
         do i=1,6
           pv(i,3)=0.d0
         enddo
         go to 99
      endif

      if(list(3) .eq. 2) then
         do i=1,6
           pv(i,3)=pv(i,3)-pv(i,10)/(1.d0+emrat)
         enddo
      endif

      if(list(10) .eq. 2) then
         do i=1,6
           pv(i,10)=pv(i,3)+pv(i,10)
         enddo
      endif

  99  do i=1,6
        rrd(i)=pv(i,ntarg)-pv(i,ncent)
      enddo

      bary=bsave

      return
      end
