      subroutine trange
      implicit none
*
      include 'timespan.h'
      include 'vlight.h'
*
      double precision et2(2),pv(6,12),pnut(4)
      integer list(12)
      double precision tt,deltt
*
* JPL  header
      include 'jplhdr.h'
      character*6 cnam(400),ttl(14,3)
      common/chrhdr/cnam,ttl
*
      data et2/2*0.d0/
      data list/12*0/
*
* Dummy call to STATE for reading JPLDE header
      CALL state(et2,list,pv,pnut,1)
* store in common timespan time span of jpleph
* transformation from JD to MJD
      tejpl1=ss(1)-2400000.5d0
      tejpl2=ss(2)-2400000.5d0
*      write(99,*) tejpl1,tejpl2
*
c Dummy call to deltt to read the ET-UT data
      tt=deltt(50000.d0)
c speed of light (IAU 1976) in km/s
      ckm=299792.458d0
c id in km/day 
      ckm=ckm*8.64d4
c conversion to AU/day
      vlight=ckm/au
      return
      end


