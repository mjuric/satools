c+++++++++++++++++++++++++++++
c
      subroutine const(nam,val,sss,n)
c
c+++++++++++++++++++++++++++++
c
c     this entry obtains the constants from the ephemeris file
c
c     calling seqeunce parameters (all output):
c
c       nam = character*6 array of constant names
c
c       val = d.p. array of values of constants
c
c       sss = d.p. jd start, jd stop, step of ephemeris
c
c         n = integer number of entries in 'nam' and 'val' arrays
c
* changed by sabrina baccili on Wed Oct 30
*      save
c
      implicit double precision (a-h,o-z)
      character*6 nam(*)
      double precision val(*),sss(3)
c ***
      dimension pp1(2),ipp2(12),pp3(6,12),pp4(4)
c ***
      include 'jplhdr.h'

      character*6 ttl(14,3),cnam(400)
      common/chrhdr/cnam,ttl
      save
c  call state to initialize the ephemeris and read in the constants
* end change
c ***
      pp1(1)=0.d0
      pp1(2)=0.d0
      do 321 ijk=1,12
 321     ipp2(ijk)=0
      do 322 ijk=1,6
         do 323 ijj=1,12
 323        pp3(ijk,ijj)=0.d0
 322  continue
      do 324 iki=1,4
 324     pp4(iki)=0.d0
      istate =2
      call state(pp1,ipp2,pp3,pp4,istate)
c      call state(0.d0,0,0,0.d0)
c ***
      n=ncon

      do i=1,3
        sss(i)=ss(i)
      enddo
c
      do i=1,n
        nam(i)=cnam(i)
        val(i)=cval(i)
      enddo
c
      return
      end
