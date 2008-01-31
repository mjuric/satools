c Copyright 1998, Orbfit Consortium
c Written Jan. 22,. 1999 by Steve Chesley
c SORTOB: accept a vector (time?) and return the permutation vector
C ASCENDING ORDER!
      subroutine sortob(t,iperm,n)
      implicit none
      include 'parobx.h'
c observations number, counters
      integer n,k,itmp,iperm(n)
      double precision t(n),tt(nobx),ttmp
      logical change

c Initialization of iperm
      do k=1,n
         iperm(k)=k
         tt(k)=t(k)
      enddo
c Sort
 3    change=.false.
      do k=1,n-1
        if(tt(k).gt.tt(k+1))then
          change=.true.
          itmp=iperm(k)
          iperm(k)=iperm(k+1)
          iperm(k+1)=itmp
          ttmp=tt(k)
          tt(k)=tt(k+1)
          tt(k+1)=ttmp
        endif
      enddo
      if(change)goto 3
      return
      end
