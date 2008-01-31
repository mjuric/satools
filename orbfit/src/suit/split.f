c+++++++++++++++++++++++++
c
      subroutine split(tt,fr)
c
c+++++++++++++++++++++++++
c
c     this subroutine breaks a d.p. number into a d.p. integer
c     and a d.p. fractional part.
c
c     calling sequence parameters:
c
c       tt = d.p. input number
c
c       fr = d.p. 2-word output array.
c            fr(1) contains integer part
c            fr(2) contains fractional part
c
c            for negative input numbers, fr(1) contains the next
c            more negative integer; fr(2) contains a positive fraction.
c
c       calling sequence declarations
c
      implicit double precision (a-h,o-z)

      dimension fr(2)
      save

c       main entry -- get integer and fractional parts

      fr(1)=dint(tt)
      fr(2)=tt-fr(1)

      if(tt.ge.0.d0 .or. fr(2).eq.0.d0) return

c       make adjustments for negative input number

      fr(1)=fr(1)-1.d0
      fr(2)=fr(2)+1.d0

      return
      end
