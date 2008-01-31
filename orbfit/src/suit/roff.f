c ======================================================
c   {\bf roff}: machine accuracy tested in a machine independent way
c
c    warning: if multiple floating point operations use temporary storage
c    in the floating point unit, the result will indicate an 
c    illusory precision. This happens e.g. wtih f2c and g77.
c    fix: roff less than 2e-16 is refused
c
c    warning: roff is only the 1/2 the value of the last stored bit;
c    rounding off in multiple floating point operations should
c    average to a much smaller value in a computer with IEEE 754
c    floating point unit (see Knuth, chap. 4).
c   
c    in output nb-1 is the number of bits in the mantissa
c     (in base 2; in base 16 this is not that clear)
c ======================================================
      double precision function roff(nb)
      double precision roffp
      integer nbp
      logical first
      save first,roffp,nbp
      data first/.true./

      if(first) then
          roff=1.d0
          do 1 nb=1,200
            roff=roff/2.d0
            if(1.d0+roff.eq.1.d0)goto 2
 1        continue
 2        if(roff.lt.2.2d-16)roff=2.2d-16
          roffp=roff
          nbp=nb
          first=.false.
      else
          nb=nbp
          roff=roffp
      endif
      end
