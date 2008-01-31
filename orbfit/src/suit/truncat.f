c This function returns the truncated integer of the input. 
c Importantly it rounds to the nearest integer if the input 
c is within machine precision of an integer value
      integer function truncat(flt,eps)
      implicit none

      double precision flt,one,eps

      one=1.d0-eps
      truncat=flt
      if(abs(truncat-flt).ge.one) truncat=nint(flt)
      end
