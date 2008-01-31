      subroutine mulmat(a,na,ma,b,nb,mb,c)
      implicit double precision (a-h,o-z)
      dimension a(na,ma),b(nb,mb),c(na,mb)
      if (ma.ne.nb) then
         write (*,*) 'dimensioning error in mulmat'
         stop
      endif
      do 3 i=1,na
        do 2 j=1,mb
          c(i,j)=0.d0
          do 1 k=1,ma
 1           c(i,j)=c(i,j)+a(i,k)*b(k,j)
 2      continue
 3    continue
      return
      end
