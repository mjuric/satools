c =====================================================================
c matrix multiplied by column vector
      subroutine mulmav(a,na,ma,b,nb,c)
      implicit none
      integer na,ma,nb,i,k
      double precision a(na,ma),b(nb),c(na)
      if (ma.ne.nb) then
         write (*,*) 'dimensioning error in mulmav'
         stop
      endif
      do 3 i=1,na
        c(i)=0.d0
        do 1 k=1,ma
          c(i)=c(i)+a(i,k)*b(k)
 1      continue
 3    continue
      return
      end      
