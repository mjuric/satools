      subroutine matvet(b,nr,nc,vec)
      implicit double precision (a-h,o-z)
      dimension b(nr,nc),vec(nr*nc)
      do 21 j=1,nc
        do 22 i=1,nr
           vec(nr*(j-1)+i)=b(i,j)
 22    continue
 21   continue
      return
      end
