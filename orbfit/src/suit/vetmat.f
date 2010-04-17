      subroutine vetmat(vec,nv,b,nr,nc)
      implicit double precision (a-h,o-z)      
      dimension vec(nv),b(nr,nc)
      if ((nr*nc).ne.nv) then
        write(99,*)'error: nr*nc must be = nv'
        stop
      endif
      do 12 j=1,nc
        do 13 i=1,nr
          b(i,j)=vec(nr*(j-1)+i)
 13     continue
 12   continue
      return
      end
