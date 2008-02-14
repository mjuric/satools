c ===========================================================
c Cholewski method for inversion
      subroutine tchinv(c,n,cinv,ws,indp)
      implicit none
      integer n,indp
      double precision c(n,n),cinv(n,n),ws(n)
c end interface
      integer i
      double precision err,omax,omin,cond
      call mcopy(n,n,c,cinv)
      err=1d-14
c matrix factorized by Tcholesky method: 
      call tchol(cinv,n,n,indp,err)
      if(indp.ne.0)then
         write(0,*)' indp=',indp,' in tchol'
      endif
c ===========================================================
c Control of conditioning number and inversion of matrix
      omax=cinv(1,1)
      omin=cinv(1,1)
      do 19 i=2,n
        if (cinv(i,i).gt.omax) then
           omax=cinv(i,i)
        endif
        if (cinv(i,i).lt.omin) then
           omin=cinv(i,i)
        endif
 19   continue
      cond=(omax/omin)**2
      write(0,111)n,n,cond
 111  format(' Conditioning of ',i2,'x',i2,'matrix=',d12.4)
c ===========================================================
c inversion
      call inver(cinv,ws,n,n)
      return
      end

