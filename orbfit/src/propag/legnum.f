c ***************************************************************
c  LEGNUM
c ***************************************************************
c reads  Runge--Kutta--gauss coefficients 
c to be read in file ./lib/rk.coef
c  is=required number of substeps; isfl=0 if found, otherwise
c  uses closest available
c ==============INTERFACE===========================
      subroutine legnum(is,isfl)
      implicit none
c =========INPUT============
      integer is
c ========OUTPUT============
      integer isfl
c ========HEADERS===========
      include 'parint.h'
c ======================================
c  coefficients RKG for rkimp
      INCLUDE 'rkcoef.h'
c input unit, current is
      integer iun,is1
c loop indexes
      integer i,j,jj
c skip trick
      character*1 cc(ismax)
c****************
c   static memory not required
c****************
c reads RKG coefficients rk
      isfl=-1
      call filopl(iun,'rk.coe')
 198  read(iun,100,end=199)is1
 100  format(6x,i4)
c  control on is compatible wtih parameter ismax
      if(is1.gt.ismax.or.is1.le.0)goto 199
      if(is1.eq.is)then
         read(iun,101)(c(j),j=1,is1)
 101     format(7x,5d24.16)
         read(iun,102)(b(j),j=1,is1)
 102     format(7x,5d24.16)
         do 103 j=1,is1
           read(iun,104)(a(i,j),i=1,is1)
 103     continue
 104     format(3x,5d24.16)
         do 105 j=1,is1
           read(iun,106)(a1(i,j),i=1,is1)
 106       format(4x,5d24.16)
 105     continue
         isfl=0
         goto 199
       else
         read(iun,201)(cc(j),j=1,is1)
 201     format(7x,5(23x,a1))
         read(iun,201)(cc(j),j=1,is1)
         do 203 j=1,is1
           read(iun,204)(cc(jj),jj=1,is1)
 204     format(3x,5(23x,a1))
 203     continue
         do 205 j=1,is1
           read(iun,206)(cc(jj),jj=1,is1)
 206     format(4x,5(23x,a1))
 205     continue
         isfl=is1
         goto 198
      endif
c end read
 199  call filclo(iun,' ')  
      return
      end
