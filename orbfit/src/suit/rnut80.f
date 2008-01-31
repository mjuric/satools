* Author: Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 5, 1996
*
*  ***************************************************************
*  *                                                             *
*  *                         R N U T 8 0                         *
*  *                                                             *
*  *    Nutation matrix according to Wahr (IAU-1980) theory      *
*  *                                                             *
*  ***************************************************************
*
*  INPUT:    TJM         -   Modified Julian Time (TDT)
*
*  OUTPUT:   RNUT(3,3)   -   Nutation matrix, transforming MEAN
*                            coordinates into TRUE coordinates
*                            Xtrue = RNUT Xmean
*
      subroutine rnut80(tjm,rnut)
      implicit none

      include 'trig.h'

      double precision tjm,rnut(3,3)
      double precision r1(3,3),r2(3,3),r3(3,3),rp(3,3)
      double precision epsm,epst,dpsi,deps
      double precision obleq
      external obleq
      integer i,j

      epsm = obleq(tjm)
      call nutn80(tjm,dpsi,deps)
      dpsi = dpsi * radsec
      epst = epsm + deps * radsec
      call rotmt(  epsm , r1 , 1)
      call rotmt( -dpsi , r2 , 3)
      call rotmt( -epst , r3 , 1)
      do 1 i=1,3
      do 1 j=1,3
 1    rp(i,j)=r2(i,1)*r1(1,j)+r2(i,2)*r1(2,j)+r2(i,3)*r1(3,j)
      do 2 i=1,3
      do 2 j=1,3
 2    rnut(i,j)=r3(i,1)*rp(1,j)+r3(i,2)*rp(2,j)+r3(i,3)*rp(3,j)

      end
