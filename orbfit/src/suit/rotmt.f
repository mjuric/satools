* Author: Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 5, 1996
*
*  ***************************************************************
*  *                                                             *
*  *                          R O T M T                          *
*  *                                                             *
*  *              Rotation matrix around k axis	                 *
*  *                                                             *
*  ***************************************************************
*
*  If X are "old" coordinates and X' are "new" coordinates (referred
*  to a frame which is rotated by an angle alpha around k axis in
*  direct sense), then X' = R X
*
      subroutine rotmt(alpha,r,k)
      implicit none

      double precision alpha,r(3,3)
      double precision cosa,sina
      integer k,i1,i2,i3

      if(k.lt.1.or.k.gt.3)stop' **** ROTMT: k = ??? ****'
      cosa=cos(alpha)
      sina=sin(alpha)
      i1=k
      if(i1.gt.3)i1=i1-3
      i2=i1+1
      if(i2.gt.3)i2=i2-3
      i3=i2+1
      if(i3.gt.3)i3=i3-3

      r(i1,i1)=1.d0
      r(i1,i2)=0.d0
      r(i1,i3)=0.d0
      r(i2,i1)=0.d0
      r(i2,i2)=cosa
      r(i2,i3)=sina
      r(i3,i1)=0.d0
      r(i3,i2)=-sina
      r(i3,i3)=cosa

      end
