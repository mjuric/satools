* Author: Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 5, 1996
*
*  ***************************************************************
*  *                                                             *
*  *                          M U L T 3                          *
*  *                                                             *
*  *         Multiplication (to the left) of a 3x3 matrix        *
*  *                                                             *
*  ***************************************************************
*
* INPUT:    R,ROT     -  3x3 matrices
*
* OUTPUT:   ROT = R * ROT
*
      subroutine mult3(r,rot)
      implicit none

      double precision r(3,3),rot(3,3),rot1(3,3)
      integer i,j

      do 1 i=1,3
      do 1 j=1,3
 1    rot1(i,j)=r(i,1)*rot(1,j)+r(i,2)*rot(2,j)+r(i,3)*rot(3,j)
      do 2 i=1,3
      do 2 j=1,3
 2    rot(i,j)=rot1(i,j)
      end
