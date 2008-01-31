* Author: Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 5, 1996
*
*  ***************************************************************
*  *                                                             *
*  *                           T R S P                           *
*  *                                                             *
*  *                  Transpose of a 3x3 matrix                  *
*  *                                                             *
*  ***************************************************************
*
*
* INPUT:    R(3,3)   -  3x3 matrix
*
* OUTPUT:   R(3,3)   -  Transpose of input matrix
*
      subroutine trsp(r)
      implicit none

      double precision r(3,3),rt
      integer i,j

      do 1 i=1,2
      do 1 j=i+1,3
      rt=r(i,j)
      r(i,j)=r(j,i)
 1    r(j,i)=rt
      end
