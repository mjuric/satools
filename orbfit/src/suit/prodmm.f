* Author: Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 5, 1996
*
*  ***************************************************************
*  *                                                             *
*  *                         P R O D M M                         *
*  *                                                             *
*  *                 Product of two 3x3 matrices                 *
*  *                           A = BC                            *
*  *                                                             *
*  ***************************************************************
*
      subroutine prodmm(a,b,c)
      implicit none

      double precision a(3,3),b(3,3),c(3,3),s
      integer j,k,l

      do 2 j=1,3
      do 2 k=1,3
      s=0.d0
      do 1 l=1,3
 1    s=s+b(j,l)*c(l,k)
 2    a(j,k)=s

      end
