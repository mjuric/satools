* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 7, 1997
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                           P R E C                           *
*  *                                                             *
*  *                      Precession matrix                      *
*  *                                                             *
*  ***************************************************************
*
*  INPUT:    TJM         -   Modified Julian Time (TDT)
*
*  OUTPUT:   RPREC(3,3)  -   Precession matrix, transforming J2000
*                            equatorial coordinates into mean
*                            equatorial coordinates at epoch TJM
*                            Xtjm = RPREC Xj2000
*
      subroutine prec(tjm,rprec)
      implicit none

      include 'trig.h'
      include 't2000.h'

      double precision tjm,rprec(3,3)
      double precision r1(3,3),r2(3,3),r3(3,3),rp(3,3)
      double precision t,zeta,theta,z
      double precision zed,zd,thd,zedd,zdd,thdd,zeddd,zddd,thddd
      integer i,j
      logical first

      save first,zed,zd,thd,zedd,zdd,thdd,zeddd,zddd,thddd
      data first/.true./
*
* Calcolo costanti usate (vedi Astronomical Almanac 1987, B18)
*
      if(first)then
          first=.false.
* Termine lineare
          zed   =   0.6406161d0 * radeg
          zd    =   0.6406161d0 * radeg
          thd   =   0.5567530d0 * radeg
* Termine quadratico
          zedd  =   0.0000839d0 * radeg
          zdd   =   0.0003041d0 * radeg
          thdd  = - 0.0001185d0 * radeg
* Termine cubico
          zeddd =   0.0000050d0 * radeg
          zddd  =   0.0000051d0 * radeg
          thddd = - 0.0000116d0 * radeg
      end if
*
* Calcolo argomenti fondamentali
*
      t     = ( tjm - t2000 ) / 36525.d0
      zeta  = ( ( zeddd * t + zedd ) * t + zed ) * t
      z     = ( (  zddd * t +  zdd ) * t +  zd ) * t
      theta = ( ( thddd * t + thdd ) * t + thd ) * t
*
* Calcolo matrice di rotazione
*
      call rotmt(- zeta , r1 , 3)
      call rotmt( theta , r2 , 2)
      call rotmt(-    z , r3 , 3)
      do 1 i=1,3
      do 1 j=1,3
 1    rp(i,j)=r2(i,1)*r1(1,j)+r2(i,2)*r1(2,j)+r2(i,3)*r1(3,j)
      do 2 i=1,3
      do 2 j=1,3
 2    rprec(i,j)=r3(i,1)*rp(1,j)+r3(i,2)*rp(2,j)+r3(i,3)*rp(3,j)

      end
