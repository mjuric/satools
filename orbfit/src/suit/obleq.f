* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 7, 1997
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                          O B L E Q                          *
*  *                                                             *
*  *                 Mean obliquity of ecliptic                  *
*  *            (see Astronomical Almanac 1987, B18)             *
*  *                                                             *
*  ***************************************************************
*
*  INPUT:    TJM   -   Modified Julian Time (TDT)
*
*  OUTPUT:   in radians
*
      double precision function obleq(tjm)
      implicit none

      include 'trig.h'
      include 't2000.h'

      double precision tjm
      double precision ob0,ob1,ob2,ob3,t
      logical first

      save first,ob0,ob1,ob2,ob3

      data first/.true./
      if(first)then
          first=.false.
* IAU value
*         ob0   =  (float(23*3600+26*60)+21.45d0) * radsec
* Improved value
          ob0   =  (float(23*3600+26*60)+21.448d0) * radsec
          ob1   =  -46.815d0  * radsec
          ob2   =  -0.0006d0  * radsec
          ob3   =   0.00181d0 * radsec
      end if
      t     = ( tjm - t2000 ) / 36525.d0
      obleq = (( ob3 * t + ob2 ) * t + ob1 ) * t + ob0
      end
