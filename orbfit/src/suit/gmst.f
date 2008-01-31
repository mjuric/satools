* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 7, 1997
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                           G M S T                           *
*  *                                                             *
*  *                Greenwich Mean Sidereal Time                 *
*  *                    as a function of UT1                     *
*  *                                                             *
*  ***************************************************************
*
*
* INPUT:    TJM       -  Modified Julian Time (UT1)
*
* OUTPUT:   GMST      -  Greenwich Mean Sidereal Time referred
*                        to the mean equinox of date (rad)
*
      double precision function gmst(tjm)
      implicit none

      include 'trig.h'
      include 't2000.h'

      double precision tjm

      double precision c0,c1,c2,c3,rap
      parameter (c0=24110.54841d0,c1=8640184.812866d0,
     +           c2=9.3104d-2,c3=-6.2d-6)
      parameter (rap=1.00273790934d0)

      integer itjm,i
      double precision t,gmst0,h

* Sidereal time at 0h UT1
      itjm=tjm
      t=(itjm-t2000)/36525.d0
      gmst0=((c3*t+c2)*t+c1)*t+c0
      gmst0=gmst0*dpig/86400.d0

* Increment in GMST from 0h
      h=(tjm-itjm)*dpig
      gmst=gmst0+h*rap
      i=gmst/dpig
      if(gmst.lt.0.d0)i=i-1
      gmst=gmst-i*dpig

      end
