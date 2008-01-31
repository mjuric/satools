* Author: Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 5, 1996
*
*  ***************************************************************
*  *                                                             *
*  *                         E Q U E Q U                         *
*  *                                                             *
*  *                 Equation of the equinoxes                   *
*  *                                                             *
*  ***************************************************************
*
*
* INPUT:    TJM       -  Modified Julian Time (TDT)
*
* OUTPUT:   EQUEQU    -  Equation of the equinoxes (difference
*                        between apparent sidereal time and mean
*                        sidereal time) in radians
*
      double precision function equequ(tjm)
      implicit none

      include 'trig.h'

      double precision tjm
      double precision oblm,dpsi,deps

      double precision obleq
      external obleq

      oblm = obleq(tjm)
      call nutn80(tjm,dpsi,deps)
      equequ=radsec*dpsi*cos(oblm)

      end
