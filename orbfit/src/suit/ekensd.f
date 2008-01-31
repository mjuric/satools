* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: February 24, 1997
*
* Transformation between keplerian and non-singular (equinoctal) elements,
* with jacobian matrix
* (all angles in radians)
*
* INPUT:    elem      -  keplerian elements
*                            elem(1) = semimajor axis
*                            elem(2) = eccentricity
*                            elem(3) = inclination
*                            elem(4) = longitude of ascending node
*                            elem(5) = argument of pericenter
*                            elem(6) = mean anomaly
*
* OUTPUT:   ekns      -  non-singular elements
*                            ekns(1) = semimajor axis
*                            ekns(2) = h = e*sin(varpi)
*                            ekns(3) = k = e*cos(varpi)
*                            ekns(4) = P = tan(i/2)*sin(Omega)
*                            ekns(5) = Q = tan(i/2)*cos(Omega)
*                            ekns(6) = mean longitude
*           dkdns     -  jacobian matrix d(ekns)/d(elem)
*
      subroutine ekensd(elem,ekns,dkdns)
      implicit none

      double precision elem(6),ekns(6),dkdns(6,6)
      double precision alp,ti2,sinalp,cosalp,dti2,sinom,cosom
      integer i,k

      include 'trig.h'

      ekns(1)=elem(1)
* Longitude of pericenter
      alp=elem(5)+elem(4)
      sinalp=sin(alp)
      cosalp=cos(alp)
      ekns(2)=elem(2)*sinalp
      ekns(3)=elem(2)*cosalp
      ti2=tan(elem(3)/2)
      sinom=sin(elem(4))
      cosom=cos(elem(4))
      ekns(4)=ti2*sinom
      ekns(5)=ti2*cosom
      ekns(6)=alp+elem(6)
      ekns(6)=mod(ekns(6),dpig)

      do 1 i=1,6
      do 1 k=1,6
 1    dkdns(i,k)=0.d0

      dkdns(1,1)=1.d0
      dkdns(2,2)=sinalp
      dkdns(2,5)=ekns(3)
      dkdns(2,4)=ekns(3)
      dkdns(3,2)=cosalp
      dkdns(3,5)=-ekns(2)
      dkdns(3,4)=-ekns(2)
      dti2=1.d0/(2*cos(elem(3)/2)**2)
      dkdns(4,3)=dti2*sinom
      dkdns(4,4)=ekns(5)
      dkdns(5,3)=dti2*cosom
      dkdns(5,4)=-ekns(4)
      dkdns(6,4)=1
      dkdns(6,5)=1
      dkdns(6,6)=1

      end
