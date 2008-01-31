* Copyright (C) 1999 by Andrea Milani
* Version: January 19, 1999
*
*  ***************************************************************
*  *                                                             *
*  *                          A B E R 2                          *
*  *                                                             *
*  *                                                             *
*  ***************************************************************
*
*
* INPUT:    XREL(6)   -  relative position and velocity
*           XAST(6)   -  position and velocity of asteroid
*
* OUTPUT:   XCOR(6)   -  corrected pos. and vel.
*
*  WARNING: this is first order  in v/c
*
      subroutine aber2(xrel,xast,xcor)
      implicit none
      double precision xrel(6),xast(6),xcor(6)
      INCLUDE 'vlight.h'
      INCLUDE 'sunmass.h'
      double precision ro,dt,vsize
      integer j
c  delay
      ro=vsize(xrel)
      dt=ro/vlight
c position correction (linear in dt)
      do  j=1,3
        xcor(j)=xrel(j)-dt*xast(j+3)
      enddo
c velocity correction (linear in dt)
      ro=vsize(xast)
      do j=1,3
        xcor(j+3)=xrel(j+3)+dt*gms*xast(j)/ro**3
      enddo
      return
      end



