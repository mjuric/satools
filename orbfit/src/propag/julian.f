       subroutine julian(iy,imo,iday,ihr,imin,sec,jd)
c
c      compute julian date using the algorithm given by van flandern
c      and pulkkinen, ap.j. suppl. vol 41, page 392.
c
c      input arguments are iy...sec
c
c      output is the real julian date: jd
c
      implicit none
      double precision jd,sec
      integer ihr,imin,iy,imo,iday,iaux
      iaux=-7*(iy+(imo+9)/12)/4-3*((iy+(imo-9)/7)/100+1)/4+275*imo/9
      jd=iaux+iday+367.d+0*iy+1721028.5d+0
     +     +(ihr+imin/60.d+0+sec/3600.d+0)/24.d+0
      return
      end
