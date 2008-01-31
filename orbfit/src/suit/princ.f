*
* Computes the principal value of an angle
*
      double precision function princ (a)
      double precision a
*
      include 'trig.h'
*
      princ=dmod(a,dpig)
      if(princ.lt.0.d0)princ=princ+dpig
      return
      end
