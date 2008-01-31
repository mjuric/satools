c========================
c  pridif
c  difference of two angles, 
c  expressed as principal values
       double precision function pridif(a,b)
       implicit none
       include 'trig.h'
       double precision a,b,princ

       a=princ(a)
       b=princ(b)
       pridif=a-b
       if(pridif.gt.pig)then
           pridif=pridif-dpig
       elseif(pridif.lt.-pig)then
           pridif=pridif+dpig
       endif
       return
       end

