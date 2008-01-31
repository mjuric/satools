c========================
c  primea
c  arithmetic mean of two angles, 
c  expressed as principal values
       double precision function primea(a,b)
       implicit none
       include 'trig.h'
       double precision a,b,princ
       a=princ(a)
       b=princ(b)
       if(abs(b-a).gt.pig)then
           primea=(a+b)/2.d0+pig
           primea=princ(primea)
       else
           primea=(a+b)/2.d0
       endif
       return
       end

