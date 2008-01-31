c ================================================
c START   Estimate of the mean motion and semimajor axis
c         allowing to combine two arcs
c Input   eq0,eqp elements of both arcs
c         t0,tp epoch times of elements of 1st and 2nd arc
c         gms Gauss constant
c         iorb select orbit 1,2
c Output  ng number of revolutions 
c         eng mean motion value for central time
c         am  semimajor axis value
c         plm mean longitude value
c ===============INTERFACE========================
      subroutine start(eq0,eqp,t0,tp,gms,iorb,ng,enm,am,plm)
      implicit none
c ===========INPUT==================
      double precision eq0(6),eqp(6),t0,tp,gms
      integer iorb
c ===========OUTPUT=================
      double precision enm,am,plm
      integer ng
c ==========END INTERFACE===========
      double precision en1,pl12,pl12p,en2,pl21,pl21p,enm0,enmp
      double precision tm,pll1,pll2
      integer ng0,ngp
c functions
      double precision primea
      integer ifloor
c ========INCLUDE HEADERS==========================
      include 'trig.h'
c =================================================
c Prediction of $\lambda$ at time tp, starting from elements at time t0
      en1=sqrt(gms/eq0(1)**3)
      pl12=eq0(6)+en1*(tp-t0)
      ng0=ifloor(pl12/dpig)
      pl12p=pl12-ng0*dpig
c The number of revolutions is computed to reduce the discrepancies in 
c lambda between the two arcs
      if(eqp(6)-pl12p.gt.pig)then
         ng0=ng0-1
      elseif(pl12p-eqp(6).gt.pig)then
         ng0=ng0+1
      endif
c Prediction of $\lambda$ at time t0, starting from elements at time tp
      en2=sqrt(gms/eqp(1)**3)
      pl21=eqp(6)+en2*(t0-tp)
      ngp=ifloor(pl21/dpig)
      pl21p=pl21-ngp*dpig
c The number of revolutions is computed to reduce the discrepancies in
c $\lambda$ \hfil\break
c between the two arcs
      if(eq0(6)-pl21p.gt.pig)then
         ngp=ngp-1
      elseif(pl21p-eq0(6).gt.pig)then
         ngp=ngp+1
      endif
c Computation of mean mean motion using the 2-body model, starting
c firstly from arc 1 and then from arc 2
      enm0=(eqp(6)-eq0(6)+ng0*dpig)/(tp-t0)
      enmp=(eq0(6)-eqp(6)+ngp*dpig)/(t0-tp)
c Choose  the best orbit to start from
      write(*, 177)ng0,ngp
 177  format(' number of rev. ',i4, i4) 
      if(iorb.eq.1)then
         enm=enm0
         ng=ng0
      elseif(iorb.eq.2)then
          enm=enmp
          ng=-ngp
       else
          write(*,*)' start: iorb=',iorb
          stop
       endif
      am=(gms/enm**2)**(1.d0/3.d0)
c 1997 change:use estimated mean motion to compute mean
c longitude at time in the middle
      tm=(t0+tp)/2.d0
      pll1=eq0(6)+enm*(tm-t0)
      pll2=eqp(6)+enm*(tm-tp)
      plm=primea(pll1,pll2)
      return
      end
c =====================================================================
      function ifloor(a)
      double precision a
      ifloor=a
      if(a.lt.0)ifloor=ifloor-1
      return
      end

