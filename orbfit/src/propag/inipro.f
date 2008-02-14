c ================================================================
c INIPRO
c ================================================================
c this subroutine reads from file 'propag.def' all the propagator options;
c  WARNING: we advise the user against changing the file propag.def;
c                do it at your risk !....
c  options:
c  control of propagation methods:
c           imet =1 (multistep) =2 (runge-kutta) =3 (everhart)
c             =0 automatic (multistep for main belt, Everhart for high
c                 eccentricity and/or planet crossing)
c    Runge-Kutta-Gauss: (imet=2; also used as starter for imet=1)
c           isrk = order of the method is isrk*2
c           h = integration step
c           eprk = convergence control in the solution of the implicit eq.
c           lit1,lit2 = no. of Gauss-Seidel iterations (first step, afterwards)
c 
c    multistep:
c           mms = multistep number of previous steps
c         WARNING: order is mms+2; e.g in Milani and Nobili, 1988, m=mms+2
c 
c    everhart:
c           h = stepsize (fixed if  llev.le.0; initial if llev.gt.0))
c           llev = control is 10**(-llev)
c 
c           iusci = control of output numerical parameters
c ================================================================
      subroutine inipro
* ************************************
      implicit none
* model parameters
      include 'model.h'
      include 'parint.h'
      include 'comint.h'
      include 'proout.h'
      include 'mscoef.h'
      logical fail,fail1,found
      double precision h
      integer iork,iord,isfl
      integer j
c****************
c   static memory not required (used only once)
c****************
      fail=.false.
*   read options from already assigned input channel
      call rdnint('propag.','imet',imet,.true.,found,fail1,fail)
      call rdnint('propag.','llev',llev,.true.,found,fail1,fail)
      call rdnrea('propag.','hev',hev,.true.,found,fail1,fail)
      call rdnrea('propag.','h',h,.true.,found,fail1,fail)
      hms=h
      call rdnrea('propag.','deltos',deltos,.true.,found,fail1,fail)
      call rdnrea('propag.','error',error,.true.,found,fail1,fail)
      call rdnint('propag.','iord',iord,.true.,found,fail1,fail)
* iord= multistep order; mms=number of back steps required to start
      mms=iord-2
      call rdnrea('propag.','epms',epms,.true.,found,fail1,fail)
      call rdnint('propag.','iork',iork,.true.,found,fail1,fail)
      isrk=iork/2
      call rdnrea('propag.','eprk',eprk,.true.,found,fail1,fail)
      call rdnint('propag.','lit1',lit1,.true.,found,fail1,fail)
      call rdnint('propag.','lit2',lit2,.true.,found,fail1,fail)
      call rdnint('propag.','iusci',iusci,.true.,found,fail1,fail)
      call rdnint('propag.','icha',icha,.true.,found,fail1,fail)
      if(fail)stop '**** inipro: abnormal end ****'
*   initialize multistep and implicit Runge-Kutta-Gauss
c ********************************************************************
c  input coefficients for Runge-Kutta
      if(imet.ne.3)then
         call legnum(isrk,isfl)
         if(isfl.ne.0)then
            write(0,997)isrk,isfl
 997        format(' required isrk=',i4,'  found only up to ',i4)
            stop
         endif
      endif
c ********************************************************************
c   calcolo coefficienti multistep
c   cambio notazione-nell'input iord=m nell'art. cel.mech.
c   d'ora in poi mms come in revtst, orbit8a
      if(mms.gt.mmax)then
         write(ipirip,998)mms,mmax
 998     format(' chiesto mms=',i4,'  spazio solo per ',i4)
         stop 998
      endif
c   calcolo coefficienti predittore
c   c=Cow pred f=Cow corr b=Ad pred a=Ad corr
c   warning: one order more than used, because c(m+2) is required
c   by the error formula
      call compco(mms+2,c,f,b,a)
      cerr=c(mms+2)
c  duplicazione per ridurre i calcoli di indici nel multistep
      do  j=1,mms+1
           c(j+mms+1)=c(j)
           f(j+mms+1)=f(j)
c  warning--i coeff per il correttore non sono pronti(dupl+primo)
      enddo
      return
      end





