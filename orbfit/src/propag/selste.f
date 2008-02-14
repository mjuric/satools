* last update Fri Feb 21 1997
* =====================================================================
* SELSTE: selection of the stepsize for multistep
* =====================================================================
*  input: 
*         ecc eccentricity
*         enne mean motion
*  output: 
*         h recommended stepsize for multistep
* =====================================================================
      subroutine selste(ecc,enne,error,mms,hmax,h)
      implicit none
* input (hms max stepsize from common)(mms multistep order from common)
      INCLUDE 'parint.h'
      INCLUDE 'trig.h'
      INCLUDE 'mscoef.h'
      INCLUDE 'proout.h'
c input/output
      double precision ecc,enne,error,hmax,h
      integer mms
c scalar temporaries
      double precision eps,econv,hh,z,emul,step,err
      integer ila,igr,nb
c functions
      double precision roff
c***************
c   static memory not required
c****************
* =====================================================================
* the truncation error will be of the order of error
* per revolution squared; however, error cannot be less than
* machine accuracy, otherwise the rounding off would be the dominant
* source of integration error.
      eps=roff(nb)
      eps=max(eps,error)
      hh=hmax
* =====================================================================
* for each orbit, compute the truncation error in longitude
* according to Milani and Nobili, 1988, Celest. Mech. 43, 1--34
* and select a stepsize giving a truncation  error equivalent to
* one roundoff per revolution squared 
      econv=1.d0
c      write(ipirip,*)' Selection of the best stepsize'
      call  zed(ecc,mms+2,z,econv,ila,igr)
* if the series with Bessel functions is divergent, e.g. for e=0.2
* with iord=12, some wild guess is used because the error
* estimation formula is no good anyway; hope in your luck to have
* a not too bad orbit
*     if(ila.ge.20)z=1.d6
      if(mod(mms,2).eq.0)then
              emul=dpig**2*3/2*cerr*z
              step=(eps/emul)**(1.d0/(mms+3))/enne
              err=emul*(enne*hh)**(mms+3)
      else
              emul=dpig**2*3/4*mms*cerr*z
              step=(eps/emul)**(1.d0/(mms+4))/enne
              err=emul*(enne*hh)**(mms+4)
      endif
c     write(ipirip,*)enne,ecc,z
* =====================================================================
* now compare with h given in input
      h=min(step,hmax)
c      write(0,110)hms,h
c110  format(' max. step required ',f9.6,' selected ',f9.6)
      return
      end
c **********************************************************
c   {\bf compco} ORB8V
c
c   calcolo coefficienti multistep
c   cs=Cow pred bs=Cow corr fs=Ad pred as=Ad corr
c   versione con calcoli real*8 (trasportabile)
c   per calcoli in quadrupla precisione cambiare:
c      - tutti i .d0 in .q0
c      - real*16 a(mmax) etc
c   m1=m+1
c   i coefficienti per le differenze fino
c   all'ordine iord-2=m sono calcolati e restituiti
c   negli array con indice da 1 a m+1
      subroutine compco(m1,cs,fs,bs,as)
      implicit none
      include 'parint.h'
      integer m,m1,i,j,i1,i2,k
      double precision a(mmax),b(mmax),c(mmax),d(mmax)
      double precision as(m1),bs(m1),cs(m1),fs(m1)
c****************
c   static memory not required (used only once)
c****************
      m=m1-1
      a(1)=1.d0
      do 10 i=2,m+3
        k=i-1
        a(i)=0.d0
        do 10 j=1,k
 10       a(i)=a(i)-a(j)/(k-j+2.d0)
      do 11 j=1,m+3
        b(j)=0.d0
        do 11 k=1,j
 11       b(j)=b(j)+a(j+1-k)*a(k)
      do 12 i=1,m+3
        c(i)=0.d0
        d(i)=0.d0
        do 12 j=1,i
          c(i)=c(i)+a(j)
 12       d(i)=d(i)+b(j)
      do 17 i=2,m+2
        i1=i-1
        as(i1)=a(i)
 17     fs(i1)=c(i)
      do 18 i=3,m+3
        i2=i-2
        bs(i2)=b(i)
 18     cs(i2)=d(i)
c     write(6,200)(cs(i),fs(i),bs(i),as(i),i=1,m+1)
c200  format(4f19.16)
      return
      end
c**********************************************************************
c  {\bf zed}  ORB8V
c Calcolo di Z(m,e) -- correzione all'errore di troncamento
c dovuta alla eccentricita'. m e' l'ordine del multistep nella
c forma Stormer predictor; in LONGSTOP m=12.
c eps controllo di convergenza.
c
c Routine che calcola Z(m,e);imax numero massimo iterazioni (dato 20)
c**********************************************************************
      subroutine zed(e,m,f,eps,i,igr)
      implicit double precision (a-h,o-z)
      INCLUDE 'proout.h'
      data imax/20/
      f=0.d0
      adf=0.d0
      if(e.gt.0.2d0)then
         f=1.d6
c         write(ipirip,*)' with e>0.2 select the stepsize by hand'
         return
      elseif(e.gt.0.3d0)then
         f=1.d6
         write(ipirip,*)' with e>0.3 you should not use the multistep'
         return
      endif
      do 1 i=1,imax
        x=i*e
        ri=i
        cie=(bessel(i-1,x)-bessel(i+1,x))/i
        sie=(bessel(i-1,x)+bessel(i+1,x))/i
        c2ie=cie*cie
        s2ie=sie*sie
        sum=(c2ie+s2ie)/2.d0
        df=sum*ri**(m+4)
        f=f+df
cc      write(0,100)i,df
cc100   format(i5,d18.6)
        vadf=adf
        adf=dabs(df)
        if(adf.gt.vadf)igr=i
        if(adf.lt.eps.and.i.gt.1)return
 1    continue
      write(ipirip,101)i-1,df,igr
 101  format(' non convergence in zed; last term=',i5,d18.6/
     +       ' max was for ',i4)
      f=1.d6
      return
      end
c**********************************************************************
c  {\bf bessel}  ORB8V
c Function che calcola la funzione di Bessel J(i,x).
c imax numero massimo di iterazioni (dato 20)
c**********************************************************************
      double precision function bessel(i,x)
      implicit double precision (a-h,o-z)
      INCLUDE 'proout.h'
      data imax/20/
      data epbs/1.d-10/
      x2=x/2.d0
      x2p=x2
      ifat=1
      do 1 j=2,i
        x2p=x2p*x2
 1      ifat=ifat*j
      if(i.eq.0)x2p=1.d0
      jfat=1
      jpifat=1
      ifl=1
      bessel=0.d0
      do 2 j=1,imax
        dbess=x2p*ifl/ifat
        dbess=dbess/jfat
        dbess=dbess/jpifat
cc      write(0,*)dbess
        bessel=bessel+dbess
cc      write(0,*)i,j,ifat,jfat,jpifat
        if(dabs(dbess).lt.epbs)return
        jpifat=jpifat*(i+j)
        jfat=jfat*j
        ifl=-ifl
        x2p=x2p*x2*x2
 2    continue
      write(ipirip,101)j-1,dbess
 101  format('bessel: non convergence; last term=',i5,d18.6)
      return
      end
