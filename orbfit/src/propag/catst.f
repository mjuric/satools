c *******************************************************************
c  {\bf catst} ORB8V
c   riempimento tavola differenze
      subroutine catst(m,m1,b,a,nvar2,nvar2x,nvar,delta,dd,y1,h,h2)
      implicit none
c order, number of variables (positions only)
      integer m,m1,nvar2,nvar2x,nvar
c multistep coefficients, table of differences, sums, state vector
      double precision b(m1),a(m1),delta(nvar2x,m1),
     +        dd(nvar2x,2),y1(nvar)
c loop indexes
      integer i,k,l
      double precision h,h2,dtemp
c****************
c   static memory not required
c****************
      do 2 i=1,nvar2
        do 2 k=1,m
          do 2 l=1,m-k+1
 2        delta(i,m+2-l)=delta(i,m+1-l)-delta(i,m+2-l)
c   prima e seconda somma determinate dalle condizioni iniziali
      do 3 i=1,nvar2
        dtemp=y1(i+nvar2)/h
        do 4 k=0,m
 4        dtemp=dtemp-a(k+1)*delta(i,k+1)
        dd(i,1)=dtemp
        dtemp=y1(i)/h2+dtemp
        do 5 k=0,m
 5        dtemp=dtemp-b(k+1)*delta(i,k+1)
        dd(i,2)=dtemp
 3    continue
      return
      end
c ***************************************************************
c  {\bf  kintrp} ORB8V
c
c  scopo : predizione dei valori di ck per interpolazione da quelli
c          del passo precedente (con polinomio di grado is-1)
c          serve per avere condizioni iniziali vicine al punto unito
c          nel procedimento iterativo per risolvere le equazioni
c          implicite la cui soluzione e' la matrice ck.
c  input :
c       ck1(ismax,nvar) : ck al passo precedente
c       is : numero di stadi del metodo rk
c       nvar : numero di variabili nell'equazione di moto
c  output:
c       ck(ismax,nvar) : valori interpolati
c  osservazione : non va usato nel passo iniziale e se rispetto al
c                 passo precedente e' cambiato il passo, oppure is.
c****************
c   static memory not required
c****************
      subroutine kintrp(ck1,ck,is,nvar)
      implicit none
      include 'parint.h'
c coefficients
      INCLUDE 'rkcoef.h'
c  dimensioni dipendenti da ismax (qui=ismax)
      integer nvar
      double precision ck1(ismax,nvar),ck(ismax,nvar)
c  numero di step intermedi
      integer is
c  indici di loop
      integer j,n,jj
c  temporanei
      double precision de
      do 1 j=1,is
        do 1 n=1,nvar
        de=0.d0
        do 2 jj=1,is
 2        de=de+a1(j,jj)*ck1(jj,n)
 1      ck(j,n)=de
      return
      end


