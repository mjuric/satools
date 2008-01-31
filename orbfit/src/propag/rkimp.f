c ***************************************************************
c  RKIMP ORBFIT
c
c  scopo : propagatore che compie un passo con
c       il metodo di runge kutta implicito (formula di gauss)
c       ad is passi. il numero di iterazioni non deve superare
c       lit .  fct e' il secondo membro, che riduce al primo
c       ordine il vero primo membro fct2
c  input:
c      t1 : tempo iniziale
c      h : lunghezza del passo da eseguire
c      y1(nvar) : vettore di stato al tempo t1
c      ck(ismax,nvar) : valori iniziali per l'equazione implicita
c      epsi : controllo di convergenza per l'equazione implicita
c      ndim : no. variabili da usare per la norma da cfr. con epsi
c  output :
c      t1: invariato
c      y3(nvar) : stato al tempo t1+h (non puo' avere lo stesso
c               indirizzo di y1)
c      ep(i) : controlli numerici;per i=1,lit converg.in
c              gauss-seidel
c      dery(nvar) : spazio di lavoro per il calcolo del secondo membro
c      lf : flag che e' >0 se c'e' stata soddisfacente convergenza
c              altrimenti segnala problemi
c  codice indici: i=1,nvar; j=1,is; id=1,ndim; it=indice di iterazione
      subroutine rkimp(t1,h,y1,dery,ck,is,y3,lit,fct2,
     +     nvar,epsi,ep,lfleps,ndim)
      IMPLICIT NONE
c coefficients
      INCLUDE 'parint.h'
      INCLUDE 'rkcoef.h'
c  dimensioni variabili
      integer nvar
      double precision y1(nvar),dery(nvar),y3(nvar)
      double precision ep(itmax),ck(ismax,nvar),t(ismax)
c name of right hand side
      external fct2
c  tempo, passo
      double precision t1,h
c  indici di iterazione, di sottopasso, di dimensione, flag
      integer it,lit,j,i,is,jj,ndim,id,lfleps
c  controlli
      double precision epsi
c  temporanei
      double precision de
c****************
c   static memory not required
c****************
c ===============================================
      do 8 j=1,is
 8      t(j)=t1+h*c(j)
      do 7 it=1,itmax
 7      ep(it)=0.d0
c
c  gauss-seidel per i ck
c  inizio iterazioni per i ck
      it=1
c  main loop
 1    do 11 j=1,is
        do 12 i=1,nvar
          de=0.d0
          do 13 jj=1,is
 13         de=de+a(j,jj)*ck(jj,i)
 12       y3(i)=de*h+y1(i)
        call fct(t(j),y3,dery,nvar,fct2)
        do 14 i=1,ndim
 14        ep(it)=ep(it)+dabs(dery(i)-ck(j,i))
        do 15 id=1,nvar
 15        ck(j,id)=dery(id)
 11   continue
c  controllo se le iterazioni g-s sono finite
      ep(it)=ep(it)/is
      lfleps=it
      if(ep(it).gt.epsi)then
         if(it.ge.lit)then
c  troppe iterazioni in gauss-seidel
c  il nuovo valore non viene calcolato
            lfleps=-it
            return
          else
            it=it+1
            goto 1
          endif
      endif
c
c  calcolo nuovo punto y3
      do 41 i=1,nvar
        de=0.d0
        do 41 j=1,is
          de=de+b(j)*ck(j,i)
 41       y3(i)=y1(i)+h*de
      return
      end
c ===========================================================
c   FCT
c   right hand side routine
c   reduces equations to order 1
c   starting from accelerations computed by force
      subroutine fct(t,y,dery,nvar,fct2)
      implicit none
      integer nvar,nvar2,idc,i
      double precision y(nvar),dery(nvar)
      double precision xxpla(6)
      double precision t
c name of right hand side
      external fct2
      INCLUDE 'proout.h'
      INCLUDE 'parbep.h'
      INCLUDE 'masses.h'
      INCLUDE 'iclap.h'
c****************
c   static memory not required
c****************
      nvar2=nvar/2
      call fct2(y,y(nvar2+1),t,dery(nvar2+1),nvar2,idc,xxpla)
      IF(iorb.eq.11)THEN
         if(iclap.ne.0.and.idc.ne.0)then
* to be improved with a real safety feature
            write(*,*)'t =',t,' close approach to planet=',
     +           ordnam(idc)
            write(iuncla,*)'t =',t,' close approach to planet=',
     +           ordnam(idc)
         endif
      ELSEIF(iorb.eq.9)THEN 
         if(idc.ne.0)then
            write(*,*)'t =',t,' close approach code=',idc
            write(iuncla,*)'t =',t,' close approach code =',idc
         endif
      ENDIF
      do i=1,nvar2
        dery(i)=y(nvar2+i)
      enddo
      return
      end

