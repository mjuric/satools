c ******************************************************************
c  b  d  n  s  t  e
c
c  propagatore multistep
c  bds ordine m+2
c  esegue nstep passi, all'ultimo calcola anche le velocita'
c  con aggiornamento catasta a flip-flop
c
c  input:
c           h=passo; h2=h*h
c           m=max ord diff da tenere (predittore ha ord.m+3)
c           c=Cow pred b=Cow corr f=Ad pred a=Ad corr
c           nvar=dim di y1; nvar2=nvar/2
c           fct2=nome secondo membro (forma con solo der. 2e)
c  input e output:
c           t1=tempo (esce invariato)
c           y1=vett stato pos+vel (esce aggiornato se converge)
c           j1=0,1 indice flip=flop per l'indirizzamento
c           delta=differenze; dd=somme
c
c  versione predittore soltanto
      subroutine bdnste(t1,y1,h,h2,nstep,m,j1,dd,delta,nvar2,nvar2x,
     $                  nvar,fct2)
      implicit none
c header files
      include 'parint.h'
      include 'mscoef.h'
      include 'model.h'
*   number of steps, order-2,flipflop control,
      integer nstep,m,j1
*   time, stepsize
      double precision t1,tt,h,h2
c name of right hand side
      external fct2
*   workspace
      integer nvar,nvar2,nvar2x
      double precision delta(nvar2x,m2max),dd(nvar2x,4),y1(nvar)
*   close approaching planet  
      integer idc
      double precision xxpla(6)
*   indexes for address computations
      integer id1,id2,in1,in2,kmax,kmin,kj1,kj2,kj2p
*   loop indexes
      integer i,k,n     
*   scalar temporary
      double precision dtemp
* **************************************
c static memory not required
* **************************************
c
c  loop sul numero di passi
      tt=t1
      do 1 n=1,nstep
c
c  indirizzamento controllato dal flip-flop j1
      if(j1.eq.0)then
         id2=2
         id1=1
         in2=4
         in1=3
         kmax=m+1
         kmin=1
         kj1=0
         kj2=m+1
      else
         id2=4
         id1=3
         in2=2
         in1=1
         kmax=2*m+2
         kmin=m+2
         kj1=m+1
         kj2=0
      endif
c
c  predittore
c
      if(m.eq.10)then
         do  i=1,nvar2
           y1(i)=h2*(delta(i,kmax)*c(kmax)+delta(i,kmax-1)*c(kmax-1)+
     +         delta(i,kmax-2)*c(kmax-2)+delta(i,kmax-3)*c(kmax-3)+
     +         delta(i,kmax-4)*c(kmax-4)+delta(i,kmax-5)*c(kmax-5)+
     +         delta(i,kmax-6)*c(kmax-6)+delta(i,kmax-7)*c(kmax-7)+
     +         delta(i,kmax-8)*c(kmax-8)+delta(i,kmax-9)*c(kmax-9)+
     +         delta(i,kmax-10)*c(kmax-10)+dd(i,id2))
         enddo
      elseif(m.eq.6)then
         do  i=1,nvar2
           y1(i)=h2*(delta(i,kmax)*c(kmax)+delta(i,kmax-1)*c(kmax-1)+
     +         delta(i,kmax-2)*c(kmax-2)+delta(i,kmax-3)*c(kmax-3)+
     +         delta(i,kmax-4)*c(kmax-4)+delta(i,kmax-5)*c(kmax-5)+
     +         delta(i,kmax-6)*c(kmax-6)+dd(i,id2))
         enddo
      else
         do 11 i=1,nvar2
          dtemp=0.d0
          do 12 k=kmax,kmin,-1
 12         dtemp=dtemp+delta(i,k)*c(k)
 11         y1(i)=(dtemp+dd(i,id2))*h2
      endif
      tt=tt+h
      IF(icrel.gt.0)THEN
         do  i=1,nvar2
            dtemp=0.d0
            do k=kmax,kmin,-1
               dtemp=dtemp+delta(i,k)*f(k)
            enddo
            y1(i+nvar2)=h*(dtemp+dd(i,id1))
         enddo
      ENDIF
c
c   accelerazione nel nuovo punto
      kj2p=kj2+1
      call fct2(y1,y1(nvar2+1),tt,delta(1,kj2p),nvar2,idc,xxpla)
c ==============================
c close approach control
      CALL clocms(idc,tt,xxpla)
c ===============================
c   aggiornamento catasta
      do 15 i=1,nvar2
       dd(i,in1)=dd(i,id1)+delta(i,kj2p)
       dd(i,in2)=dd(i,id2)+dd(i,in1)
 15   continue
      do 16 k=1,m
        do 16 i=1,nvar2
 16       delta(i,k+1+kj2)=delta(i,k+kj2)-delta(i,k+kj1)
c ============================================
c   passo concluso
      j1=1-j1
c   aggiungere un correttore per controllo?
c
c   fine loop sul numero di passi
 1    continue
c   calcolo velocita', se non gia' fatto
      IF(icrel.eq.0)THEN
         do  i=1,nvar2
            dtemp=0.d0
            do k=kmax,kmin,-1
               dtemp=dtemp+delta(i,k)*f(k)
            enddo
            y1(i+nvar2)=h*(dtemp+dd(i,id1))
         enddo
      ENDIF
      return
      end






