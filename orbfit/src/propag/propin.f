c **********************************************************
c  PROPIN   ORBFIT version, Jun 13 1997
c  purpose: propagator/interpolator; the state vector y1
c           at time t1 is propagated to y2 at time t2
c  input:
c      nfl: if 0, integration needs to be restarted
c      t1: initial time
c      y1: state vector at t1
c      nfl: flag $>0$ if continuing, $=0$ if it has to restart
c      t2: final time
c      h: stepsize (only if it is fixed; ra15 uses hev as initial step)
c      nvar2: nvar/2
c      nvar: length(y1)
c  output:
c      t2: time to which y2 refers (different by .lt.deltos from input)
c      y2: state vector at time t2
c      t1,y1: time and state vector after last integration step
c     
c  osservazione: se nelle chiamate successive alla prima t1 ed y1 non
c                sono stati modificati dal programma chiamante,l'integra
c                zione prosegue a partire da t1,y1 usando i dati dei pas
c                si precedenti accumulati nelle cataste ck (per rkimp)
c                e dd, delta (per il multistep)
c                se invece t1,y1 sono stati modificati, il che risulta
c                da nfl=0,l'integrazione riprende con un passo iniziale.
c                se pero' t1,y1 non sono modificati,i tempi t2 devono es
c                sere in successione monotona consistente con h
c  subroutine chiamate:
c       ra15: metodo di everhart
c       force: external per chiamata secondo membro 
c       legnum :lettura coefficienti r-k
c       rkimp :propagatore runge-kutta
c       kintrp :interpolatore di ck
c       bdnste :multistep BDS che fa nstep passi
c       catst :caricatore catasta differenze e somme
c       coeff :calcolo coefficienti multistep
c       rkstep :variatore automatico del passo
c       fct :secondo membro equazioni differenziali (fisso; riduce
c            al primo ordine quanto fornito da force)
c   codice indici : i=1,nvar, j=1,isrk
c **********************************************************
      subroutine propin(nfl,y1,t1,t2,y2,h,nvar2,nvar)
      implicit none
c headers
      INCLUDE 'model.h'
      INCLUDE 'nvarx.h'
      INCLUDE 'parint.h'
      INCLUDE 'comint.h'
      INCLUDE 'proout.h'
      INCLUDE 'mscoef.h'
c state vectors,times, stepsize
      double precision y1(nvarx),y2(nvarx),t1,t2,h,h2
c  workspace per rkimp
      double precision ck(ismax,nvarx),ck1(ismax,nvarx),ck2(ismax,nvarx)
      double precision ep(itmax),dery(nvarx)
c  workspace for bdstep
      double precision dd(nvar2x,4),delta(nvar2x,m2max)
c  close approaching planet
      double precision xxpla(6)
c internal times
      double precision t0,tint,din
c controls
      double precision sdelto,epin
      integer nfl
c dimensions
      integer nvar,nvar2,ndim
c counters
      integer nstep,nrk,npas
c integers
      integer lflag, idc,j,j1,n,lf,it,lit,i,m
c type of equation 
      integer nclass
c arrays to store state transition matrix
      DOUBLE PRECISION stm(6,6),stmout(6,6),stm0(6,6),tcur
c name of right hand side routine
      external force
* **************************************
c static memory allocation
      save
c **********************************************************
c  controllo metodi propagazione:
c           imet=1 (multistep) =2 (runge-kutta) =3 (everhart)
c    runge-kutta:
c           isrk=ordine del runge-kutta (diviso 2)
c           h=passo di integrazione
c           eprk=controllo di convergenza nell'equaz. implicita del rk
c           lit1,lit2=iterazioni di gauss-seidel al primo passo e dopo
c           iusci=flag uscita controlli numerici
c    multistep:
c           mms=ordine del multistep ('m' nell'art. Cel.Mech)
c nnnn      ipc=iteraz correttore; epms=controllo
c nnnn      iusms=flag uscita controlli numerici
c    everhart:
c nnnn      isrk=ordine (per ora solo 15)
c           h=passo (fisso se ll.le.0)
c           ll=controllo 10**(-ll); se ll.gt.0, 
c              scelta automatica del passo
c           iusci=flag uscita contr. num.; 
c                  se iusci.ge.0, uscita passo cambiato
      data lflag/0 /
c *******************************************************
      if(icmet.eq.3)then
         if(abs(t1-t2).lt.deltos)then
c safety against divide zero in everhart (if times are equal)
            t2=t1
            do  i=1,nvar
              y2(i)=y1(i)
            enddo
            return
         else
c  everhart method (propagates to exact time):
             if(icrel.gt.0)then
                nclass=2
             else
                nclass=-2
             endif
c accumulate state transition matrix
             IF(nvar.gt.6)THEN 
                CALL vawrxv(y1,y1(nvar2+1),stm0,nvar2)
                CALL invaxv(y1,y1(nvar2+1),nvar2)
             ENDIF
 666         CONTINUE
             call ra15(y1,y1(nvar2+1),t1,t2,tcur,nvar2,nclass,idc)
             IF(tcur.eq.t2)THEN
c  current time and state is the final one;
                IF(nvar.gt.6)THEN 
c  but the accumulated state
c  transition matrix stm0 has to be used: stmout=stm*stm0
                   CALL vawrxv(y1,y1(nvar2+1),stm,nvar2)
                   CALL mulmat(stm,6,6,stm0,6,6,stmout)
                   CALL varunw(stmout,y1,y1(nvar2+1),nvar2)
                ENDIF
                do i=1,nvar
                   y2(i)=y1(i)
                enddo
                t1=t2
                return
             ELSE
c write message
                WRITE(*,*)'end close approach to planet',idc,tcur
c propagation has been interrupted because of a close approach
                CALL vawrxv(y1,y1(nvar2+1),stm,nvar2)
                CALL mulmat(stm,6,6,stm0,6,6,stmout)
                CALL mcopy(6,6,stmout,stm0)
                CALL invaxv(y1,y1(nvar2+1),nvar2)
                t1=tcur
                GOTO 666
             ENDIF
         endif
      endif
c **********************************************************
c set m (order of multistep method)
      m=mms
      if(lflag.eq.0)then
c  inizializzazione
c  contapassi:npas caricamento delta
c  nrk caricamento ck
         npas=0
         t0=t1
         nrk=0
c  dimensione del sec membro eq ordine 2
         nvar2=nvar/2
c  dimensione usata per controllo di convergenza (no variational eq.)
         ndim=3
c  inizializzazioni per il multistep
         j1=0
         h2=h*h
c  fine inizializzazioni
         lflag=1
      else
c  controllo se nuovo arco
         if(nfl.eq.0)then
            npas=0
            t0=t1
            nrk=0
c  inizializzazioni per il multistep
            j1=0
            h2=h*h
         endif
      endif
*******************************************************
c  controllo direzione del tempo
      if((t2-t1)*h.lt.0.d0) then
          write(*,999)t1,t2,h
 999      format('propin: from t1=',1p,f12.4,' to t2=',f12.4,
     $         ' with step h=',d12.4)
          write(*,*)'propin: this should not happen'
          STOP
c          h=-h
      endif
c **********************************************************
c  main loop:
c  controllo di funzione: propagatore o interpolatore
  5   if(dabs(t2-t1).lt.dabs(h)-deltos)goto 90
c
c  scelta propagatore
      if(npas.lt.m.or.icmet.ge.2)then
c **********************************************************
c  runge kutta implicito fa un solo passo
c
 24      continue
c  passo iniziale?
         if(nrk.le.0)then
c
c  passo iniziale: store secondo membro
            call force(y1,y1(nvar2+1),t1,delta(1,m+1),nvar2,idc,xxpla)
c close approach control
            CALL clocms(idc,t1,xxpla)
c  passo iniziale: inizializzazione di ck a zero
            do 11 j=1,isrk
            do 11 i=1,nvar
 11           ck(j,i)=0.d0
            lit=lit1
         else
c  passo non iniziale : interpolazione dei ck
            do 21 j=1,isrk
            do 21 n=1,nvar
 21           ck1(j,n)=ck(j,n)
            call kintrp(ck1,ck,isrk,nvar)
            lit=lit2
         endif
c  un passo del rk
 22      call rkimp(t1,h,y1,dery,ck,isrk,y2,lit,force,
     +            nvar,eprk,ep,lf,ndim)
c  controllo di avvenuta convergenza
         if(lf.le.0)then
c  caso di non convergenza
c           call camrk(ep,npas,nrk,lf)
            CALL rkstep(ep,npas,nrk,lf,h)
            h2=h*h
            if(lf.eq.2)goto 24
            goto 22
         endif
c  passo del rk adottato
         npas=npas+1
         nrk=nrk+1
         t1=t0+h*npas
         do 25 i=1,nvar
 25        y1(i)=y2(i)
         if(iusci.gt.0)then
            it=iabs(lf)
            if(npas.eq.iusci*(npas/iusci))write(ipirip,1000)npas,
     $                (ep(i),i=1,it)
 1000       format(' npas',i6,' ep ',5d12.3/(5d12.3))
         endif
         if(icmet.lt.2)then
c  preparazione per il multistep
            call force(y1,y1(nvar2+1),t1,delta(1,m+1-npas),nvar2,
     +            idc,xxpla)
c close approach control
            CALL clocms(idc,t1,xxpla)
c store in differences array
            if(npas.eq.m)call catst(m,m+1,b,a,nvar2,nvar2x,nvar,
     $                              delta,dd,y1,h,h2)
         endif
         goto 5
c ***************************************************************
c  propagatore multistep fa nstep passi, l'ultimo con la velocita'
      else
c  occorrono altri passi; quanti?
         sdelto=deltos*dabs(h)/h
         din=(t2-t1+sdelto)/h
         nstep=din
c
c  propagazione per nstep passi
c
c  versione predittore soltanto
c
 32      call bdnste(t1,y1,h,h2,nstep,m,j1,dd,delta,nvar2,nvar2x,
     $                 nvar,force)
c  passo del ms adottato
         npas=npas+nstep
         t1=t0+h*npas
      endif
c  nuovo passo
      goto 5
c ***************************************************************
c
c  interpolazione
 90   continue
c  controllo se occorre interpolare
      if(dabs(t2-t1).le.deltos)then
c  t2 e' circa t1 : non occorre interpolare
         t2=t1
         do 91 n=1,nvar
 91        y2(n)=y1(n)
      else
c  occorre interpolare
c  con il propagatore runge-kutta usato con passo variato
         do 92 j=1,isrk
         do 92 n=1,nvar
 92         ck2(j,n)=0.d0
         lit=lit1
         epin=eprk
         tint=t2-t1
 94      call rkimp(t1,tint,y1,dery,ck2,isrk,y2,lit,force,
     +   nvar,epin,ep,lf,ndim)
         if(lf.le.0)then
c  interpolatore impazzito
            it=iabs(lf)
            write(ipirip,1002)tint,epin,(ep(j),j=1,it)
 1002       format( ' interpolatore impazzito,passo h=',
     $      f10.7,' controllo =',d12.3/' ep= ',5 d12.3/(5d12.3/))
            epin=epin*10.d0
            goto 94
         endif
      endif
*************************************************
      return
      end


