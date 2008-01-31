c Copyright 1998, Orbfit Consortium
c Modified Dec. 2 1998 by Steve Chesley to include sorting of selection flags.
c SRTOSS: remake of srtarc
c Ordinamento dei tempi di osservazione di un arco rispetto a t0
c INPUT:    t0       -  Tempo centrale dell'arco
c           iobs     -  Observation type
c           toss(j)  -  Tempi di osservazione (j=1,noss)
c           al(j)    -  Ascensioni rette (j=1,noss)
c           de(j)    -  Declinazioni (j=1,noss)
c           sel(i)   -  Selection Flags
c           w(j)     -  Pesi delle osservazioni (j=1,2*noss)
c           noss     -  Numero di osservazioni 
c OUTPUT: 
c         iobsrt   -  Obs. types sorted
c         tsort(k) -  Tempi ordinati (prima i tempi >t0, in ordine crescente,
c                       poi quelli <t0, in ordine decrescente) (k=1,ntot)
c         iposs(k) -  Mappa: I_{noss} --> I_{noss}
c           als(j) -  Ascensioni rette riordinate
c           des(j) -  Declinazioni riordinate
c           sels(i)-  Sorted Selection Flags
c           ws(j)  -  Pesi delle osservazioni riordinati
      subroutine srtoss(t0,iobs,toss,al,de,ioco,sel,w,noss,iobsrt,tsort,
     +     iposs,als,des,iocos,sels,ws)
      implicit none
      include 'parobx.h'
c observations number, counters
      integer noss,k,j,itt,ii
      double precision toss(noss),al(noss),de(noss),w(2*noss)
      double precision tsort(noss),tt,t0
      integer iobs(noss),iobsrt(noss)
      double precision als(noss),des(noss),ws(2*noss)
      integer sel(noss),sels(noss)
      integer  iposs(noss),ioco(noss),iocos(noss)
      logical change,order
c****************
c   static memory not required
c****************
c Inizializzazione di tsort
      k=0
      do 1 j=1,noss
      k=k+1
      tsort(k)=toss(j)
      iposs(k)=j
 1    continue
c Sort
 3    change=.false.
      do 2 k=1,noss-1
        if(.not.order(t0,tsort(k),tsort(k+1)))then
          change=.true.
          tt=tsort(k)
          tsort(k)=tsort(k+1)
          tsort(k+1)=tt
          itt=iposs(k)
          iposs(k)=iposs(k+1)
          iposs(k+1)=itt
        end if
 2    continue
      if(change)goto 3
c Reordering of right ascensions declinations and weigths
c (And Selection Flags)
      DO  ii=1,noss
         als(ii)=al(iposs(ii))
         des(ii)=de(iposs(ii))
         iocos(ii)=ioco(iposs(ii))
         sels(ii)=sel(iposs(ii))
         ws(2*ii-1)=w(2*iposs(ii)-1)
         ws(2*ii)=w(2*iposs(ii))
         iobsrt(ii)=iobs(iposs(ii))
      ENDDO
      return
      end
c==================================================
c================     UNSORT     ==================
c==================================================
c UNSORT: reordering of residuals in original order
c
c Input:   iposs
c          noss
c          nos2
c          csi
c          tsort
c          toss
c          sels
c
c Output:  csir
c          sel
c
c==================================================
      subroutine unsort(iposs,noss,nos2,csi,csir,sel,sels,tsort,toss)
      implicit none
      integer noss,nos2,iposs(noss),n,k
      double precision csi(nos2),csir(nos2)
      double precision tsort(noss),toss(noss),tmp
      integer sel(noss),sels(noss)
      do  n=1,noss
        k=iposs(n)
        tmp=toss(k)-tsort(n)
        if(tmp.ne.0.d0)then
           write(*,*)'sorting problem ',tmp,n,tsort(n),k,toss(k)
        endif
        sel(k)=sels(n)
        csir(2*k)=csi(2*n)
        csir(2*k-1)=csi(2*n-1)
      enddo
      return
      end
c==================================================
c================     ORDER      ==================
c==================================================
c ORDER:  Relazione d'ordine rispetto a t0
      logical function order(t0,t1,t2)
      implicit none
      double precision t0,t1,t2
c****************
c   static memory not required
c****************
      if(t1.ge.t0.and.t2.ge.t0)then
          order=(t2.ge.t1)
      else if(t1.ge.t0.and.t2.lt.t0)then
          order=.true.
      else if(t1.lt.t0.and.t2.ge.t0)then
          order=.false.
      else if(t1.lt.t0.and.t2.lt.t0)then
          order=(t2.le.t1)
      else
          stop' **** order: errore ****'
      end if
      return
      end


