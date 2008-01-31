* version 1.8.3 A. Milani Feb 1999
* *** SELMET ***
* Choice of numerical integration method
*
* This routine is called from propag only if  imet=0
* in 'namerun'.top.
*            INPUT: equinoctal elements of asteroid
*            OUTPUT : icmet etc. stored in model.h
      subroutine selmet(eq)
      implicit none
      include 'model.h'
      include 'iclap.h'
      include 'comint.h'
      include 'proout.h'
* asteroid elements, eccentricity, perielion, aphelion
      double precision eq(6),ecc,q,qg
      integer iord,iork
* controls to define a main belt asteroid
      double precision qmin,qgmax,eccmax 
**********************************
c ==========modification 28/10/2000========
c trick to save the ilun of the option file, 
c being free to change it for the current case
      integer lflag,ilunold
*  static memory only for:
      save qmin,qgmax,eccmax,ilunold,lflag
      data lflag /0/
c ========================================
**********************************
* selection of minimum perihelion: all NEO 
      qmin=1.3d0
* selection of maximum aphelion: almost Jupiter crossing
      qgmax=4.3d0
* selection of maximum eccentricity for multistep
      eccmax=0.25d0
* current values of the elements
      ecc=sqrt(eq(2)**2+eq(3)**2)
      q=eq(1)*(1.d0-ecc)
      qg=eq(1)*(1.d0+ecc)
c =========================28/10/2000
c relativity flag set to the choice done in the option file
      icrel=irel
c lunar flag set to  the choice done in the option fi
      if(lflag.eq.0)then
         ilunold=ilun
         lflag=1
      endif
      ilun=ilunold
c ==========================end modif.
c asteroids
      if(eq(1).lt.5.d0)then
c pluto is irrelevant anyway
         iplut=0
c mercury is required
         imerc=1
* main belt case
         if(q.gt.qmin.and.qg.lt.qgmax.and.ecc.lt.eccmax)then
c physical model
c            icrel=irel
c numerical integration method
            icmet=1
            iord=8
            mms=iord-2
            iork=8
            isrk=iork/2
            epms= 1.0d-12
            lit1= 10
            lit2= 4
c non mainbelt: NEA, Mars crosser, high eccentricity, Jupiter crossing
         else
c physical model
            if(q.lt.qmin.and.irel.lt.1)then
               icrel=1
            endif
c ==================28/10/00
            if(q.lt.qmin.and.ilun.eq.0)then
               ilunold=ilun
               ilun=1
            endif
c ================end modif.
c numerical integration method
            icmet=3
            llev=12
         endif
c EKO
      elseif(qg.gt.32.d0.and.ecc.lt.eccmax)then
c physical model
         iplut=1
         icrel=0
         imerc=0
c numerical integration method
         icmet=1
         iord=8
         mms=iord-2
         iork=8
         isrk=iork/2
         epms= 1.0d-13
         lit1= 10
         lit2= 4
c Trojans
      elseif(q.gt.4.d0.and.qg.lt.7.d0)then
c physical model
         iplut=0
         icrel=0
         imerc=0
c numerical integration method
         icmet=1
         iord=8
         mms=iord-2
         iork=8
         isrk=iork/2
         epms= 1.0d-12
         lit1= 10
         lit2= 4
c centaurs
      else
         icmet=3
         icrel=0
         iplut=1
c         llev=9
      endif
*****************************************
*  automatic selection of ilun????
*****************************************
c write option selected
      IF(iusci.gt.0)THEN
         write(ipirip,*) 'a,e= ',eq(1),ecc,' icmet=',icmet
         write(ipirip,*) 'hms,epms,eprk,deltos,error, mms,isrk',
     +        'lit1,lit2,iusci,icha,llev,hev'
         write(ipirip,*) hms,epms,eprk,deltos,error, mms,isrk
     +        ,lit1,lit2,iusci,icha,llev,hev
         write(ipirip,*)'Force: ilun,imerc,iplut,irel,iast,iaber,istat'
         write(ipirip,*) ilun,imerc,iplut,irel,iast,iaber,istat
         write(ipirip,*) 'Close-approaches param; iclap=',iclap
         if(iclap.ne.0)then
            write(ipirip,*) 'dmea,dmoon,dmjup,dmast', 
     +      dmea,dmoon,dmjup,dmast
         endif
      ENDIF
      return
      end



