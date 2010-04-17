c Copyright 1998 Orbfit Consortium
c Version December 18, 1998 Steven Chesley (chesley@dm.unipi.it)
c ====================================================
c FSTEPH Ephemerides Generation for FITOBS
c This is a hacked version of FSTPRO
c First integrate from t0 back to tr, saving at interval step.
c Then write this data in chronological order
c Finally integrate from to forward to tf, writing after each step
c inputs:
c     name - asteroid name (for file and ephem listing)
c     dir - location for files
c     defele - logical for existence of elements
c     ok - success flag
c     coord - coordinate type for input elements(EQU,CAR,KEP)
c     t0 - current epoch
c     eq0 - input elements
c     tr - initial time for ephemerides
c     tf - final time
c     step - stepsize
c     numsav - an upper bound for the number of steps necessary before t0
c     ephefl - logical flag for output of ephemrides file
c     eltype - coordinate type for ephemerides (output) elements (EQU,CAR,KEP)
c     moidfl - logical flag for output of file of MOID's and nodal distances
c output: NONE
c     
c REMARK: IF moidfl=.false. and ephefl=.false. you will still 
c     get a close approach file.
c ====================================================
      SUBROUTINE fsteph(name,dir,defele,ok,coord,t0,eq0,
     +     tr,tf,step,numsav,ephefl,eltype,moidfl)
      IMPLICIT NONE
c =================INPUT=========================================
c name, place, output element type
      CHARACTER*(*) name,dir
c availability of initial conditions, covariance, all required
      LOGICAL defele,ok,moidfl,ephefl
c necessary size of temporary storage array
      INTEGER numsav
c epoch times, elements
      DOUBLE PRECISION t0,tr,tf,step,eq0(6)
      CHARACTER*(3) coord,eltype
c ================OUTPUT=================================
c none
c ================END INTERFACE==========================
c loop indexes
      INTEGER i,n
c converted elements
      DOUBLE PRECISION elem0(6),enne,eqtmp(6)
c temporary storage
      INTEGER numsavx
      PARAMETER (numsavx=10000)
      DOUBLE PRECISION tsav(numsavx),eqsav(6,numsavx),t1,elem1(6)
c output
      INTEGER unit
      INTEGER ln,lnm,lnnam
      CHARACTER*(60) file,filem
c ======== time spans for JPL data etc. =========
      INCLUDE 'timespan.h'
      INCLUDE 'sysdep.h'
      INCLUDE 'restart.h'
      INCLUDE 'sunmass.h'
c moid
      double precision moid,dnp,dnm
      double precision msav(numsavx),ndsav(numsavx,2)
      integer munit,iconv,icsav(numsavx)
c =====================================================================
c check dimensions
      IF(numsav.gt.numsavx)THEN
         write(99,*)'fsteph: numsav=',numsav,' is > numsavx=', numsavx
         RETURN
      ENDIF
c check availability of required data
      ok=.true.
      IF (.not.defele)THEN
         write(99,*) 'Sorry: You must provide an orbit first.'
         ok = .false.
      ENDIF
      IF (tr .ge. tf)THEN
         write(99,*) 'Sorry: Initial time must be before final time.'
         ok = .false.
      ENDIF
      IF(.not.ok)RETURN
c =====================================================================
c check availability of JPL ephemrides
      call chetim(tr,tf,ok)
      if(.not.ok)return
c========= OPEN FILES =================================
      if(ephefl)then
c open ephemerides ouput file
         call filnam(dir,name,'ele',file,ln)
         call filopn(unit,file(1:ln),'unknown')
         call wro1lh(unit,'ECLM','J2000',eltype)
      endif
      if(moidfl)then
c open moid file
         call filnam(dir,name,'moid',filem,lnm)
         call filopn(munit,filem(1:lnm),'unknown')
      endif
      call rmsp(name,lnnam)
c========= CONVERT INPUT ELEMENTS TO REQUESTED TYPE ===============
      call coocha(eq0,coord,gms,elem0,eltype,enne)
c========= PROPAGATE, SAVE, AND THEN WRITE IN TIME ORDER ===============
c     step back in time first (if necessary)
      if(tr .le. t0)then
         t1=t0
         do i=1,6
            elem1(i)=elem0(i)
         enddo
c        but first propagate back to tf if t0 isn't within interval
         if(tf .lt. t0)then
            t1=tf
            call proele(eltype,t0,elem0,t1,elem1)
         endif
         if(moidfl)then
            call coocha(elem1,eltype,gms,eqtmp,'EQU',enne)
            call nomoid(t1,eqtmp,moid,iconv,dnp,dnm)
         endif
         n=1
         tsav(n)=t1
         msav(n)=moid
         icsav(n)=iconv
         ndsav(n,1)=dnp
         ndsav(n,2)=dnm
         do i=1,6
            eqsav(i,n)=elem1(i)
         enddo
c        start loop
         do t1=t1,tr+step,-step
            call proele(eltype,t0,elem0,t1-step,elem1)
            restar=.false.
            if(moidfl)then
               call coocha(elem1,eltype,gms,eqtmp,'EQU',enne)
               call nomoid(t1-step,eqtmp,moid,iconv,dnp,dnm)
            endif
            n=n+1
            tsav(n)=t1-step
            msav(n)=moid
            ndsav(n,1)=dnp
            ndsav(n,2)=dnm
            icsav(n)=iconv
            do i=1,6
               eqsav(i,n)=elem1(i)
            enddo
         enddo
c        Get last point
         if(tsav(n).gt.tr)then
            call proele(eltype,t0,elem0,tr,elem1)
            if(moidfl)then
               call coocha(elem1,eltype,gms,eqtmp,'EQU',enne)
               call nomoid(tr,eqtmp,moid,iconv,dnp,dnm)
            endif
            n=n+1
            tsav(n)=tr
            msav(n)=moid
            ndsav(n,1)=dnp
            ndsav(n,2)=dnm
            icsav(n)=iconv
            do i=1,6
               eqsav(i,n)=elem1(i)
            enddo
         endif
         restar=.true.
c        write in forward time order
         do i=n,1,-1
            if(ephefl) call wro1lr(unit,name(1:lnnam),eqsav(1,i),
     +           eltype,tsav(i),-1.d4,1.0d0)
            if(moidfl) write(munit,199)tsav(i),msav(i),
     +           ndsav(i,1),ndsav(i,2)
 199        format(f13.4,1x,f8.5,1x,f8.5,1x,f8.5)
         enddo
      endif
c========= PROPAGATE AND WRITE SIMULTANEOUSLY ========================
      if(tf .ge. t0)then
         t1=t0
         do i=1,6
            elem1(i)=elem0(i)
         enddo
c        now step to the beginning (if necessary)
         if(tr .gt. t0)then
            t1=tr
            call proele(eltype,t0,elem0,t1,elem1)
            if(ephefl) call wro1lr(unit,name(1:lnnam),elem1,eltype,
     +           t1,-1.d4,1.0d0)
            if(moidfl)then
               call coocha(elem1,eltype,gms,eqtmp,'EQU',enne)
               call nomoid(t1,eqtmp,moid,iconv,dnp,dnm)
               write(munit,199)t1,moid,dnp,dnm
            endif
         endif
         do t1=t1,tf-step,step
            call proele(eltype,t0,elem0,t1+step,elem1)
            restar=.false.
            if(ephefl) call wro1lr(unit,name(1:lnnam),elem1,eltype,
     +           t1+step,-1.d4,1.0d0)
            if(moidfl)then
               call coocha(elem1,eltype,gms,eqtmp,'EQU',enne)
               call nomoid(t1+step,eqtmp,moid,iconv,dnp,dnm)
               write(munit,199)t1+step,moid,dnp,dnm
            endif
         enddo
c        Get last point
         if(t1.lt.tf)then
            call proele(eltype,t0,elem0,tf,elem1)
            if(ephefl) call wro1lr(unit,name(1:lnnam),elem1,eltype,
     +           tf,-1.d4,1.0d0)
            if(moidfl)then
               call coocha(elem1,eltype,gms,eqtmp,'EQU',enne)
               call nomoid(tf,eqtmp,moid,iconv,dnp,dnm)
               write(munit,199)tf,moid,dnp,dnm
            endif
         endif
         restar=.true.
      endif
c =====================================================================
      if(ephefl) call filclo(unit,' ')
      if(moidfl) call filclo(munit,' ')
      if(ephefl) write(99,*)'Ephemeris file generated:',file(1:ln)
      if(moidfl) write(99,*)'Moid file generated:',filem(1:lnm)

      RETURN
      END
