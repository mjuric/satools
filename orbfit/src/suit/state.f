c++++++++++++++++++++++++++++++++
c
      subroutine state(et2,list,pv,pnut,istate)
c
c++++++++++++++++++++++++++++++++
c
c this subroutine reads and interpolates the jpl planetary ephemeris file
c
c     calling sequence parameters:
c
c     input:
c
c         et2   dp 2-word julian ephemeris epoch at which interpolation
c               is wanted.  any combination of et2(1)+et2(2) which falls
c               within the time span on the file is a permissible epoch.
c
c                a. for ease in programming, the user may put the
c                   entire epoch in et2(1) and set et2(2)=0.
c
c                b. for maximum interpolation accuracy, set et2(1) =
c                   the most recent midnight at or before interpolation
c                   epoch and set et2(2) = fractional part of a day
c                   elapsed between et2(1) and epoch.
c
c                c. as an alternative, it may prove convenient to set
c                   et2(1) = some fixed epoch, such as start of integration,
c                   and et2(2) = elapsed interval between then and epoch.
c
c        list   12-word integer array specifying what interpolation
c               is wanted for each of the bodies on the file.
c
c                         list(i)=0, no interpolation for body i
c                                =1, position only
c                                =2, position and velocity
c
c               the designation of the astronomical bodies by i is:
c
c                         i = 1: mercury
c                           = 2: venus
c                           = 3: earth-moon barycenter
c                           = 4: mars
c                           = 5: jupiter
c                           = 6: saturn
c                           = 7: uranus
c                           = 8: neptune
c                           = 9: pluto
c                           =10: geocentric moon
c                           =11: nutations in longitude and obliquity
c                           =12: lunar librations (if on file)
c
c
c     output:
c
c          pv   dp 6 x 11 (note: 6 x 12)
c               array that will contain requested interpolated
c               quantities.  the body specified by list(i) will have its
c               state in the array starting at pv(1,i).  (on any given
c               call, only those words in 'pv' which are affected by the
c               first 10 'list' entries (and by list(12) if librations are
c               on the file) are set.  the rest of the 'pv' array
c               is untouched.)  the order of components starting in
c               pv(1,i) is: x,y,z,dx,dy,dz.
c
c               all output vectors are referenced to the earth mean
c               equator and equinox of epoch. the moon state is always
c               geocentric; the other nine states are either heliocentric
c               or solar-system barycentric, depending on the setting of
c               common flags (see below).
c
c               lunar librations, if on file, are put into pv(k,11) if
c               list(12) is 1 or 2.
c
c         nut   dp 4-word array that will contain nutations and rates,
c               depending on the setting of list(11).  the order of
c               quantities in nut is:
c
c                        d psi  (nutation in longitude)
c                        d epsilon (nutation in obliquity)
c                        d psi dot
c                        d epsilon dot
c
c           *   statement # for error return, in case of epoch out of
c               range or i/o errors.
c
c
c     common area stcomx:
c
c          km   logical flag defining physical units of the output
c               states. km = .true., km and km/sec
c                          = .false., au and au/day
c               default value = .false.  (km determines time unit
c               for nutations and librations.  angle unit is always radians.)
c
c        bary   logical flag defining output center.
c               only the 9 planets are affected.
c                        bary = .true. =\ center is solar-system barycenter
c                             = .false. =\ center is sun
c               default value = .false.
c
c       pvsun   dp 6-word array containing the barycentric position and
c               velocity of the sun.
c
c
c change by A.Milani and Z. Knezevic, March 10, 1998, for compatibility with 
c Lahey compiler
      IMPLICIT NONE
      DOUBLE PRECISION  et2(2),pv(6,12),pnut(4),t(2),pjd(4),buf(1500)
      integer list(12)
      logical first
c
      character*6 ttl(14,3),cnam(400)
      common/chrhdr/cnam,ttl
c
      character*150 namfil
c
      DOUBLE PRECISION  pvsun(6)
      logical km,bary
      common/stcomx/km,bary,pvsun
c variable declared for implicit none
      INTEGER istate
      INTEGER nrecl, ksize, nrfile, irecsz, ncoeffs, numde, nrl
      INTEGER i, j, nr, k
      DOUBLE PRECISION s, aufac
c
      include 'jplhdr.h'
      save
      data first/.true./
c end change 1998
c
c       entry point - 1st time in, get pointer data, etc., from eph file
c
      if(first) then
        first=.false.

c ************************************************************************
c ************************************************************************

c the user must select one of the following by deleting the 'c' in column 1

c ************************************************************************

c        call fszer1(nrecl,ksize,nrfile,namfil)
         call fszer2(nrecl,ksize,nrfile,namfil)
c        call fszer3(nrecl,ksize,nrfile,namfil)

      if(nrecl .eq. 0) write(*,*)'  ***** fszer is not working *****'

c ************************************************************************
c ************************************************************************

      irecsz=nrecl*ksize
      ncoeffs=ksize/2

        open(nrfile,
     *       file=namfil,
     *       access='direct',
     *       form='unformatted',
     *       recl=irecsz,
     *       status='old',
     *       err=10)

      read(nrfile,rec=1)ttl,cnam,ss,ncon,au,emrat,
     . ((ipt(i,j),i=1,3),j=1,12),numde,lpt

      read(nrfile,rec=2)cval

      do i=1,3
      ipt(i,13)=lpt(i)
      enddo
      nrl=0

      endif
c
c       ********** main entry point **********
c
      if(et2(1) .eq. 0.d0) return
c
      s=et2(1)-.5d0
      call split(s,pjd(1))
      call split(et2(2),pjd(3))
      pjd(1)=pjd(1)+pjd(3)+.5d0
      pjd(2)=pjd(2)+pjd(4)
      call split(pjd(2),pjd(3))
      pjd(1)=pjd(1)+pjd(3)
c
c       error return for epoch out of range
c
      if(pjd(1)+pjd(4).lt.ss(1) .or. pjd(1)+pjd(4).gt.ss(2)) go to 98
c
c       calculate record # and relative time in interval
c
      nr=idint((pjd(1)-ss(1))/ss(3))+3
      if(pjd(1).eq.ss(2)) nr=nr-1
      t(1)=((pjd(1)-(dble(nr-3)*ss(3)+ss(1)))+pjd(4))/ss(3)
c
c       read correct record if not in core
c
      if(nr.ne.nrl) then
        nrl=nr
        read(nrfile,rec=nr,err=99)(buf(k),k=1,ncoeffs)
      endif

      if(km) then
      t(2)=ss(3)*86400.d0
      aufac=1.d0
      else
      t(2)=ss(3)
      aufac=1.d0/au
      endif
c
c   interpolate ssbary sun
* ****** changed on Sat Jun 14 1997 ******
*      call interp(buf(ipt(1,11)),t,ipt(2,11),3,ipt(3,11),2,pvsun)
      call interp(buf(ipt(1,11)),t,ipt(2,11),3,ipt(3,11),istate,pvsun)
* **************************************

      do i=1,6
      pvsun(i)=pvsun(i)*aufac
      enddo
c
c   check and interpolate whichever bodies are requested
c
      do 4 i=1,10
        if(list(i).eq.0) go to 4
* ****** changed on Sat Jun 14 1997 ******
*      call interp(buf(ipt(1,i)),t,ipt(2,i),3,ipt(3,i),
*     & list(i),pv(1,i))
        call interp(buf(ipt(1,i)),t,ipt(2,i),3,ipt(3,i),
     & istate,pv(1,i))
* **************************************
        do j=1,6
          if(i.le.9 .and. .not.bary) then
             pv(j,i)=pv(j,i)*aufac-pvsun(j)
          else
             pv(j,i)=pv(j,i)*aufac
          endif
        enddo
c
   4  continue
c
c       do nutations if requested (and if on file)
c
      if(list(11).gt.0 .and. ipt(2,12).gt.0)
     * call interp(buf(ipt(1,12)),t,ipt(2,12),2,ipt(3,12),
     * list(11),pnut)
c
c       get librations if requested (and if on file)
c
      if(list(12).gt.0 .and. ipt(2,13).gt.0)
     * call interp(buf(ipt(1,13)),t,ipt(2,13),3,ipt(3,13),
     * list(12),pv(1,11))
c
      return
c
  98  write(*,198)et2(1)+et2(2),ss(1),ss(2)
 198  format(' ***  requested jed,',f12.2,
     * ' not within ephemeris limits,',2f12.2,'  ***')
c
      return
c
   99 write(*,'(2f12.2,a80)')
     & et2,'error return in state'
c
      stop
c
 10   continue
      STOP '**** state: error opening JPL DE file ****'
c
      end
