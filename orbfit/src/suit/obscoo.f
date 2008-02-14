* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: February 24, 1997
*
*  ***************************************************************
*  *                                                             *
*  *                         O B S C O O                         *
*  *                                                             *
*  *          Body-fixed coordinates of an observatory           *
*  *                                                             *
*  ***************************************************************
*
*
* INPUT:    IDOBS     -  Identifier of the observatory (0-999)
*
* OUTPUT:   XBF(3)    -  Body-fixed position of the observatory (AU)
*           NAME      -  Name of the observatory
*
      subroutine obscoo(idobs,xbf,name)
      implicit none

      include 'trig.h'
      include 'obscoo.h'
      include 'proout.h'
      integer idobs
      double precision xbf(3)
      character*(*) name

      integer ns1,ns2
      parameter (ns1=0,ns2=1000)

      double precision aukm,eradkm,eradau
* Astronomical Unit in km
      parameter (aukm=149.59787066d6)
* Earth radius in km
      parameter (eradkm=6378.140d0)
* Earth radius in AU
      parameter (eradau=eradkm/aukm)

      double precision al1,pxy1,pz1,xbfv(3,ns1:ns2)
      integer unit,i,k
      character*(lnobnx) name1,namev(ns1:ns2)
      character*80 rec
      logical first,loaded(ns1:ns2)
      save first,loaded,xbfv,namev
      data first/.true./

      if(first)then
* Get parallax data from MPC
          do 1 i=ns1,ns2
 1        loaded(i)=.false.

          call filopl(unit,'OBSCODE.dat')

 2        continue
          read(unit,110,end=3)k,al1,pxy1,pz1,name1
          if(k.lt.ns1.or.k.gt.ns2)
     +        stop ' **** obscoo: internal error (01) ****'
          al1=al1*radeg
          xbfv(1,k)=eradau*pxy1*cos(al1)
          xbfv(2,k)=eradau*pxy1*sin(al1)
          xbfv(3,k)=eradau*pz1
          namev(k)=name1
          loaded(k)=.true.
          goto 2
 3        continue

          call filclo(unit,' ')

************************************
* Get parallax data for radar sites
          call filopl(unit,'RADCODE.dat')
 4        continue
             read(unit,'(a)',end=5)rec
             if(rec(1:1).eq.'!'.or.rec(1:1).eq.' ')goto 4
             read(rec,*)k,al1,pxy1,pz1,name1
             if(k.lt.ns1.or.k.gt.ns2)
     +            stop ' **** obscoo: internal error (11) ****'
             al1=al1*radeg
             xbfv(1,k)=1d-10*pxy1*cos(al1)
             xbfv(2,k)=1d-10*pxy1*sin(al1)
             xbfv(3,k)=1d-10*pz1
             namev(k)=name1
             loaded(k)=.true.
          goto 4
 5        call filclo(unit,' ')
************************************

          first=.false.
      end if
 110  format(i3,f10.5,f8.6,f9.5,a)

      if(idobs.lt.ns1.or.idobs.gt.ns2)THEN

         WRITE(0,*) ' **** obscoo: IDOBS outside allowed range ****'
         WRITE(0,*)' idobs=',idobs
         STOP
      endif
      if(loaded(idobs)) then
          do i=1,3
            xbf(i)=xbfv(i,idobs)
          enddo
          name=namev(idobs)
      else
          write(0,101)idobs
 101      format(' obscoo: observatory',i4,
     +       ' is not listed in file "OBSCODE.dat"')
c          stop ' **** obscoo: abnormal end ****'
          write(ierrou,101)idobs
          numerr=numerr+1
          do i=1,3
            xbf(i)=0.d0
          enddo
          name='UNKNOWN'
      end if

      end

