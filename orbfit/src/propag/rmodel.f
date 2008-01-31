* Copyright (C) 1998 by OrbFit Consortium
* Version: December 14, 1998 Mario Carpino
c ================================================================
c RMODEL
c ================================================================
c this subroutine reads from file 'propag.def' all the propagator options;
c the only input argument is the string "run", used to open 
c output files with appropriate names
c  WARNING: we advise the user against changing the file propag.def;
c                do it at your risk !....
c COMMON STORED:
c    model.h : for dinamical model, numerical integrators parameters
c                and controls.
c input options for phisical model! version 1.2 of propagator
c       ilun=0!  0=no moon  1= yes 
c       imerc=1! 0=no mercury 1=yes (recommended =1 for asteroids)
c       iplut=0! 0=no pluto 1=yes (recommended =0 for asteroids, =1 for 
c                                   transneptunian)
c       irel=0!  0=newtonian 1=gen. relativity (recommended =0 for main belt,
c                                   =1 for Earth crossing)
c       iast=0!  0=no asteroids with mass n=no. of massive asteroids 
c       filbe='CPV'! name of the asteroid ephemerides file (written by BINEPH)
c       iclap=1! 0=no close approach control 1=yes (recommended =1)
c       iaber=1! aberration 0=no 1=yes (recommended =1 always)
c       istat=1! 0=no topocentric corr 1= yes (recommended =1 always)
c
c       iclap = 1 means for Radau alg. to calculate time pos. and vel.
c                at closest approach; for multistep alg. the propagator just
c                detects the time of close-appr.
c      iclap =0 the subroutine force does not check for possible close
c                approaches to major planets andor massive asteroids. 
c       iyark=1 ! Yarkovski force 
c       iyarpt=0! partials of yarkovski are generally not computed
c
c   cloapp.h: npoint = minimal number of close-approach records
c              ndeg  = degree of polimomial interpolation 
c                        (ndeg < 2*npoint-1);
c   The minimal distance to define a close-approch is controlled by:
c              dmea (for terrestrial planets), dmoon (for the moon),
c              dmjup (for giant planets), dmast (for massive asteroids). 
c ================================================================
c ========INTERFACE============================
      subroutine rmodel(run)
      implicit none
      character*80 run
c ========END INTERFACE========================
      include 'model.h'
      include 'iclap.h'
      include 'bifina.h'
      include 'proout.h'
      include 'parbep.h'
      include 'combep.h'
      include 'selast.h'
      INCLUDE 'restart.h'
      INCLUDE 'closapl.h'
      INCLUDE 'yarkov.h'
      INCLUDE 'yarkom.h'
      character*80 filbe
c     character*80 file
      logical fail,fail1,found
      integer ll,ia
c controls for bizarre orbits
      DOUBLE PRECISION ecclim, samin,samax,phmin,ahmax
c****************
c   static memory not required (used only once)
c****************
* read time-range
      call trange
c restart flag set to default value
      restar=.true.
* read options
* force model flags
      fail=.false.
      call rdnint('propag.','ilun',ilun,.true.,found,fail1,fail)
      call rdnint('propag.','imerc',imerc,.true.,found,fail1,fail)
      call rdnint('propag.','iplut',iplut,.true.,found,fail1,fail)
      call rdnint('propag.','irel',irel,.true.,found,fail1,fail)
      call rdnint('propag.','iast',iast,.true.,found,fail1,fail)
      call rdncha('propag.','filbe',filbe,.true.,found,fail1,fail)
      call rmsp(filbe,ll)
      if(ll.le.0)STOP '**name of asteroid ephem file is wrong**'
      filbep=filbe(1:ll)//'.bep'
      filbec=filbe(1:ll)//'.bai'
      DO ia=1,iast
        astid(ia)=ia
      ENDDO
      iatrue=iast
c close approach control
      call rdnint('propag.','iclap',iclap,.true.,found,fail1,fail)
      iorb=11
      call rdnint('propag.','iaber',iaber,.true.,found,fail1,fail)
      call rdnint('propag.','istat',istat,.true.,found,fail1,fail)
      mtpon=.false.
      eprdot=1.d-10
c non gravitational perturbations
      call rdnint('propag.','iyark',iyark,.true.,found,fail1,fail)
      call rdnint('propag.','iyarpt',iyarpt,.true.,found,fail1,fail)
      call rdncha('propag.','yardir',yardir,.true.,found,fail1,fail)
      yarfil=.false.
      yarini=.false.
* numerical integrator options
      call rmsp(run,ll)
      call inipro
*
      if(iclap.ne.0) then
* close approach control if requeststed
        call rdnint('propag.','npoint',npoint,.true.,found,fail1,fail)
        call rdnrea('propag.','dmea',dmea,.true.,found,fail1,fail)
        call rdnrea('propag.','dmoon',dmoon,.true.,found,fail1,fail)
        call rdnrea('propag.','dmjup',dmjup,.true.,found,fail1,fail)
        call rdnrea('propag.','dmast',dmast,.true.,found,fail1,fail)
      endif
* Options for difcor (including outlier rejection)
      call difini
      CALL rejini
* Options for stopping difcor at bizarre orbits; use default
      ecclim=0.d0
      samin=0.d0
      samax=0.d0
      phmin=0.0d0
      ahmax=0.d0
      CALL bizset(ecclim,samin,samax,phmin,ahmax)
      if(fail) stop '**** rmodel: abnormal end ****'
      return
      end

