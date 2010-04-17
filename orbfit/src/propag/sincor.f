* Copyright (C) 1998 by OrbFit Consortium
* Version: December 11, 1997 Steven Chesley
c Single iteration of differential correction of orbital elements
c
c Input: m      observations number
c        iobsrt sorted obs. type 1000's=astrometry, 2000's=radar
c        tsort  sorted observation times
c        als    sorted right ascensions
c        des    sorted declination
c        iocos  sorted observatory codes
c        t0     epoch time for asteroid elements
c        icor(6) flags .ne.0 to correct this element, 0 to leave it as in eq0 
c        inew =2 for pseudo-Newton (use this), =1 for Newton (not well tested)
c        ws     sorted weight vector
c        iun    unit for output
c        matonly = .true. if only need covariance; do not update orbit
c
c Output eq     corrected orbital elements at time t0
c        delnor norm of the corrections
c        csi    residuals
c        csinor norm of residuals
c        gamma covariance matrix
c        gtwg inverse of covariance matrix
c           warning: if only some elements are corrected, only the 
c              corresponding entries in gamma and gtwg are nonzero
c
      SUBROUTINE sincor(m,iobsrt,tsort,als,des,iocos,t0,icor,inew,ws,
     +              iun,matonly,eq,delnor,csi,csinor,gamma,gtwg)
      IMPLICIT NONE

      INCLUDE 'parobx.h'
c elongation,distance to Earth, distance to Sun (to compute magnitude)
      include 'phase.h'
c magnitude data
      include 'mag.h'

      INTEGER m,iocos(m),inew,iun,iobsrt(m)
      DOUBLE PRECISION tsort(m),eq(6),t0,csi(2*m),csinor,ws(2*m)
      DOUBLE PRECISION gtwg(6,6),gamma(6,6),delnor,als(m),des(m)
      LOGICAL matonly

      INTEGER j,iocj,ider,i,icor(6),k,no
      INTEGER nused
      DOUBLE PRECISION tauj
      DOUBLE PRECISION alj,dej,cond,deq(6)
c first and second derivatives of alpha, delta w.r. to elements
      DOUBLE PRECISION dade(6),ddde(6),ddade(6,6),dddde(6,6)
c control of two body approximation (must be false)
      LOGICAL twobo
c second derivatives of observations
      DOUBLE PRECISION h(nob2x,6,6)
c DOUBLE PRECISION functions
      DOUBLE PRECISION snormd,snorm,appmag,pridif

      INCLUDE 'restart.h'
      INCLUDE 'codode.h'

c assign ider depending from inew
      IF(inew.eq.2)THEN
        ider=1
      ELSEIF(inew.eq.1)THEN
        ider=2
      ELSE
        write(99,*)'sincor: inew not correct',inew,icor
        STOP
      ENDIF
      twobo=.false.

c Compute observations and derivatives
      restar=.true.

      DO 61 j=1,m
          tauj=tsort(j)
          iocj=iocos(j)
          IF(iobsrt(j)/1000.eq.1)THEN
             CALL alfdel(eq,t0,tauj,iocj,alj,dej,dade,ddde,ider,twobo,
     +                ddade,dddde)
c  compute magnitude difference (apparent minus absolute)
             dmagns(j)=appmag(0.d0,gmagc,dsun,dis,pha)
             phaas(j)=pha
             dsunas(j)=dsun
             disas(j)=dis
             adots(j)=adot
             ddots(j)=ddot
          ELSEIF(iobsrt(j)/1000.eq.2)THEN
             CALL rrdot(eq,iobsrt(j),t0,tauj,iocj,alj,dej,dade,ddde,
     +             ider,twobo)
          ELSE
             write(99,*)'sincor: iobs= ',iobsrt(j), ' not known'
             STOP
          ENDIF
          restar=.false.
c Compute residuals, form matrix g
          IF(iobsrt(j)/1000.eq.1)THEN
             csi(2*j-1)=pridif(als(j),alj)
             csi(2*j)=pridif(des(j),dej)
          ELSE
             csi(2*j-1)=als(j)-alj
             csi(2*j)=des(j)-dej
          ENDIF
          DO 62 i=1,6
              g(2*j-1,i)=dade(i)
              g(2*j,i)=ddde(i)
              IF(inew.eq.1)THEN
                  DO k=1,6
                      h(2*j-1,i,k)=ddade(i,k)
                      h(2*j,i,k)=dddde(i,k)
                  ENDDO
              END IF
 62       continue
 61   continue
      restar=.true.
c Compute solution of linear least squares
      no=2*m
      CALL minsol(csi,no,ws,g,h,inew,icor,iun,gtwg,deq,gamma,cond)
c Norm of the residuals
      csinor=snormd(csi,ws,no,nused)
c Norm of the corrections
      delnor=snorm(deq,gtwg,6,6)
c Update solution (only solve-for variables) 
      IF(.not.matonly)THEN
          DO k=1,6
              IF(icor(k).ne.0) eq(k)=eq(k)+deq(k)
          ENDDO
      END IF

      END
