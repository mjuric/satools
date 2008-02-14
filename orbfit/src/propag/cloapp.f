c ==============================================================
c CLOAPP 
c close approach control; driver for falsi
c vers. 1.9.2, A.Milani, August 25, 1999
c with decreasing stepsize near closest approach
c also with end of close approach flag
c ==============================================================
      SUBROUTINE cloapp(tcur,th,xa,va,idc,xpla,xldir,dir,nes,cloend)
      IMPLICIT NONE
c close approach control: which planet is close (0=none)
      INTEGER idc
c stepsize control: flag for fixed step, stepsize, direction
      LOGICAL nes
      DOUBLE PRECISION xldir,dir
c current time, previous stepsize
      DOUBLE PRECISION tcur,th
c positon, velocity of asteroid, of close encounter planet
      DOUBLE PRECISION xa(3),va(3)
      DOUBLE PRECISION xpla(6)
c logical flag for close approach termination
c to allow for storage of state transition matrix
      LOGICAL cloend
c =========END INTERFACE========================================
c stepsize tricks
      LOGICAL nesold
      DOUBLE PRECISION xlold,tmin,tleft
c which planet is being approached, vector difference
      INTEGER ic
      DOUBLE PRECISION x(3),v(3)
c
      DOUBLE PRECISION vsize
      INCLUDE 'model.h'
      INCLUDE 'parbep.h'
      INCLUDE 'masses.h'
      INCLUDE 'proout.h'
c common data on close appr. 
      INCLUDE 'closapl.h'
      INCLUDE 'cloapp.h'
c counters for arrays of multiple minima, sections
      INTEGER jc,jt
c loop indexes i=1,3
      INTEGER i
c logical flags for regulae falsi being initiaited
      LOGICAL first
c startup from a non-close approaching state at each integration restart
      INCLUDE 'closta.h'
c static memory model required
      SAVE 
c ===========================================
      IF(clost)THEN
         ic=0
         clost=.false.
      ENDIF
c close approach is not ending, in most cases
      cloend=.false.
c ========================================================
c   control on close approaches
      IF(ic.eq.0)THEN
c close approach not going on; check if it is beginning
         IF(idc.ne.0)THEN
c close approach detected (first step inside influence region)
c relative position and velocity
            DO  i=1,3
              x(i)=xa(i)-xpla(i)
              v(i)=va(i)-xpla(i+3)
            ENDDO
c planet with which close approach is taking place
            ic=idc
            iplam=idc
c           WRITE(iuncla,*)' approach to planet', iplam
c close approach minima counter
            jc=0
c target plane crossing counter
            jt=0
c first call to falsi
            first=.true.
            CALL falsi(tcur,xa,va,xpla,jc,jt,first,iplam)
c setup of fixed stepsize
            nesold=nes
            nes=.true.
            xlold=xldir
c  stepsize based upon relative velocity
            tmin=2*dmin(idc)/vsize(v)
            xldir=dir*tmin/npoint
            IF(abs(xldir).gt.abs(th)/2.d0)xldir=dir*abs(th)/2.d0
            tleft=vsize(x)/(1.5d0*vsize(v))
            IF(abs(xldir).gt.tleft)xldir=dir*tleft
c           WRITE(iuncla,*)' closap: initial step=',xldir
         ENDIF
      ELSE
c close approach taking place
c    control of inconsistences
         IF(idc.ne.0)THEN
            IF(idc.ne.ic.or.idc.gt.nmass.or.idc.lt.0)THEN
               WRITE(0,*)' closap: this should not happen',idc,ic
            ENDIF
c relative position and velocity
            DO  i=1,3
              x(i)=xa(i)-xpla(i)
              v(i)=va(i)-xpla(i+3)
            ENDDO
            tleft=vsize(x)/vsize(v)
            IF(abs(xldir).gt.tleft)THEN
               xldir=dir*tleft
c              WRITE(iuncla,*)' closap: reduced step=',xldir
            ENDIF
            first=.false.
            CALL falsi(tcur,xa,va,xpla,jc,jt,first,iplam)
         ELSE
c   close approach ended; reset flags etc.
            cloend=.true.
            xldir=xlold
            nes=nesold
            ic=0
            njc=jc
            njt=jt
            jc=0
            jt=0
c           write(iuncla,*)' closap7: approach ended to planet ',iplam
c           write(iuncla,*)
!           write(0,*)
         ENDIF
      ENDIF
c =================================================
      RETURN
      END






