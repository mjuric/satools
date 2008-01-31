c =====================================
c FALSI  regula falsi
c =====================================
      SUBROUTINE falsi(t,xa,va,xpla,jc,jt,first,iplam)
      IMPLICIT NONE
      DOUBLE PRECISION t,xa(3),va(3),xpla(6)
      INTEGER jc,jt
c =====================================
c fixed stepsize time interval
      DOUBLE PRECISION tt1,tt2
c data for regula falsi
      DOUBLE PRECISION r1,r2,r0,rdot1,rdot2,rdot0,t1,t2,t0
      DOUBLE PRECISION z1,z2,z0,zdot1,zdot2,zdot0
      DOUBLE PRECISION vsize,prscal
      DOUBLE PRECISION x(3),v(3),xt(3),vt(3),xat(3),vat(3),xplat(6)
      DOUBLE PRECISION dt,tt,di,hh
      INTEGER it,i,iplam
      INTEGER itmax
      PARAMETER (itmax=50)
      LOGICAL first
      INCLUDE 'closapl.h'
      INCLUDE 'parbep.h'
      INCLUDE 'masses.h'
      INCLUDE 'proout.h'
      SAVE
c planetocentric position
      DO i=1,3
        x(i)=xa(i)-xpla(i)
        v(i)=va(i)-xpla(i+3)
      ENDDO
c initialisation at the beginning of each close approach
      IF(first)THEN
         tt1=t
         r1=vsize(x)
         rdot1=prscal(x,v)/r1    
C        WRITE(*,*) ' close approach to planet ',ordnam(iplam)
C        WRITE(*,*) ' beginning t,r,rdot=',tt1,r1,rdot1
C        WRITE(iuncla,*) ' close approach to planet ',ordnam(iplam)
C        WRITE(iuncla,*) ' beginning t,r,rdot=',tt1,r1,rdot1
         IF(mtpon.and.iplam.eq.ipla0)THEN
            z1=prscal(tpnor,x)
            zdot1=prscal(tpnor,v)
c           WRITE(*,*) ' z,zdot=',z1,zdot1
c           WRITE(iuncla,*) ' z,zdot=',z1,zdot1
         ENDIF
         RETURN
      ENDIF
c compute r, rdot
      r2=vsize(x)
      rdot2=prscal(x,v)/r2
      tt2=t
c     write(*,*)tt2,r2,rdot2
      IF(tt2.gt.tt1)THEN
         di=1.d0
      ELSEIF(tt2.lt.tt1)THEN
         di=-1.d0
      ELSE
         write(*,*)' falsi: zero step'
         write(*,*)tt1,tt2,x,v,xpla
      ENDIF
c     write(*,*)rdot1,rdot2
      IF(abs(rdot2).lt.eprdot)THEN
         CALL strclo(ordnam(iplam),t,x,v,jc,r2,rdot2)
         rdot2=0.d0
      ELSEIF(rdot1*di.lt.0.d0.and.rdot2*di.gt.0.d0)THEN
         t1=tt1
         t2=tt2
*        WRITE(*,*) ' r. f. begins, t,r,rdot=',t1,r1,rdot1,t2,r2,rdot2
*        WRITE(iuncla,*) ' r. f. begins ',t1,r1,rdot1,t2,r2,rdot2
         dt=-rdot2*(t1-t2)/(rdot1-rdot2)
         tt=dt+t2
         hh=tt-t
c iterate regula falsi
         DO it=1,itmax
           CALL rkg(t,xa,va,hh,xat,vat,xplat)
c planetocentric position
           DO i=1,3
             xt(i)=xat(i)-xplat(i)
             vt(i)=vat(i)-xplat(i+3)
           ENDDO
           r0=vsize(xt)
           rdot0=prscal(xt,vt)/r0
           t0=tt
           IF(abs(rdot0*dt).lt.eprdot)THEN
c           IF(abs(rdot0).lt.eprdot)THEN
              CALL strclo(ordnam(iplam),tt,xt,vt,jc,r0,rdot0)
              GOTO 2
           ELSEIF(rdot0*rdot1.lt.0.d0)THEN
              r2=r0
              rdot2=rdot0
              t2=t0
           ELSEIF(rdot0*rdot2.lt.0.d0)THEN
              r1=r0
              rdot1=rdot0
              t1=t0 
           ENDIF
           dt=-rdot2*(t1-t2)/(rdot1-rdot2)
           tt=dt+t2
           hh=tt-t
C         WRITE(*,111)t1,r1,rdot1,t2,r2,rdot2,dt,tt
 111     format(8(1x,f16.9))
         ENDDO
c failed convergence
         CALL strclo(ordnam(iplam),t0,xt,vt,jc,r0,rdot0)
         WRITE(ierrou,*)' falsi: failed convergence for ',ordnam(iplam)
         WRITE(ierrou,*)'t1,r1,rdot1,t2,r2,rdot2,dt,tt'
         WRITE(ierrou,111)t1,r1,rdot1,t2,r2,rdot2,dt,tt
         numerr=numerr+1
      ENDIF
c found zero
 2    CONTINUE
c other regula falsi for cross section
      IF(mtpon.and.iplam.eq.ipla0)THEN
         z2=prscal(tpnor,x)
         zdot2=prscal(tpnor,v)
c        write(*,*)z1,zdot1,z2,zdot2
         IF(abs(z2).lt.epsmtp)THEN
            CALL strmtp(t,x,v,jt,z2,zdot2)
            z2=0.d0
         ELSEIF(z1*di.lt.0.d0.and.z2*di.gt.0.d0)THEN
            t1=tt1
            t2=tt2
c      WRITE(*,*) ' r. f. begins, t,z,zdot=',t1,z1,zdot1,t2,z2,zdot2
c      WRITE(iuncla,*) ' r. f. mtp begins ',t1,z1,zdot1,t2,z2,zdot2
            dt=-z2*(tt1-tt2)/(z1-z2)
            tt=dt+t2
            hh=tt-t
c begin iterations
            DO it=1,itmax
              CALL rkg(t,xa,va,hh,xat,vat,xplat)
c planetocentric position
              DO i=1,3
                xt(i)=xat(i)-xplat(i)
                vt(i)=vat(i)-xplat(i+3)
              ENDDO
              z0=prscal(tpnor,xt)
              zdot0=prscal(tpnor,vt)
              t0=tt
              IF(abs(z0).lt.epsmtp)THEN
                 CALL strmtp(tt,xt,vt,jt,z0,zdot0)
                 GOTO 3
              ELSEIF(z0*z1.lt.0.d0)THEN
                 z2=z0
                 zdot2=zdot0
                 t2=t0
              ELSEIF(z0*z2.lt.0.d0)THEN
                 z1=z0
                 zdot1=zdot0
                 t1=t0 
              ENDIF
              dt=-z2*(t1-t2)/(z1-z2)
              tt=dt+t2
              hh=tt-t
c           write(*,*)t1,z1,zdot1
c           write(*,*)t2,z2,zdot2,dt,tt,di,it
            ENDDO
c failed convergence
            WRITE(*,*)' falsi: failed mtp convergence'
            WRITE(*,*)t1,z1,zdot1
            WRITE(*,*)t2,z2,zdot2,dt,tt,di,itmax
         ENDIF
      ENDIF
 3    CONTINUE
c save current state to be previous state next time
      tt1=tt2
      r1=r2
      rdot1=rdot2
      IF(mtpon.and.iplam.eq.ipla0)THEN
         z1=z2
         zdot1=zdot2
      ENDIF
      RETURN
      END
c ===================================================
c STRMTP
c store mod. target plane intersection 
c ===================================================
      SUBROUTINE strmtp(tcur,x,v,jt,z,zdot)
      IMPLICIT NONE
      INTEGER jt   
      DOUBLE PRECISION tcur,x(3),v(3),z,zdot
      INTEGER i
c common data on close appr. 
      INCLUDE 'cloapp.h'
      INCLUDE 'proout.h'
c store close approach point
      jt=jt+1
      IF(jt.gt.njtx)THEN
          WRITE(*,*)' strmtp: jt>njtx ',jt,njtx
          STOP
      ENDIF
      ttar(jt)=tcur
      DO i=1,3
        xtp(i,jt)=x(i)
        vtp(i,jt)=v(i)
      ENDDO
c     WRITE(*,*) ' target plane at time, distance, zdot'
c     WRITE(*,*) tcur, z, zdot
      WRITE(iuncla,*) ' target plane at time, distance, rdot'
      WRITE(iuncla,*) tcur, z, zdot
      WRITE(iuncla,*) ' relative position and velocity'
      WRITE(iuncla,*) x,v
      RETURN
      END
c ===================================================
c STRCLO
c store close approach 
c ===================================================
      SUBROUTINE strclo(planam,tcur,x,v,jc,r,rdot)
      IMPLICIT NONE
      INTEGER jc   
      DOUBLE PRECISION tcur,x(3),v(3),r,rdot
      DOUBLE PRECISION vl,vsize
      INTEGER i
c planet name
      CHARACTER*30 planam
      INTEGER lpla,lench
c calendar date variables
      INTEGER iyear,imonth,iday
      DOUBLE PRECISION hour
      CHARACTER*16 date
c common data on close appr. 
      INCLUDE 'cloapp.h'
      INCLUDE 'proout.h'
c store close approach point
      jc=jc+1
      IF(jc.gt.njcx)THEN
          WRITE(*,*)' strclo: jc>njcx ',jc,njcx
          STOP
      ENDIF
      tcla(jc)=tcur
      rmin(jc)=r
      DO i=1,3
        xcla(i,jc)=x(i)
        vcla(i,jc)=v(i)
      ENDDO
c     WRITE(*,*) ' closest approach at time, distance, rdot'
c     WRITE(iuncla,*) ' closest approach at time, distance, rdot'
      lpla=lench(planam)
      numcla=numcla+1
      call mjddat(tcur,iday,imonth,iyear,hour)
!      write(date,'(i4,a1,i2.2,a1,i2.2,f6.5)')
!     +              iyear,'/',imonth,'/',iday,hour/24d0
!      WRITE(iuncla,100) planam(1:lpla),date,tcur,r,rdot,x,v
 100  FORMAT(a,1x,a16,f12.5,1x,f11.8,e11.3,1x,6(1x,f11.8))
!      WRITE(*,97)planam(1:lpla),date,tcur,r
 97   FORMAT(' Close approach to ',a,' on ',a16,f12.5,' MJD at ',
     +     f10.8,' AU.')
c     WRITE(iuncla,*) ' relative position and velocity'
c     WRITE(iuncla,*) x,v
c store normal to MTP
      vl=vsize(vcla(1,jc))
      DO i=1,3
        vmtp(i,jc)=vcla(i,jc)/vl
      ENDDO
      RETURN
      END
c =================================================
c RKG
c  Runge-Kutta-Gauss, used as a pure single step
c =================================================
      SUBROUTINE rkg(t1,xa,va,h,xat,vat,xplat)
      IMPLICIT NONE
      DOUBLE PRECISION xa(3),va(3),xat(3),vat(3),xplat(6)
      DOUBLE PRECISION t1,h
      DOUBLE PRECISION y1(6),dery(6),yat(6),de
      INCLUDE 'parint.h'
      INCLUDE 'comint.h'
      INCLUDE 'rkcoef.h'
      INTEGER nvar
      PARAMETER (nvar=6)
      DOUBLE PRECISION ep(itmax),ck(ismax,nvar),t(ismax)
      INTEGER i,j,it,jj
c control of derivatives
      INTEGER ide,ideold
      COMMON/deriv/ide
      ideold=ide
      ide=0
c state vector before the step
      DO i=1,3
        y1(i)=xa(i)
        y1(i+3)=va(i)
      ENDDO
c set intermediate times; array ck initialized to zero
      DO j=1,isrk
        t(j)=t1+h*c(j)
        DO i=1,nvar
          ck(j,i)=0.d0
        ENDDO
      ENDDO
c controls on convergence inititalized to zero
      DO  it=1,itmax
        ep(it)=0.d0
      ENDDO
c  gauss-seidel iterations for array ck
      it=1
c  main loop on intermediate points
 1    DO 11 j=1,isrk
        DO 12 i=1,nvar
          de=0.d0
          DO  jj=1,isrk
            de=de+a(j,jj)*ck(jj,i)
          ENDDO
          yat(i)=de*h+y1(i)
 12     continue  
        CALL fctcl(t(j),yat,dery,nvar,xplat)
        DO i=1,nvar
          ep(it)=ep(it)+dabs(dery(i)-ck(j,i))
        ENDDO
        DO i=1,nvar
          ck(j,i)=dery(i)
        ENDDO
 11   continue
c  control on end of gauss-seidel iterations
      ep(it)=ep(it)/isrk
      IF(it.gt.1.and.ep(it).gt.ep(it-1)*1.1d0)THEN
         WRITE(*,*)' rkg: stop at iteration ',it, ' before too late'
         WRITE(*,*)t1,(ep(jj),jj=it-1,it)
         GOTO 77
      ENDIF
c  new state vector y3
      DO i=1,3
        de=0.d0
        DO  j=1,isrk
          de=de+b(j)*ck(j,i)
          yat(i)=y1(i)+h*de
        ENDDO
      ENDDO
      IF(ep(it).gt.eprk)THEN
         IF(it.ge.lit1)THEN
c  too many gauss-seidel iterations
            WRITE(*,*)' rkg: non convergent after ',it,' iterations'
            WRITE(*,*)t1,ep(it)
         ENDIF
         it=it+1
         GOTO 1
      ENDIF
c
c right hand side at new state
 77   CALL fctcl(t1+h,yat,dery,nvar,xplat)
c copy pos. vel.
      DO i=1,3
        xat(i)=yat(i)
        vat(i)=yat(i+3)
      ENDDO
c control of derivatives reset
      ide=ideold
      RETURN
      END
c ==========================================================
c   FCTCL
c reduction to first order, to use with RKG
c ==========================================================
c   subroutine secondo membro
c   equazioni ridotte all'ordine 1
c   a partire dall'eq del secondo ordine
c   calcolata da force
      subroutine fctcl(t,y,dery,nvar,xxpla)
      implicit none
      integer nvar,nvar2,idc,i
      double precision y(nvar),dery(nvar)
      double precision xxpla(6)
      double precision t
c****************
c   static memory not required
c****************
      nvar2=nvar/2
      call force(y,y(nvar2+1),t,dery(nvar2+1),nvar2,idc,xxpla)
      do  i=1,nvar2
        dery(i)=y(nvar2+i)
      enddo
      return
      end







