c=================================================
c INTSTEP integration step for propagation along LOV
      SUBROUTINE intstep(eq0,eq1,hh,imint,
     +       mc,wc,sel,iobs,tauc,ioco,tc,
     +           alc,dec,iun,wdir,sdir,fail)
      IMPLICIT NONE
c stepsize, integration method
      DOUBLE PRECISION hh
      INTEGER imint
c ======observations====
c number of obs., time, station code, alpha, delta
      INTEGER mc,ioco(mc)
      DOUBLE PRECISION tauc(mc),alc(mc),dec(mc)
c rms of alpha, delta
c     DOUBLE PRECISION rmsa(mc),rmsd(mc)
c selection flags, obs type
      INTEGER sel(mc),iobs(mc)
c weighing to be computed
c weighing to be computed
      INCLUDE 'parobx.h'
      DOUBLE PRECISION wc(nob2x)
c initial conditions: epoch, elements, new elements after step
      DOUBLE PRECISION tc,eq0(6),eq1(6)
c normal and covariance matrices, norms of residuals, computed at eq0
c     DOUBLE PRECISION gc(6,6),csinoc
c output unit
      INTEGER iun
c failure flag
      LOGICAl fail
c =========end interface================
c intermediate point: state, covariance, normal matr., RMS, norm corr
      DOUBLE PRECISION eq12(6),g12(6,6),c12(6,6),csino12,delno12
      INCLUDE 'comdif.h'
c weak direction (in input computed at eq0), length of axis
      DOUBLE PRECISION wdir(6),sdir,wdir12(6),sdir12,wdirst(6),sdirst
c chi**2 rms for each obs,residuals, control, flag (for difcor)
      DOUBLE PRECISION x2(nobx),csi(nob2x),delcr
      INTEGER inew, icor(6),ncor,itmaxold
c success control is dummy,bizarre is impor
      LOGICAL succ,bizarre
c loop indexes
      INTEGER j
c controls of fixed point iteration
      INTEGER itx,it
      DOUBLE PRECISION eps,cosa,prscag,ang,dsi,e1,e12,direc
c ================================================
      IF(imint.eq.1)THEN
c Euler method
        DO  j=1,6
           eq1(j)=eq0(j)+wdir(j)*sdir*hh
        ENDDO
        fail=.false.
      ELSEIF(imint.eq.2)THEN
c Runge-Kutta-Gauss of order 2:
        itx=5
        eps=1.d-4
c first store vectorfield as it is on eq0
        CALL vcopy(6,wdir,wdirst)
        sdirst=sdir
c compute intermediate point (first guess)        
        DO j=1,6
           eq12(j)=eq0(j)+wdir(j)*sdir*hh*0.5d0
        ENDDO 
c       write(*,*)'eq12, iter.0 ',(eq12(j),j=1,3)
c before running differential corrections, tests that orbit is still elliptic
        e12=sqrt(eq12(2)**2+eq12(3)**2)
        IF(e12.gt.0.99d0.or.eq12(1).le.0.d0)THEN
           fail=.true.
           call vcopy(6,eq12,eq1)
           RETURN
        ELSEIF(bizarre(eq12))THEN
           fail=.true.
           call vcopy(6,eq12,eq1)
           RETURN
        ENDIF
c fixed point iteration
        DO it=1,itx
c compute vectorfield at intermediate point
           itmaxold=itmax
           itmax=0
           CALL whicor(0,icor,ncor,inew)
c compute covariance and residual norm
c          write(*,*)mc,iobs(1),ioco(1)
           CALL difcor(mc,wc,sel,tc,iobs,tauc,ioco,eq12,alc,dec,
     +     icor,inew,iun,delcr,
     +     eq12,g12,c12,csino12,delno12,csi,x2,succ)
           itmax=itmaxold
c compute weak direction and length
           CALL weakdi(g12,wdir12,sdir12,iun)
           direc=prscag(6,wdir12,wdir)
           IF(direc.lt.0.d0)THEN
              DO j=1,6
                 wdir12(j)=-wdir12(j)
              ENDDO
           ENDIF
c recompute intermediate point
           DO j=1,6
              eq12(j)=eq0(j)+wdir12(j)*sdir12*hh*0.5d0
           ENDDO 
c          write(*,*)'weak sigma, iter, eq12',sdir12,it,(eq12(j),j=1,3)
c before a new iteration, tests that orbit is still elliptic
           e12=sqrt(eq12(2)**2+eq12(3)**2)
           IF(e12.gt.0.99d0.or.eq12(1).le.0.d0)THEN
              fail=.true.
              call vcopy(6,eq12,eq1)
c               write(*,*)'intstep: fail at iteration ',it,fail
              RETURN
           ELSEIF(bizarre(eq12))THEN
              fail=.true.
              call vcopy(6,eq12,eq1)
              RETURN
           ENDIF
c convergence control
           cosa=prscag(6,wdir12,wdirst)
           IF(cosa.le.1.d0)THEN
              ang=acos(cosa)

           ELSE
              ang=0.d0
           ENDIF
           dsi=abs(sdirst-sdir12)/sdirst
c          WRITE(*,*)' intstep: it, ang, dsi, sdir12,sdirst ',
c    +          it,ang,dsi,sdir12,sdirst
           IF(ang.lt.eps.and.dsi.lt.eps)THEN
c             WRITE(*,*)' intstep: it, ang, dsir ',it,ang,dsi
c convergence achieved
              GOTO 2
           ENDIF
           CALL vcopy(6,wdir12,wdirst)
           sdirst=sdir12           
        ENDDO
c convergence failed
c       WRITE(*,*)' intstep: failed convergence, it, ang, dsig'
c       WRITE(*,*)it-1,ang,(sdir12-sdirst)/sdirst
        fail=.true.
        call vcopy(6,eq12,eq1)
        RETURN
c convergence succeeded    
  2     CONTINUE
c compute final point         
        DO j=1,6
           eq1(j)=eq0(j)+wdir12(j)*sdir12*hh
        ENDDO 
c before a new iteration, tests that orbit is still elliptic
        e1=sqrt(eq1(2)**2+eq1(3)**2)
        IF(e1.gt.0.99d0.or.eq1(1).le.0.d0)THEN
           fail=.true.
        ELSEIF(bizarre(eq1))THEN
           fail=.true.
        ELSE
           fail=.false.
        ENDIF
c but we go on
      ELSE
         WRITE(*,*)' intstep: not invented yet, imint=',imint
         STOP
      ENDIF
c     write(*,*)'intstep: at exit ',fail, (eq1(j),j=1,3)
      RETURN
      END


