c ========================================================
c RA15V
c Vers 2.1.1 last update April 29, 2000 A. Milani
c structured version; subroutines are in rasubs.f
c
c  integrator radau by e. everhart, physics department, university of denver
c  this 15th-order version, called ra15, is written out for faster execution.
c  y'=f(y,t) is  nclass=1,  y"=f(y,t) is nclass= -2,  y"=f(y',y,t) is nclass=2
c  tfin is t(final); tini is t(initial).
c  nv = the number of simultaneous differential equations.
c  the dimensioning below assumes nv will not be larger than 60.
c  ll controls sequence size. thus ss=10**(-ll) controls the size of a term.
c  a typical ll-value is in the range 6 to 12 for this order 15 program.
c  however, if ll.lt.0 then xl is the constant sequence size used.
c  x and v enter as the starting position-velocity vector, at time tini,
c  and are output as the final position-velocity vector at time tfin.
c =========================INTERFACE===========================
      SUBROUTINE ra15(x,v,tini,tfin,tcur,nv,nclass,idcend)
      IMPLICIT NONE
c =================INPUT============================
c initial and final time, current time at output 
c (in case propagation is interrupted)
      DOUBLE PRECISION tini,tfin,tcur
c dimension of state variables
      INTEGER nv
c error control
      INTEGER ll
c initial stepsize
      DOUBLE PRECISION xl
c equation type
      INTEGER nclass
c =================OUPUT============================
c state variables
      INCLUDE 'nvarx.h' 
      DOUBLE PRECISION x(nvar2x),v(nvar2x)
c close approach end flag
      INTEGER idcend
c =====================END INTERFACE===========================
c total number of function calls
      INTEGER nf
c state variables (WARNING: there is a fixed limit here!)
      INTEGER ndx,nvx
      PARAMETER (ndx=60,nvx=2*ndx)
c right hand side at begin/end of step
      DOUBLE PRECISION f1(nvx)
c storage arrays
      DOUBLE PRECISION b(7,nvx),g(7,nvx),e(7,nvx),bd(7,nvx)
c ===============CLOSE APPROACH CONTROL====================
c headers
      INCLUDE 'iclap.h'
      INCLUDE 'proout.h'
      INTEGER idc
c positon and velocities of planet which has close-encounter
c whith the asteroid
      DOUBLE PRECISION xpla(6)
      LOGICAL cloend
c logical flag for variational eq., state transition matrix accumulation
      LOGICAL variaz
c ========================================================
c constants of the integration method
      DOUBLE PRECISION h(8),w(7),u(7),c(21),d(21),r(21),w1
      COMMON/raconst/h,w,u,c,d,r,w1
c ========================================================
c logical control
      LOGICAL npq,nsf,nper,ncl,nes
c ========================================================
c time and stepsize
c initial stepsize with sign,time direction, final and current time from tini 
      DOUBLE PRECISION xldir,dir,tf,tm
c stepsize control: first guess, current stepsize, t or t**2
      DOUBLE PRECISION tp,t,t2
c error control, abs. val. stepsize, truncation error parameter 
      DOUBLE PRECISION ss,tval,hv
c error control: convergence
      INCLUDE 'parint.h'
      INCLUDE 'comint.h'
      DOUBLE PRECISION ep(itmax)
c intermediate times
      DOUBLE PRECISION s,q
c scalar temporaries
      DOUBLE PRECISION gk,temp
c loop indexes k=1,nv; l=1,7; j=2,8
      INTEGER k,l,j
c iteration count for implicit equations m=1,ni 
      INTEGER m
c iteration number control
      INTEGER ni
c count of steps done
      INTEGER ns
c count of step reductions
      INTEGER ncount
c ========================================================
c scalar variables
c constants
      DOUBLE PRECISION pw
c static memory allocation
      SAVE
c ===============to remove ORBFIT commons========================
c     INTEGER lit1,lit2,itmax,iusci,ipirip
c     DOUBLE PRECISION eprk
c     parameter (itmax=20)
c     lit1=10
c     lit2=4
c     ipirip=10
c     iusci=0
c     eprk=1.d-12
c     iclap=0
c ========================================================
c begin execution
c ========================================================
c setup of logical controls
c  y'=f(y,t)  ncl=.true.    y"=f(y,t)  ncl=.false.   y"=f(y',y,t) ncl=.false.
c  nclass=1   npq=.true.    nclass= -2 npq=.true.    nclass= 2    npq=.false.
      npq=nclass.lt.2
      ncl=nclass.eq.1
c set to zero storage arrays (for first order equations only)
      DO  k=1,nv
        IF(ncl) v(k)=0.d0
      ENDDO
c  nper is .true. only on last sequence of the integration.
      nper=.false.
c  nsf is .false. on starting sequence, otherwise .true.
      nsf=.false.
c  nes is .true. only if ll is negative. then the sequence size is xl.
      ll=llev
      xl=hev
      nes=ll.lt.0
c variaz is true if the state transition matrix is being propagated;
      variaz=nv.gt.3
c ===============================================================
c  evaluate the constants in the h-, w-, u-, c-, d-, and r-vectors
c ===============================================================
      CALL radcon(ncl)
      pw=1.d0/9.d0
c ==========================================================
c initialisation
c ==========================================================
c direction of time
      dir=1.d0
      tf=tfin-tini
      if(tf.lt.0.d0) dir=-1.d0
      xldir=dir*abs(xl)
      ss=10.**(-ll)
c ===============================================================
c  the statements above are used only once in an integration to set up the
c  constants.  the  next set in
c  a reasonable estimate to tp based on experience. same sign as dir.
c  an initial first sequence size can be set with xl even with ll positive.
c ===============================================================
      IF(xldir.ne.0.d0)then
        tp=xldir
      ELSE
         IF(nes)then
            write(99,*)' ra15v: fixed stepsize xl=0; ll=',ll
            stop
         ELSE
            tp=0.1d0*dir
         ENDIF
      ENDIF
      IF(tp/tf.gt.1.d0)then
         tp=tf
      ELSEIF(tp/tf.gt.0.5d0)then
         tp=0.5d0*tf
      ENDIF
      ncount=0
c ============================================================
c information on initial state of the integrator
      IF(iusci.gt.100)write(ipirip,999)tini,tfin,tp*dir
 999  format(' ra15: from tini=',f12.4,' to tfin=',f12.4,
     $   ' with max. step h=',1p,d12.4)
c ===============================================================
c  line 4000 is the starting place of the first sequence.
c ===============================================================
 4000 ns=0
      nf=0
c ===============================================================
c force initial value for first step
c ================================================================
      tm=0.d0
      CALL force(x, v, tm+tini, f1,nv,idc,xpla)
      nf=nf+1
c
 3000 CONTINUE
c set to zero storage arrays
c when extrapolation from previous step is not to be used
      do  k=1,nv
        do  l=1,7
          e(l,k)=0.d0
          b(l,k)=0.d0
          bd(l,k)=0.d0
        enddo
      enddo
c ===============================================================
c line 722  begins every step after the first.
 722  CONTINUE
c ===============================================================
c compute array g from b
      CALL rabeta(nv,b,d,g)
c ===============================================================
c set time variables
      t=tp
      t2=t*t
      IF(ncl) t2=t
      tval=abs(t)
c ===============================================================
c  loop 175 is lit1 iterations on first step and lit2 iterations therafter.
      IF(nsf)THEN
         ni=lit2
      ELSE
         ni=lit1
      ENDIF
      do 175 m=1,ni
c =========do on substep========================= 
        CALL rasust(m,t,t2,tm,tini,x,v,b,f1,nv,ncl,npq,g,ep(m),nf)
c =====================================================
c  iteration of step is over.
c =====================================================
c iteration control based on norm of differences
        IF(m.eq.1)goto 175
        IF(ep(m)/ep(1).lt.eprk)then
c prepare for stepsize determination of next step
           IF(.not.nes)then
c if variable stepsize, step size control
              hv=0.d0
              do 635 k=1,nv
 635            hv=dmax1(hv,abs(b(7,k)))
              hv=hv*w(7)/tval**7
           ENDIF
           IF(iusci.gt.1000)write(ipirip,996)ni,tini+tm+t,ep(1)
     +            ,(ep(j)/ep(1),j=2,m)
 996       format('ra15v: good convergence iter ',i3,' time=',f10.2,
     +    ' controls:'/(5d12.4/)) 
           goto 176
        ENDIF
c =========end do on iterations========================= 
 175  continue
c ======================================================
c bad convergence of iterations
      IF(iusci.gt.0)write(ipirip,997)ni,eprk,ep(1)
     +            ,(ep(j)/ep(1),j=2,ni)
 997  format('ra15v: bad convergence iter ',i3,' eprk=',d10.2,
     +    ' controls:'/(5d12.4/))   
      IF(nes)then
         write(99,*)tm,ep
         write(99,*)' ra15v: non convergence with fixed step ',t 
c         stop
      ELSE
         tp=.8d0*tp
         goto 3000
      ENDIF
c ======================================================
c satisfactory convergence; continue as in old version
 176  IF (.not.nsf)then
c ======================================================
c block executed only for first step
c ======================================================
         IF(nes)then
c fixed stepsize
            tp=xldir
         ELSE
c variable stepsize: compute initial stepsize
            tp=(ss/hv)**pw*dir
c not too quick increase 
            IF(tp/t.gt.1.4d0) tp=t*1.4d0
c not too quick decrease
            IF(tp/t.lt.1.d0)then
               tp=.8d0*tp
               ncount=ncount+1
               IF(ncount.gt.20) then
c too many shortenings of the stepsize
                  write(ipirip,*)' ra15v: ncount.gt.10',xl,tp
                  stop
               ENDIF
        IF(ncount.gt.1.and.iusci.gt.100)write(ipirip,888)ncount,t,tp
 888           format (2x,2i3,2d18.10)
c  restart with 0.8x sequence size if new size called for is smaller than
c  originally chosen starting sequence size on first sequence.
               go to 4000
            ELSE
c  initial step accepted
               IF(iusci.gt.100)write(ipirip,*)'ra15v: second step=',tp 
            ENDIF
         ENDIF
         nsf=.true.
c ======================================================
         IF(iusci.gt.100)write(ipirip,*)' tp ',tp
      ENDIF
c =====================================================
      CALL rapred(ncl,x,v,t,t2,f1,b,nv)
      DO k=1,nv
        IF(abs(x(k)).gt.1.d15)THEN
            write(99,*)' rapred ',k,x(k),v(k),f1(k)
        ENDIF
        IF(abs(v(k)).gt.1.d15)THEN
            write(99,*)' rapred ',k,x(k),v(k),f1(k)
        ENDIF
      ENDDO
c =====================================================
c current time update
      tm=tm+t
      ns=ns+1
c  return if done.
      IF(nper.or.abs(t-tf).lt.1.d-10)then
         tcur=tfin
         IF(iusci.gt.100)write(ipirip,*) 'nf,ns,tfin ',nf,ns,tfin
         return
      ENDIF
c  control on size of next sequence and adjust last sequence to exactly
c  cover the integration span. nper=.true. set on last sequence.
c ===============================================================
c force initial value for next step
c ================================================================
      idcend=idc
      CALL force(x,v,tm+tini,f1,nv,idc,xpla)
      nf=nf+1
c ================================================================
c close approach control
      IF(iclap.eq.1)THEN
         CALL cloapp(tm+tini,t,x,v,idc,xpla,xldir,dir,nes,cloend)
         IF(cloend.and.variaz)THEN
            tcur=tm+tini
            return
         ELSE
            idcend=0
         ENDIF
      ENDIF
c ================================================================
c check if the integration required is finished
      IF(nes)THEN
         tp=xldir
      ELSE
         tp=dir*(ss/hv)**pw
         IF(tp/t.gt.1.4d0) tp=t*1.4d0
c         write(99,*)' next step ',tp
         IF(iusci.gt.100)write(ipirip,*)'step=',tp
      ENDIF
      IF(dir*(tm+tp).gt.dir*tf-1.d-8)THEN
         tp=tf-tm
         nper=.true.
      ENDIF
c ================================================================
c  new step, no extrapolation, recompute right hand side
      IF(.not.nsf) GOTO 4000
c  if the integration continues without discontinuities, 
c  extrapolate b-values for next step.
      q=tp/t
      CALL bintrp(q,b,e,bd,nv,ns)
c  new step
c     IF(.not.nsf) GOTO 3000
      go to 722
      end







