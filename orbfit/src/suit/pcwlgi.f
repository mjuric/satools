* Copyright (C) 1999 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 11, 1999
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         P C W L G I                           *
*  *                                                               *
*  *      Piece-wise interpolation using Chebychev polynomials     *
*  *                                                               *
*  *****************************************************************
*
* ARGUMENTS:
*    X(i,k)  (IN)   -  (i=1,N; k=1,ND) Dependent variable
*    T(i)    (IN)   -  (i=1,N) Independent variable
*    N       (IN)   -  Number of data points
*    ND      (IN)   -  Actual dimension of dependent variable
*    NX      (IN)   -  First dimension of X and T arrays
*
*    TI      (IN)   -  Interpolation time
*    XI(k)   (OUT)  -  Interpolated data: value
*    XID(k)  (OUT)  -  Interpolated data: first derivative
*    XIDD(k) (OUT)  -  Interpolated data: second derivative
*
*    NL      (IN)   -  Length of interpolation interval
*    NV      (IN)   -  Length of unused part of interpolation
*    NS      (IN)   -  Length of superposition
*    NSMORD  (IN)   -  Order of smoothness for SMOOCN
*    NTDER   (IN)   -  Requested order of time derivatives
*    START   (IN)   -  Startup flag (must be a variable)
*    RMSX(k) (IN)   -  Max allowed interpolation RMS
*
*    COEF(0:NGX,NIX,ND)  (WORK)  -  Polynomial coefficients
*    TLIM(2,NIX)         (WORK)  -  Limits of interpolation intervals
*    VOID(NIX)           (WORK)  -  (logical) interpolation flag
*    NGX     (IN)   -  Max polynomial degree (dim of COEF)
*    NIX     (IN)   -  Max number of intervals (dim of COEF,VOID)
*    IRET    (OUT)  -  Return code:
*                        0   = OK
*                        1-4 = time is out of limits
*
      SUBROUTINE pcwlgi(x,t,n,nd,nx,
     +                  ti,xi,xid,xidd,
     +                  nl,nv,ns,nsmord,ntder,start,rmsx,
     +                  coef,tlim,void,ngx,nix,iret)
      IMPLICIT NONE

      INTEGER n,nd,nx,nl,nv,ns,nsmord,ntder,ngx,nix,iret
      DOUBLE PRECISION x(nx,nd),t(nx)
      DOUBLE PRECISION ti,xi(nd),xid(nd),xidd(nd),rmsx(nd)
      DOUBLE PRECISION coef(0:ngx,nix,nd),tlim(2,nix)
      LOGICAL start,void(nix)

      INCLUDE 'parlgi.h'

      INTEGER ndx
      PARAMETER (ndx=10)

      INTEGER nu,na,ni,i,k,kint,kkn,kip,ksip,kofp,kis,ksis,kofs
      DOUBLE PRECISION dtmed,t1,t2,tin,ft,ft2,sigma,ss,ssd,ssdd
      DOUBLE PRECISION ts1,ts2,fc,c1,c1d,c1dd,c2,c2d,c2dd
      DOUBLE PRECISION tn(lgin1x)
      DOUBLE PRECISION pol(0:lgintx),pold(0:lgintx),poldd(0:lgintx)
      DOUBLE PRECISION xi1(ndx),xi1d(ndx),xi1dd(ndx)
      DOUBLE PRECISION xi2(ndx),xi2d(ndx),xi2dd(ndx)
      LOGICAL super

      IF(ntder.LT.0 .OR. ntder.GT.2) STOP '**** pcwlgi: ntder = ? ****'

      nu=nl-2*(nv+ns)
      na=ns+nu
      ni=(n-nl)/na
      dtmed=(t(n)-t(1))/(n-1)

      IF(start) THEN
          IF(nu.LT.0) STOP '**** pcwlgi: nu < 0  ****'
          IF(ni.GT.nix) STOP '**** pcwlgi: ni > nix  ****'
          DO 12 i=1,ni
          void(i)=.true.
 12       CONTINUE
          IF(nl.GT.ngx) STOP '**** pcwlgi: nl > ngx  ****'
          IF(nl.GT.lgintx) STOP '**** pcwlgi: nl > lgintx  ****'
          IF(nd.GT.ndx) STOP '**** pcwlgi: nd > ndx  ****'
          start=.false.
      END IF

      IF(ti.LT.t(1).OR.ti.GT.t(n)) THEN
          iret=1
          RETURN
      END IF

* KINT = number of the interval t(kint) <= ti < t(kint+1)
      kint=(ti-t(1))/dtmed+1
 1    CONTINUE
      IF(ti.LT.t(kint)) THEN
          kint=kint-1
      ELSEIF(ti.GE.t(kint+1)) THEN
          kint=kint+1
      ELSE
          GOTO 2
      END IF
      IF(kint.LT.1.OR.kint.GT.n-1) THEN
          iret=2
          RETURN
      END IF
      GOTO 1
 2    CONTINUE

* KIP = number of "primary" interpolation
      kkn=kint-nv-1
      kip=kkn/(ns+nu)+1
      IF(kkn.LT.0) kip=kip-1
      IF(kip.LT.1.OR.kip.GT.ni) THEN
          iret=3
          RETURN
      END IF

* KSIP = starting interval of primary interpolation
      ksip=(ns+nu)*(kip-1)+1
      IF(ksip.LT.1.OR.ksip.GT.n-nl)
     +    STOP '**** pcwlgi: internal error (01) ****'

* KOFP = offset with respect to starting interval (interval number)
      kofp=kint-ksip+1
      IF(kofp.LE.nv) STOP '**** pcwlgi: internal error (02) ****'
      IF(kofp.GT.nv+ns+nu) STOP '**** pcwlgi: internal error (03) ****'

* SUPER = superposition (with previous interpolation)
      super=(kofp.LE.nv+ns)

* Primary interpolation
      IF(void(kip)) THEN
* Normalization of time axis
          t1=t(ksip)
          t2=t(ksip+nl)
          tlim(1,kip)=t1
          tlim(2,kip)=t2
          ft=2/(t2-t1)
          DO 3 i=1,nl+1
          tn(i)=ft*(t(i+ksip-1)-t1)-1
 3        CONTINUE
          DO 4 i=1,nd
          CALL lgnint(x(ksip,i),tn,nl+1,nl,coef(0,kip,i),sigma)
          IF(sigma.GT.rmsx(i)) STOP '**** pcwlgi: rms > rmsx ****'
 4        CONTINUE
          void(kip)=.false.
      END IF
      t1=tlim(1,kip)
      t2=tlim(2,kip)
      ft=2/(t2-t1)
      ft2=ft**2
      tin=ft*(ti-t1)-1
      CALL chebyd(tin,nl,ntder,pol,pold,poldd)
      DO 6 i=1,nd
      ss  =0.d0
      ssd =0.d0
      ssdd=0.d0
      DO 5 k=0,nl
      ss  =ss  +coef(k,kip,i)*pol(k)
      ssd =ssd +coef(k,kip,i)*pold(k)
      ssdd=ssdd+coef(k,kip,i)*poldd(k)
 5    CONTINUE
      xi1(i)  =ss
      xi1d(i) =ssd*ft
      xi1dd(i)=ssdd*ft2
 6    CONTINUE

* Auxiliary interpolation (which is before the primary)
      IF(super) THEN
          kis=kip-1
* KSIS = starting interval of auxiliary interpolation
          ksis=(ns+nu)*(kis-1)+1
          IF(kis.LE.0) THEN
              iret=4
              RETURN
          END IF
          IF(ksis.LT.1 .OR. ksis.GT.n-nl)
     +           STOP '**** pcwlgi: internal error (04) ****'
* KOFS = offset with respect to starting interval (interval number)
          kofs=kint-ksis+1
          IF(kofs.LE.nv+ns+nu)
     +        STOP '**** pcwlgi: internal error (05) ****'
          IF(kofs.GT.nv+2*ns+nu)
     +        STOP '**** pcwlgi: internal error (06) ****'
          IF(void(kis)) THEN
              t1=t(ksis)
              t2=t(ksis+nl)
              tlim(1,kis)=t1
              tlim(2,kis)=t2
              ft=2/(t2-t1)
              DO 13 i=1,nl+1
              tn(i)=ft*(t(i+ksis-1)-t1)-1
 13           CONTINUE
              DO 14 i=1,nd
              CALL lgnint(x(ksis,i),tn,nl+1,nl,coef(0,kis,i),sigma)
 14           CONTINUE
              void(kis)=.false.
          END IF
          t1=tlim(1,kis)
          t2=tlim(2,kis)
          ft=2/(t2-t1)
          ft2=ft**2
          tin=ft*(ti-t1)-1
          CALL chebyd(tin,nl,ntder,pol,pold,poldd)
          DO 16 i=1,nd
          ss  =0.d0
          ssd =0.d0
          ssdd=0.d0
          DO 15 k=0,nl
          ss  =ss  +coef(k,kis,i)*pol(k)
          ssd =ssd +coef(k,kis,i)*pold(k)
          ssdd=ssdd+coef(k,kis,i)*poldd(k)
 15       CONTINUE
          xi2(i)  =ss
          xi2d(i) =ssd*ft
          xi2dd(i)=ssdd*ft2
 16       CONTINUE
* Superposition of the two interpolations
          ts1=t(ksip+nv)
          ts2=t(ksip+nv+ns)
          ft=1.d0/(ts2-ts1)
          ft2=ft**2
          fc=ft*(ti-ts1)
          IF(fc.LT.0.d0.OR.fc.GT.1.d0)
     +        STOP '**** pcwlgi: internal error (07) ****'
          CALL smoocn(fc,c1,c1d,c1dd,nsmord)
          c1d =c1d*ft
          c1dd=c1dd*ft2
          c2  = 1-c1
          c2d =  -c1d
          c2dd=  -c1dd
          DO 7 i=1,nd
          xi(i)=c1*xi1(i)+c2*xi2(i)
          IF(ntder.GE.1) THEN
              xid(i)=c1d*xi1(i)+c1*xi1d(i)+c2d*xi2(i)+c2*xi2d(i)
          ELSE
              xid(i)=0.d0
          END IF
          IF(ntder.GE.2) THEN
              xidd(i)=c1dd*xi1(i)+2*c1d*xi1d(i)+c1*xi1dd(i)
     +               +c2dd*xi2(i)+2*c2d*xi2d(i)+c2*xi2dd(i)
          ELSE
              xidd(i)=0.d0
          END IF
 7        CONTINUE
      ELSE
          DO 8 i=1,nd
          xi(i)=xi1(i)
          IF(ntder.GE.1) THEN
              xid(i)=xi1d(i)
          ELSE
              xid(i)=0.d0
          END IF
          IF(ntder.GE.2) THEN
              xidd(i)=xi1dd(i)
          ELSE
              xidd(i)=0.d0
          END IF
 8        CONTINUE
      END IF

      iret=0

      END
