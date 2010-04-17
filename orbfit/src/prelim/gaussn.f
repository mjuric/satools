* Copyright (C) 1998-2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: June 7, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         G A U S S N                           *
*  *                                                               *
*  *       Initial orbit determination with Gauss' method          *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    TOBS      -  Time of observations (MJD, TDT)
*           ALPHA     -  Right ascension (rad)
*           DELTA     -  Declination (rad)
*           OBSCOD    -  Observatory code
*           DEBUG     -  Print debug information
*           MULTI     -  Multi-line output
*
* OUTPUT:   ELEM      -  Orbital elements (mean ecliptic
*                                          and equinox J2000)
*           ELETYP    -  Type of orbital elements
*           T0        -  Epoch of orbital elements
*           NROOTS    -  Number of positive roots of 8th degree pol.
*           NSOL      -  Number of solutions (some roots may be
*                        discarded if they lead to solution with
*                        eccentricity > ECCMAX)
*           FAIL      -  Error flag
*           MSG       -  Error message
*
      SUBROUTINE gaussn(tobs,alpha,delta,obscod,elem,eletyp,
     +                  t0,nroots,nsol,fail,msg,debug,multi)
      IMPLICIT NONE

      INCLUDE 'parobx.h'

      INTEGER nroots,nsol,obscod(3)
      DOUBLE PRECISION tobs(3),alpha(3),delta(3)
      DOUBLE PRECISION elem(6,8),t0(8)
      CHARACTER*(*) msg,eletyp(8)
      LOGICAL fail,debug,multi

      INCLUDE 'trig.h'

* NEEDED common blocks:
      INCLUDE 'comgau.h'

      INTEGER i,j,k,ising,ir,it
      DOUBLE PRECISION xt(3,3),sinv0(3,3),a(3),b(3),c(3)
      DOUBLE PRECISION ra(3),rb(3),coef(0:8),a2star,b2star,r22,s2r2
      DOUBLE PRECISION esse0(3,3),cosd,det,tau1,tau3,tau13,gk,gkp,gm
      DOUBLE PRECISION roots(8),esse(3,3),esse1(3,3),sinv(3,3),r2m3
      DOUBLE PRECISION gcap(3),crhom(3),rho(3),xp(3,3),vp(3),xv(6)
      DOUBLE PRECISION vekp(6),v1(3),v2(3),vs,err,sca,vaber(3),xv1(6)
      DOUBLE PRECISION xp1(3,3),tis2,rot(3,3)
      DOUBLE PRECISION xve(6),vekpe(6),fs1,gs1,fs3,gs3,fggf
      CHARACTER*4 eltyp,eltype
      LOGICAL first
      DATA first/.true./

      SAVE first,gk,gkp,gm,rot

      DOUBLE PRECISION vsize,princ
      EXTERNAL vsize,princ

      IF(iicgau.NE.36) STOP '**** gaussn: internal error (01) ****'

      IF(first) THEN
          gk=0.01720209895d0
          gkp=gk
          gm=gkp**2
          CALL rotpn(rot,'EQUM','J2000',0.d0,'ECLM','J2000',0.d0)
          first=.false.
      END IF

      fail=.true.
      msg=' '
      nroots=0
      nsol=0

* COMPUTATION OF PRELIMINARY ORBIT
*
* ESSE = unit vector pointing in the direction of observations
      DO 10 k=1,3
      cosd=COS(delta(k))
      esse0(1,k)=cosd*COS(alpha(k))
      esse0(2,k)=cosd*SIN(alpha(k))
      esse0(3,k)=SIN(delta(k))
* Position of the observer
      DO 11 i=1,3
      CALL posobs(tobs(i),obscod(i),1,xt(1,i))
 11   CONTINUE
 10   CONTINUE

* Inverse of ESSE matrix
      DO 25 i=1,3
      DO 25 j=1,3
      sinv0(i,j)=esse0(i,j)
 25   CONTINUE
      CALL matin(sinv0,det,3,0,3,ising,1)
      IF(ising.NE.0) THEN
          IF(multi) THEN
              msg='Singular S matrix (coplanar orbits?)'
          ELSE
              msg='SingS'
          END IF
          RETURN
      END IF
      tis2=tobs(2)

* A and B vectors
      tau1=gkp*(tobs(1)-tis2)
      tau3=gkp*(tobs(3)-tis2)
      tau13=tau3-tau1
      a(1)= tau3/tau13
      a(2)=-1.d0
      a(3)=-(tau1/tau13)
      b(1)=a(1)*(tau13**2-tau3**2)/6.d0
      b(2)=0.d0
      b(3)=a(3)*(tau13**2-tau1**2)/6.d0

* Coefficients of 8th degree equation for r2
      CALL prodmv(ra,xt,a)
      CALL prodmv(rb,xt,b)
      a2star=sinv0(2,1)*ra(1)+sinv0(2,2)*ra(2)+sinv0(2,3)*ra(3)
      b2star=sinv0(2,1)*rb(1)+sinv0(2,2)*rb(2)+sinv0(2,3)*rb(3)
      r22=xt(1,2)**2+xt(2,2)**2+xt(3,2)**2
      s2r2=esse0(1,2)*xt(1,2)+esse0(2,2)*xt(2,2)+esse0(3,2)*xt(3,2)
      coef(8)=1.d0
      coef(7)=0.d0
      coef(6)=-(a2star**2)-r22-(2.d0*a2star*s2r2)
      coef(5)=0.d0
      coef(4)=0.d0
      coef(3)=-(2.d0*b2star*(a2star+s2r2))
      coef(2)=0.d0
      coef(1)=0.d0
      coef(0)=-(b2star**2)
      IF(coef(0).EQ.0) THEN
          msg='coef(0)=0'
          RETURN
      END IF
      CALL solv8(coef,roots,nroots)
      IF(debug) THEN
         write(99,520)
         write(99,521) (i,coef(i),i=0,8)
         write(99,513) 2
         IF(nroots.LE.0) THEN
             write(99,524)
         ELSE
             write(99,514)(i,roots(i),i=1,nroots)
         END IF
      END IF
 520  FORMAT(12X,'Coefficients of 8-th degree polynomial:')
 521  FORMAT(16X,'coef(',i1,')  =',F18.10)
 513  FORMAT(12X,'Possible roots for Sun-asteroid distance at ',
     +       'observation',I2,':')
 514  FORMAT(16X,'r(',i1,')  =',F10.6)
 524  FORMAT(16X,'none')

      IF(nroots.LE.0) THEN
          IF(multi) THEN
              msg='8th degree polynomial has no real roots'
          ELSE
              msg='NoPolSol'
          END IF
          RETURN
      END IF

* Orbital elements of preliminary solution
      DO 20 ir=1,nroots
      DO 69 i=1,3
      DO 69 k=1,3
      esse(i,k)=esse0(i,k)
      esse1(i,k)=esse0(i,k)
      sinv(i,k)=sinv0(i,k)
 69   CONTINUE
      r2m3=1.d0/(roots(ir)**3)
      c(1)=a(1)+b(1)*r2m3
      c(2)=-1.d0
      c(3)=a(3)+b(3)*r2m3
      CALL prodmv(gcap,xt,c)
      CALL prodmv(crhom,sinv,gcap)
      DO 13 k=1,3
      rho(k)=-(crhom(k)/c(k))
 13   CONTINUE

* Position of the asteroid at the time of observations
      DO 14 k=1,3
      DO 14 i=1,3
      xp(i,k)=xt(i,k)+rho(k)*esse(i,k)
 14   CONTINUE

* Gibbs' transformation, giving the velocity of the planet at the
* time of second observation
      CALL gibbs(xp,tau1,tau3,vp,gkp)
* Orbital elements of preliminary orbit
      DO 15 i=1,3
      xv(i)=xp(i,2)
      xv(i+3)=vp(i)
 15   CONTINUE

      CALL ccek1(vekp,eltyp,xv,gm)

      it=0
      IF(debug) THEN
          write(99,525) ir
          IF(eltyp.EQ.'KEP') THEN
              write(99,535) 'a',vekp(1),vekp(2)
          ELSEIF(eltyp.EQ.'COM') THEN
              write(99,535) 'q',vekp(1),vekp(2)
          ELSE
              STOP '**** gaussn: internal error (02) ****'
          END IF
      END IF
 525  FORMAT(12X,'ROOT NO.',I2)
 535  FORMAT(16X,'Preliminary orbit: ',A,' =',F10.5,';  ecc =',F10.5)

* CORRECTION OF PRELIMINARY ORBIT

 30   CONTINUE
      CALL modgeq(gkp,xp(1,2),xp(1,1),tau1,vekp,eltyp,fs1,gs1,v1)
      CALL modgeq(gkp,xp(1,2),xp(1,3),tau3,vekp,eltyp,fs3,gs3,v2)

      it=it+1
      DO 31 i=1,3
 31   vp(i)=(v1(i)+v2(i))/2.d0
      fggf=fs1*gs3-fs3*gs1
      c(1)=gs3/fggf
      c(3)=-(gs1/fggf)

* Updated set of orbital elements
      DO 33 i=1,3
      xv1(i)=xp(i,2)
 33   CONTINUE

* Correction for planetary aberration
      DO 87 k=1,3
      CALL ekcc1(vekp,eltyp,xv1,gm,tobs(k)-tis2)
      DO 94 i=1,3
      vaber(i)=-xv1(i+3)
 94   CONTINUE
      DO 88 i=1,3
      esse(i,k)=esse0(i,k)*rho(k)
 88   CONTINUE
      CALL aber1(esse(1,k),vaber,esse(1,k))
      vs=vsize(esse(1,k))
      DO 89 i=1,3
      esse(i,k)=esse(i,k)/vs
 89   CONTINUE
 87   CONTINUE

* Inverse of ESSE matrix
      DO 65 i=1,3
      DO 65 k=1,3
      sinv(i,k)=esse(i,k)
 65   CONTINUE
      CALL matin(sinv,det,3,0,3,ising,1)
      IF(ising.NE.0) THEN
          IF(debug) write(99,541) it
          GOTO 20
      END IF
 541  FORMAT(16X,'ROOT DISCARDED: singular S matrix at iteration',I3)
      CALL prodmv(gcap,xt,c)
      CALL prodmv(crhom,sinv,gcap)
      DO 23 k=1,3
      rho(k)=-(crhom(k)/c(k))
 23   CONTINUE
      DO 77 k=1,3
      DO 77 i=1,3
      xp1(i,k)=xt(i,k)+rho(k)*esse(i,k)
 77   CONTINUE

* Error with respect to previous iteration
      err=0.d0
      sca=0.d0
      DO 75 k=1,3
      DO 75 i=1,3
      err=err+(xp1(i,k)-xp(i,k))**2
      sca=sca+xp1(i,k)**2
      xp(i,k)=xp1(i,k)
 75   CONTINUE
      err=SQRT(err/sca)
      DO 16 i=1,3
      xv1(i)=xp(i,2)
      xv1(i+3)=vp(i)
 16   CONTINUE
      CALL ccek1(vekp,eltyp,xv1,gm)
      CALL prodmv(xve(1),rot,xv1(1))
      CALL prodmv(xve(4),rot,xv1(4))
      CALL ccek1(vekpe,eltype,xve,gm)

* Check for anomalous orbits
      IF(vekp(2).GT.eccmax) THEN
          IF(debug) write(99,542) vekp(2),it
          GOTO 20
      END IF
 542  FORMAT(16X,'ROOT DISCARDED: ecc =',1P,E12.4,0P,' at iteration',I3)

* Convergency control
      IF(err.LE.errmax) GOTO 26
      IF(it.LE.itmax) GOTO 30
      IF(debug) write(99,543)
 543  FORMAT(16X,'WARNING: iterations not converging')

* Final orbit
 26   CONTINUE
      nsol=nsol+1
      DO 76 i=1,6
      elem(i,nsol)=vekpe(i)
 76   CONTINUE
      eletyp(nsol)=eltype
      t0(nsol)=tis2
      IF(debug) THEN
          IF(eltyp.EQ.'KEP') THEN
              write(99,544) 'a',vekp(1),vekp(2),it
          ELSE
              write(99,544) 'q',vekp(1),vekp(2),it
          END IF
      END IF
 544  FORMAT(16X,'Final orbit:       ',A,' =',F10.5,';  ecc =',F10.5,
     +       ' (',I3,' iterations)')

 20   CONTINUE

      IF(nsol.LE.0) THEN
          IF(multi) THEN
              msg='No acceptable solution'
          ELSE
              msg='NoAccSol'
          END IF
          RETURN
      END IF

      fail=.false.
      IF(.NOT.multi) THEN
          msg='OK'
      END IF

      END
