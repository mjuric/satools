c =========================================
c NOMOID - compute moid using lowell routine
c nodal distances and Minimum Orbital Intersection Distance
c with respect to the Earth
c for an orbit, given in equinoctal elements, at a given time
c =========================================
      SUBROUTINE nomoid(t0,eq0,moid,icon,dnp,dnm)
      IMPLICIT NONE
c ===========INPUT=====================
c elements of asteroid (equinoctal), epoch (MJD)
      DOUBLE PRECISION eq0(6),t0
c ===========OUTPUT=================================
c MOID, iteration count and succ. flag
c ascending node distance,  descending node distance
      DOUBLE PRECISION moid,dnp,dnm
c archaic argument
      INTEGER icon
c ==========END INTERFACE================================
c get gms (GM_sun)
      INCLUDE 'sunmass.h'
      include 'trig.h'
c commons for lowell routine
      DOUBLE PRECISION AP1,AN1,AI1,AE1,AA1,AP2,AN2,AI2,AE2,AA2
      COMMON/ORB1/AP1,AN1,AI1,AE1,AA1
      COMMON/ORB2/AP2,AN2,AI2,AE2,AA2
c elements of Earth (equinoctal)
      DOUBLE PRECISION eqp(6),eq01(6),eqp1(6),enne
c archaic parameter
      icon=0
c ======================================================
c get Earth elements
      CALL earth(t0,eqp)
c compute nodal distances
      CALL nodedi(eq0,eqp,dnp,dnm)
c compute moid
c =============== COMPUTE THEIR MOID ====================
C	   APn - Argument of perihelion (deg)
C	   ANn - Longitude of ascending node (deg)
C	   AIn - Inclination (deg)
C	   AEn - Eccentricity
C	   AAn - Semi-major axis (AU)
      call coocha(eq0,'EQU',gms,eq01,'KEP',enne)
      AP1=eq01(5)*degrad
      AN1=eq01(4)*degrad
      AI1=eq01(3)*degrad
      AE1=eq01(2)
      AA1=eq01(1)
      call coocha(eqp,'EQU',gms,eqp1,'KEP',enne)
      AP2=eqp1(5)*degrad
      AN2=eqp1(4)*degrad
      AI2=eqp1(3)*degrad
      AE2=eqp1(2)
      AA2=eqp1(1)
c      write(99,*)AA1,AE1,AI1,AN1,AP1
c      write(99,*)eq0
c      write(99,*)AA2,AE2,AI2,AN2,AP2
c      write(99,*)eqp
      call lowmoid(moid)
      RETURN
      END
