* Copyright (C) 1999-2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: October 31, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         C R M S C N                           *
*  *                                                               *
*  *   Computes a-priori observation RMS based on known classes    *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    OBSCOD    -  Observatory code
*           ADS       -  Accuracy description string (RA/DEC)
*           MPCTYP    -  Observation type (column 15 of MPC record)
*           TDT       -  Time (MJD, TDT)
*
* OUTPUT:   RMSA      -  A-priori RMS of RA (arcsec) before sub. ave.
*           RMSD      -  A-priori RMS of DEC (arcsec) before sub. ave.
*           AVEA      -  Average residual (bias) in RA (arcsec)
*           AVED      -  Average residual (bias) in DEC (arcsec)
*           RMSAA     -  A-priori RMS of RA (arcsec) after sub. ave.
*           RMSDA     -  A-priori RMS of DEC (arcsec) after sub. ave.
*           IDCL      -  Class progressive number in index
*           STEP      -  RMS assignation steps:
*                           0 = not assigned
*                           1 = specific station class
*                           2 = mixed class
*           TDTLIM    -  Class limits (MJD, TDT)
*
* WARNING: if no valid class for the observation is found, the
*          corresponding output rms is set to a negative value
*
      SUBROUTINE crmscn(obscod,ads,mpctyp,tdt,rmsa,rmsd,avea,aved,
     +                  rmsaa,rmsda,idcl,step,tdtlim)
      IMPLICIT NONE

      INTEGER obscod,idcl(2),step(2)
      DOUBLE PRECISION rmsa,rmsd,avea,aved,rmsaa,rmsda,tdt,tdtlim(2,2)
      CHARACTER*(*) mpctyp,ads(2)

      INCLUDE 'parobc.h'
      INCLUDE 'parrms.h'

* NEEDED common blocks:
      INCLUDE 'rmscl.h'

      INTEGER ic,i,i1,i2,i3
      DOUBLE PRECISION rms1,ave1,rms1a

      IF(iiccrm.NE.36) STOP '**** crmscn: internal error (01) ****'
      IF(obscod.LT.obsc1 .OR. obscod.GT.obsc2)
     +    STOP '**** crmscn: input error (01) ****'

      DO 20 ic=1,2
      rms1=-1
      ave1=0
      rms1a=-1
      step(ic)=0
      idcl(ic)=0
      tdtlim(1,ic)=-9.D9
      tdtlim(2,ic)=-9.D9
      IF(crmobp(1,obscod,ic).GT.0) THEN
          DO 1 i=crmobp(1,obscod,ic),crmobp(2,obscod,ic)
          IF(crmad(i,ic).NE.ads(ic)) GOTO 1
          IF(crmotd(i,ic).NE.mpctyp) GOTO 1
          IF(tdt.LT.crmot1(i,ic) .OR. tdt.GE.crmot2(i,ic)) GOTO 1
          rms1=crmrms(i,ic)
          ave1=crmave(i,ic)
          rms1a=crmrma(i,ic)
          idcl(ic)=i
          step(ic)=1
          tdtlim(1,ic)=crmot1(i,ic)
          tdtlim(2,ic)=crmot2(i,ic)
          GOTO 2
 1        CONTINUE
 2        CONTINUE
      END IF
      IF(step(ic).GT.0) GOTO 10
      DO 3 i1=1,crx1n(ic)
      IF(ads(ic).EQ.crx1ad(i1,ic)) THEN
          DO 4 i2=crx1pt(1,i1,ic),crx1pt(2,i1,ic)
          IF(mpctyp.EQ.crx2ty(i2,ic)) THEN
              DO 5 i3=crx2pt(1,i2,ic),crx2pt(2,i2,ic)
              IF(tdt.GE.crx3t(1,i3,ic) .AND. tdt.LT.crx3t(2,i3,ic)) THEN
                  rms1=crx3r(i3,ic)
                  ave1=crx3a(i3,ic)
                  rms1a=crx3ra(i3,ic)
                  idcl(ic)=i3
                  step(ic)=2
                  tdtlim(1,ic)=crx3t(1,i3,ic)
                  tdtlim(2,ic)=crx3t(2,i3,ic)
                  GOTO 10
              END IF
 5            CONTINUE
          END IF
 4        CONTINUE
      END IF
 3    CONTINUE

 10   CONTINUE
      IF(ic.EQ.1) THEN
          rmsa=rms1
          avea=ave1
          rmsaa=rms1a
      ELSE
          rmsd=rms1
          aved=ave1
          rmsda=rms1a
      END IF
 20   CONTINUE

      END
