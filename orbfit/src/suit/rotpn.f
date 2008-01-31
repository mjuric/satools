* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: March 7, 1997
* ---------------------------------------------------------------------
*
*  ***************************************************************
*  *                                                             *
*  *                          R O T P N                          *
*  *                                                             *
*  *                  General purpose routine                    *
*  *  for transformations between different reference systems    *
*  *                                                             *
*  ***************************************************************
*
*
* INPUT:    RSYS1     -  Starting reference system (EQUM/EQUT/ECLM)
*           EPOCH1    -  Starting epoch (J2000/OFDATE)
*           DATE1     -  Starting date (MJD, TDT): relevant only if
*                        EPOCH1=OFDATE
*           RSYS2     -  Final reference system (EQUM/EQUT/ECLM)
*           EPOCH2    -  Final epoch (J2000/OFDATE)
*           DATE2     -  Final date (MJD, TDT): relevant only if
*                        EPOCH2=OFDATE
*
* OUTPUT:   ROT(3,3)  -  Rotation matrix giving the transformation from
*                        starting to final reference systems:
*                        X2 = ROT X1
*
      SUBROUTINE rotpn(rot,rsys1,epoch1,date1,rsys2,epoch2,date2)
      IMPLICIT NONE

      DOUBLE PRECISION rot(3,3),date1,date2
      CHARACTER*(*) rsys1,rsys2,epoch1,epoch2

      DOUBLE PRECISION eps
      PARAMETER (eps=1.d-6)

      INCLUDE 't2000.h'

      DOUBLE PRECISION r(3,3),date,obl
      INTEGER i,j,nit
      LOGICAL error,epdif
      CHARACTER rsys*4,epoch*10

      DOUBLE PRECISION obleq
      EXTERNAL obleq

      CALL chkref(rsys1,epoch1,error)
      IF(error) THEN
          WRITE(*,110) ' starting',rsys1,epoch1
          STOP '**** rotpn: abnormal end ****'
      END IF
      CALL chkref(rsys2,epoch2,error)
      IF(error) THEN
          WRITE(*,110) ' final',rsys1,epoch1
          STOP '**** rotpn: abnormal end ****'
      END IF
 110  FORMAT(' ERROR: unsupported ',A,' reference system:'/
     +       10X,'RSYS  = ',A/
     +       10X,'EPOCH = ',A)

* Starting point
      rsys=rsys1
      epoch=epoch1
      IF(epoch.EQ.'J2000') THEN
          date=t2000
      ELSEIF(epoch.EQ.'OFDATE') THEN
          date=date1
      ELSE
          STOP '**** rotpn: internal error (01) ****'
      END IF

* Initialization of the rotation matrix (equal to the unit matrix)
      DO 1 i=1,3
      DO 1 j=1,3
 1    rot(i,j)=0.d0
      DO 2 i=1,3
 2    rot(i,i)=1.d0

* Building of the rotation matrix
      nit=0
 3    CONTINUE

* Understand whether the final epoch and the current epoch are the
* same of not
      IF(epoch.EQ.epoch2) THEN
          IF(epoch.EQ.'J2000') THEN
              epdif=.false.
          ELSEIF(epoch.EQ.'OFDATE') THEN
              epdif=(ABS(date-date1).GT.eps)
          ELSE
              STOP '**** rotpn: internal error (02) ****'
          END IF
      ELSE
          epdif=.true.
      END IF

* Different epochs: the transformation have to pass through J2000
* equatorial system
      IF(epdif) THEN
          IF(epoch.NE.'J2000') THEN
              IF(rsys.EQ.'ECLM') THEN
* Transformation ecliptical --> equatorial
                  obl=obleq(date)
                  CALL rotmt(-obl,r,1)
                  CALL mult3(r,rot)
                  rsys='EQUM'
              ELSEIF(rsys.EQ.'EQUT') THEN
* Transformation true equator --> mean equator
                  CALL rnut80(date,r)
                  CALL trsp(r)
                  CALL mult3(r,rot)
                  rsys='EQUM'
              ELSEIF(rsys.EQ.'EQUM') THEN
* Transformation to J2000 (precession)
                  CALL prec(date,r)
                  CALL trsp(r)
                  CALL mult3(r,rot)
                  epoch='J2000'
                  date=t2000
              ELSE
                  STOP '**** rotpn: internal error (03) ****'
              END IF
          ELSE
              IF(rsys.EQ.'ECLM') THEN
* Transformation ecliptical --> equatorial
                  obl=obleq(t2000)
                  CALL rotmt(-obl,r,1)
                  CALL mult3(r,rot)
                  rsys='EQUM'
              ELSEIF(rsys.EQ.'EQUT') THEN
* Transformation true equator --> mean equator
                  CALL rnut80(t2000,r)
                  CALL trsp(r)
                  CALL mult3(r,rot)
                  rsys='EQUM'
              ELSEIF(rsys.EQ.'EQUM') THEN
                  IF(epoch2.EQ.'OFDATE') THEN
                      CALL prec(date2,r)
                      CALL mult3(r,rot)
                      epoch=epoch2
                      date=date2
                  ELSE
                      STOP '**** rotpn: internal error (04) ****'
                  END IF
              ELSE
                  STOP '**** rotpn: internal error (05) ****'
              END IF
          END IF
* Same epoch
      ELSE
          IF(rsys.EQ.rsys2) RETURN
* Transformation of reference system at the same epoch (date)
          IF(rsys.EQ.'EQUT') THEN
* Transformation true equator --> mean equator
              CALL rnut80(date,r)
              CALL trsp(r)
              CALL mult3(r,rot)
              rsys='EQUM'
          ELSEIF(rsys.EQ.'ECLM') THEN
* Transformation ecliptical --> equatorial
              obl=obleq(date)
              CALL rotmt(-obl,r,1)
              CALL mult3(r,rot)
              rsys='EQUM'
          ELSEIF(rsys.EQ.'EQUM') THEN
              IF(rsys2.EQ.'EQUT') THEN
* Transformation mean equator --> true equator
                  CALL rnut80(date,r)
                  CALL mult3(r,rot)
                  rsys='EQUT'
              ELSEIF(rsys2.EQ.'ECLM') THEN
* Transformation equatorial --> ecliptical
                  obl=obleq(date)
                  CALL rotmt(obl,r,1)
                  CALL mult3(r,rot)
                  rsys='ECLM'
              ELSE
                  STOP '**** rotpn: internal error (06) ****'
              END IF
          ELSE
              STOP '**** rotpn: internal error (07) ****'
          END IF
      END IF
      nit=nit+1
      IF(nit.GT.20) STOP '**** rotpn: internal error (08) ****'
      GOTO 3

      END
