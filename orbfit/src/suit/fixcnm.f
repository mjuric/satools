* Copyright (C) 1998 by Mario Carpino (carpino@brera.mi.astro.it),
*
* Version: June 19, 1998
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         F I X C N M                           *
*  *                                                               *
*  *                Fix covariance/normal matrices                 *
*  *                                                               *
*  *****************************************************************
*
* IN/OUT:   DEFCOV    -  Tells whether the covariance matrix is defined
*           DEFNOR    -  Tells whether the normal matrix is defined
*           COVE      -  Covariance matrix of orbital elements
*           NORE      -  Normal matrix of orbital elements
*
* OUTPUT:   DEFCN     -  Tells whether covariance/normal matrices
*                            are defined
*
* The purpose of this routine is to compute the covariance or
* normal matrix of orbital elements when only one of the two is
* available
*
      SUBROUTINE fixcnm(defcov,defnor,defcn,cove,nore)
      IMPLICIT NONE

      DOUBLE PRECISION cove(6,6),nore(6,6)
      LOGICAL defcov,defnor,defcn

      DOUBLE PRECISION err
      PARAMETER (err=1.D-15)

      DOUBLE PRECISION tmp(6)
      INTEGER i,k,indp

      defcn=(defcov.AND.defnor)
      IF(defcn) RETURN

      IF(defcov) THEN
          DO 1 i=1,6
          DO 1 k=1,6
          nore(i,k)=cove(i,k)
 1        CONTINUE
          CALL tchol(nore,6,6,indp,err)
          defnor=(indp.EQ.0)
          IF(defnor) CALL inver(nore,tmp,6,6)
      END IF
      IF(defnor) THEN
          DO 2 i=1,6
          DO 2 k=1,6
          cove(i,k)=nore(i,k)
 2        CONTINUE
          CALL tchol(cove,6,6,indp,err)
          defcov=(indp.EQ.0)
          IF(defcov) CALL inver(cove,tmp,6,6)
      END IF

      defcn=(defcov.AND.defnor)
      IF(defcn) RETURN

      DO 3 i=1,6
      DO 3 k=1,6
      cove(i,k)=0
      nore(i,k)=0
 3    CONTINUE
      defcov=.false.
      defnor=.false.

      END
