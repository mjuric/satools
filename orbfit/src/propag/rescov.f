c =========================================================
c RESCOV: rescaling factor of covariance matrix
c to be used as follows:
c     gamma ---> gamma *rescov**2
c     normatr--> normatr/rescov**2
      DOUBLE PRECISION FUNCTION rescov(nsolv,nused,csinor)
      IMPLICIT NONE
c input: number of parameters to be solved, nomber of scaalr obs,
      INTEGER nsolv,nused
c norm of residuals (RMS relative to the given observatory weights)
      DOUBLE PRECISION csinor
      IF(nsolv.lt.nused)THEN
         IF(csinor.gt.1.d0)THEN
            rescov=csinor*sqrt(float(nused)/float(nused-nsolv))
         ELSE
            rescov=sqrt(float(nused)/float(nused-nsolv))
         ENDIF
      ELSE
         rescov=1.d0
      ENDIF
      RETURN
      END

