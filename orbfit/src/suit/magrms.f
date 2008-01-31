************************************************************************
* 'magrms' returns the default magnitude rms based on the MPC obs string
************************************************************************
      DOUBLE PRECISION FUNCTION magrms(magstr,tdt,idsta,typ)
      IMPLICIT NONE
      
      CHARACTER*6 magstr
c obs. type (from column 15 of .obs format), station code, time MJD
      CHARACTER*1 typ
      INTEGER idsta
      DOUBLE PRECISION tdt
      INTEGER ll,lench
c magnitude weighting for now is simple; 
c should be based on digits given,color
      ll=lench(magstr)
      IF(ll.le.0)THEN
         magrms=-1.d0
         RETURN
      ENDIF
      IF(magstr(3:5).eq.'   ')THEN
         magrms=1.0
      ELSEIF(magstr(5:5).eq. ' ')THEN
         magrms=0.7
      ELSE
         magrms=0.5
      ENDIF

      RETURN
      END
