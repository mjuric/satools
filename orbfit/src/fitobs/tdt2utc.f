c ========================================
c TDT2UTC
c patch 1.6.1, A. Milani, May 2, 1998
c time conversion from TDT to UTC (UT1 before 1972)
c warning: to be used after checking availability of TAI-UT table
c =======================================
      DOUBLE PRECISION FUNCTION tdt2utc(t1)
      IMPLICIT NONE
      DOUBLE PRECISION t1
c ======================================
      CHARACTER*3 scale
      INTEGER mjd1,mjd2,intlo
      DOUBLE PRECISION sec1,sec2
      IF(t1.LT.41317.d0) THEN
          scale='UT1'
      ELSE
          scale='UTC'
      END IF
      mjd1=intlo(t1)
      sec1=(t1-float(mjd1))*86400.d0
      CALL cnvtim(mjd1,sec1,'TDT',mjd2,sec2,scale)
      tdt2utc=mjd2+sec2/86400.d0
      RETURN
      END
