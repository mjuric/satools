c ===============================================
c  NIGHTS
c function computing number of nights of observations
c Copyright A. Milani, OrbFit consortium, 21/9/2000
c ===============================================
      INTEGER FUNCTION nights(m,iobs,aln,den,tut,idsta,sel,
     +     rmsa,rmsd) 
      IMPLICIT NONE
c =========OBSERVATIONS =========================
      INCLUDE 'parobx.h'
c number of observations
      INTEGER m
c observations: alpha, delta, time (UT), station code 
      double precision aln(nobx),den(nobx),tut(nobx)
      integer idsta(nobx),iobs(nobx)
c RMS of observation error
      double precision rmsa(nobx),rmsd(nobx)
c selection flags; number of observations not discarded
      INTEGER sel(nobx)
c ===========END INTERFACE=======================
      double precision t1
      integer i
c ===============================================
      t1=tut(1)
      nights=1
      DO i=2,m
c This is the most trivial algorithm: if there is a 16 hours interval
c between two observations, they belong to different nights. 
c But a night containing only observations being discarded does not count
        IF(tut(i)-t1.gt.0.66d0.and.sel(i).gt.0)THEN
           nights=nights+1
           t1=tut(i)
        ENDIF
      ENDDO
      RETURN
      END
c ==================================================================
c this should be improved to cover the following "strange" cases:
c 1) an asteroid which is observed around the clock; the above algorithm
c    would rate it as a 3-nighter after 48 hours!
c 2) an isolate observation, maybe even with degraded accuracy, 
c    might not qualify as 'one night'
c 3) a single observation with radar (maybe, in the future, from a space misison)
c    could be considered equivalent to many, many nights
c All the data are available in the function call to add these improvements, 
c e.g. the RMS (rmsa, rmsd), the obs. type (iobs, 200x for radar)
c and the outlier rejection flag (sel)
c Note: the computation is currently done using UT; a leap second does not matter


