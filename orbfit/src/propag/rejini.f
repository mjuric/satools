* Copyright (C) 1998 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 14, 1998 Steve Chesley
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         R E J I N I                           *
*  *                                                               *
*  *        Initialization of options for outlier rejection        *
*  *                                                               *
*  *****************************************************************
*
      SUBROUTINE rejini
      IMPLICIT NONE

* Common blocks to be initialized:
      INCLUDE 'comrej.h'

      LOGICAL found,fail1,fail

      fail=.false.

      autrej=.true.
      CALL rdnlog('reject.','auto',autrej,.false.,found,
     +            fail1,fail)

      rejopp=.false.
      CALL rdnlog('reject.','rejopp',rejopp,.false.,found,
     +            fail1,fail)

      x2rej=8.0D0
      CALL rdnrea('reject.','chi2_reject',x2rej,.false.,found,
     +            fail1,fail)

      x2rec=7.0D0
      CALL rdnrea('reject.','chi2_recover',x2rec,.false.,found,
     +            fail1,fail)

      x2frac=0.25D0
      CALL rdnrea('reject.','chi2_frac',x2frac,.false.,found,
     +            fail1,fail)

      delrej=1.D-1
      CALL rdnrea('reject.','conv_cntr',delrej,.false.,found,
     +            fail1,fail)

      itmaxr=15
      CALL rdnint('reject.','nit_max',itmaxr,.false.,found,
     +            fail1,fail)

      fomax=50.D0
      CALL rdnrea('reject.','max_perc',fomax,.false.,found,
     +            fail1,fail)
      fomax=fomax/100

c Magnitude chi^2 values
      x2mrej=8.0D0
      CALL rdnrea('reject.','chi2_mag_rej',x2mrej,.false.,found,
     +            fail1,fail)

      x2mrec=7.0D0
      CALL rdnrea('reject.','chi2_mag_rec',x2mrec,.false.,found,
     +            fail1,fail)

      IF(fail) STOP '**** rejini: abnormal end ****'

      iicrej=36
      
      RETURN
      END
