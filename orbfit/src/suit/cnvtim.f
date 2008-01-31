* Author: Mario Carpino (carpino@brera.mi.astro.it)
* Version: April 1, 1996
*
*  *****************************************************************
*  *                                                               *
*  *                         C N V T I M                           *
*  *                                                               *
*  *          General purpose time conversion routine              *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    MJD1      -  Modified Julian Day (integer part)
*           SEC1      -  Seconds within day
*           SCALE1    -  Input time scale
*           SCALE2    -  Output (required) time scale
*
* OUTPUT:   MJD2      -  Modified Julian Day (integer part)
*           SEC2      -  Seconds within day
*
* Supported time scales:
*        UT1
*        TAI
*        UTC
*        TDT = TT = ET
*
      subroutine cnvtim(mjd1,sec1,scale1,mjd2,sec2,scale2)
      implicit none

      integer mjd1,mjd2
      double precision sec1,sec2
      character*3 scale1,scale2,eqsc,eqsc2

      external itaiut,deltt
      integer loops,nit,mjd2r,mjdt,itaiut
      character*3 scale
      double precision tjm2,dt,tjmt,sec2r,err,sect,dat,deltt

* ET-TAI in seconds
      double precision etmtai
      parameter (etmtai=32.184d0)
* Max number of iterations
      integer nitmax
      parameter (nitmax=5)
* Error limit for iterations
      double precision epst
      parameter (epst=1.d-10)

      loops=0

      mjd2=mjd1
      sec2=sec1
* Current timescale (in which mjd2,sec2 are given)
      scale=scale1

* Equivalent timescales (de-aliasing non-standard names)
      eqsc=scale
      if(eqsc.eq.'TT'.or.eqsc.eq.'ET') eqsc='TDT'
      eqsc2=scale2
      if(eqsc2.eq.'TT'.or.eqsc2.eq.'ET') eqsc2='TDT'

* Check on timescales
      if(eqsc.ne.'UT1'.and.eqsc.ne.'TDT'.and.eqsc.ne.'TAI'.and.
     .        eqsc.ne.'UTC') then
          write(*,103) scale1
          stop ' **** cnvtim: abnormal end ****'
      end if
      if(eqsc2.ne.'UT1'.and.eqsc2.ne.'TDT'.and.eqsc2.ne.'TAI'.and.
     .        eqsc2.ne.'UTC') then
          write(*,103) scale2
          stop ' **** cnvtim: abnormal end ****'
      end if
 103  format(' **** cnvtim: unsupported timescale "',a,'" ****')

 1    continue
      call timnf(mjd2,sec2,eqsc)

* Required timescale has been reached
      if(eqsc.eq.eqsc2) return

* Check on infinite loops
      if(loops.gt.6) stop' **** cnvtim: too many loops ****'
      loops=loops+1

* Transformations are performed according to the following path:
*
*                 UT1 -- TDT -- TAI -- UTC
*
      if(eqsc.eq.'UT1') then

* Conversion UT1 --> TDT
          tjm2=mjd2+sec2/86400.d0
          dt=deltt(tjm2)
          sec2=sec2+dt
          eqsc='TDT'

      elseif(eqsc.eq.'TDT')then

          if(eqsc2.eq.'UT1')then

* Conversion TDT --> UT1 (iterative method)
*   a) computation of DT = TDT - UT1 using (mjd2,sec2) (TDT) as an
*      approximate value of UT1
              tjm2=mjd2+sec2/86400.d0
              dt=deltt(tjm2)
*   b) subtract DT from (mjd2,sec2), finding a first approximation
*      for UT1
              mjdt=mjd2
              sect=sec2-dt
*   start iterations
              nit=0
 3            call timnf(mjdt,sect,'UT1')
              nit=nit+1
              if(nit.gt.nitmax)then
                  write(*,100) mjd1,sec1,scale1
                  stop' **** cnvtim: abnormal end ****'
              endif
*   c) try to find the starting value of TDT from the approximate
*      value of UT1
              tjmt=mjdt+sect/86400.d0
              dt=deltt(tjmt)
              mjd2r=mjdt
              sec2r=sect+dt
              call timnf(mjd2r,sec2r,'TDT')
*   d) computation of error and correction of the approximate value
              err=(mjd2r-mjd2)*86400.d0+sec2r-sec2
              if(abs(err).gt.epst)then
                  sect=sect-err
                  goto 3
              endif
              mjd2=mjdt
              sec2=sect
              eqsc='UT1'

          else

* Conversion TDT --> TAI
              sec2=sec2-etmtai
              eqsc='TAI'
          endif

      elseif(eqsc.eq.'TAI')then

          if(eqsc2.eq.'UTC')then

* Conversion TAI --> UTC (iterative method)
*   a) computation of DAT = TAI - UTC using (mjd2,sec2) (TAI) as an
*      approximate value of UTC
              mjdt=mjd2
              sect=sec2
              dat=itaiut(mjdt)
*   b) subtract DAT from (mjd2,sec2), finding a first approximation
*      for UTC
              sect=sec2-dat
*   start iterations
              nit=0
 2            call timnf(mjdt,sect,'UTC')
              nit=nit+1
              if(nit.gt.nitmax)then
                  write(*,101) mjd1,sec1,scale1
                  stop' **** cnvtim: abnormal end ****'
              end if
*   c) try to find the starting value of TAI from the approximate
*      value of UTC
              mjd2r=mjdt
              sec2r=sect+itaiut(mjdt)
              call timnf(mjd2r,sec2r,'TAI')
*   d) computation of error and correction of the approximate value
              err=(mjd2r-mjd2)*86400.d0+sec2r-sec2
     .               +itaiut(mjd2r)-itaiut(mjd2)
              if(abs(err).gt.epst)then
                  sect=sect-err
                  goto 2
              endif
              mjd2=mjdt
              sec2=sect
              eqsc='UTC'
          else

* Conversion TAI --> TDT
              sec2=sec2+etmtai
              eqsc='TDT'
          endif

      elseif(eqsc.eq.'UTC')then

* Conversione UTC --> TAI
          sec2=sec2+itaiut(mjd2)
          eqsc='TAI'

      else
          write(*,102) eqsc
          stop ' **** cnvtim: abnormal end ****'
      endif

      goto 1

 100  format(' **** cnvtim: nit > nitmax (UT1 --> TDT) ****'/
     .       ' mjd =',i6,' sec =',f12.4,' scale = ',a)
 101  format(' **** cnvtim: nit > nitmax (TAI --> UTC) ****'/
     .       ' mjd =',i6,' sec =',f12.4,' scale = ',a)
 102  format(' **** cnvtim: scale "',a,'" not supported ****')
      end
