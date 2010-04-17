* Copyright (C) 1997-2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 11, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         E P H E M C                           *
*  *                                                               *
*  *                 Computation of ephemerides                    *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Output FORTRAN unit
*           ELTYPE    -  Type of orbital elements (EQU/KEP/CAR)
*           T0        -  Epoch of orbital elements (MJD, TDT)
*           ELEM      -  Orbital elements (ECLM J2000)
*           COVE      -  Covariance matrix of orbital elements
*           DEFCOV    -  Tells whether the covariance matrix is defined
*           T1        -  Starting time for ephemeris (MJD, TDT)
*           T2        -  Ending time for ephemeris (MJD, TDT)
*           DT        -  Ephemeris stepsize (d)
*           MASS      -  Mass (solar masses)
*           HMAG      -  H absolute magnitude (if <-100, missing)
*           GMAG      -  G slope parameter
*           IDSTA     -  Station identifier
*           SCALE     -  Timescale for output
*           FIELDS    -  Output fields (separated by commas)
*
* SUPPORTED OUTPUT FIELDS:
*     cal       calendar date
*     mjd       Modified Julian Day
*     coord     coordinates (RA and DEC, or ecliptic long. and lat.)
*     mag       magnitude
*     delta     distance from the Earth
*     r         distance from the Sun
*     elong     Sun elongation angle
*     phase     Sun phase angle
*     glat      galactic latitude
*     appmot    apparent motion
*     skyerr    sky plane error
*
      SUBROUTINE ephemc(unit,eltype,t0,elem,cove,defcov,t1,t2,dt,mass,
     +                  hmag,gmag,idsta,scale,fields)
      IMPLICIT NONE

      INTEGER unit,idsta
      CHARACTER*(*) eltype,scale,fields
      DOUBLE PRECISION t0,t1,t2,elem(6),dt,mass,hmag,gmag,cove(6,6)
      LOGICAL defcov

      DOUBLE PRECISION epst
      PARAMETER (epst=1.d-6)

* Max number of output fields
      INTEGER nfx
      PARAMETER (nfx=30)
* Max number of ephemeris points
      INTEGER nephx
      PARAMETER (nephx=600)
* Max length of output records
      INTEGER lrx
      PARAMETER (lrx=200)

      INTEGER nf,lh,ider,i,lf,lr,day,month,year,ia,ma,id,md,ls,neph,k,ip
      INTEGER srtord(nephx),lrv(nephx),obstyp,iepfor,la,lad,lfo,lf1,lf2
      INTEGER inb1,inb2
      PARAMETER (obstyp=1000)
      DOUBLE PRECISION tdt,alpha,delta,mag,hour,sa,sd,difft,signdt,cvf
      DOUBLE PRECISION gamad(2,2),sig(2),axes(2,2),err1,err2,pa
      DOUBLE PRECISION teph(nephx),velsiz
      CHARACTER*1 siga,sigd,anguni
      CHARACTER*3 cmonth(12)
      CHARACTER*20 field(nfx),frameo,amuni,amunid,amfor
      CHARACTER*(lrx) head1,head2,head3,head4,outrec,recv(nephx),blank
      CHARACTER cval*80
      LOGICAL outmot,outerr,outmag,oldrst,found,fail1,fail,usexp
* Time conversion (added by Steve Chesley)
      INTEGER mjdt,mjdout
      DOUBLE PRECISION sect,secout,tout

      INTEGER lench,intlo
      EXTERNAL lench,intlo

      INCLUDE 'trig.h'
      INCLUDE 'phase.h'
      INCLUDE 'restart.h'

      DATA cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     +            'Jul','Aug','Sep','Oct','Nov','Dec'/

      inb1=3
      inb2=2

* Parameter which should become options
      frameo='EQUATORIAL'

      signdt=1
      IF(dt.LT.0) signdt=-1

* List of ephemeris epochs
      neph=0
      tdt=t1
 2    CONTINUE
      difft=signdt*(tdt-t2)
      IF(difft.LE.epst) THEN
          neph=neph+1
          IF(neph.GT.nephx) STOP '**** ephemc: neph > nephx ****'
          teph(neph)=tdt
          tdt=t1+neph*dt
          GOTO 2
      END IF

* Sorting of ephemeris epochs
      CALL srtept(teph,neph,t0,srtord)

* List of output fields
      CALL spflds(fields,field,nf,nfx)

* COMPOSITION OF HEADER LINES
      head1=' '
      head2=' '
      head3=' '
      head4=' '
      blank=' '
      lh=0
      outmot=.false.
      outerr=.false.
      ider=0

      DO 5 i=1,nf
      IF(field(i).EQ.'cal') THEN
          head1(lh+1:)=blank
          head2(lh+1:)='    Date      Hour '
          ls=lench(scale)
          WRITE(head3(lh+1:),300) scale(1:ls)
          head4(lh+1:)=' =========== ======'
          lh=lh+19
      ELSEIF(field(i).EQ.'mjd') THEN
          head1(lh+1:)=blank
          head2(lh+1:)='     MJD     '
          ls=lench(scale)
          WRITE(head3(lh+1:),301) scale(1:ls)
          head4(lh+1:)=' ============'
          lh=lh+13
      ELSEIF(field(i).EQ.'coord') THEN
          IF(frameo.EQ.'EQUATORIAL') THEN
              head1(lh+1:)='     Equatorial coordinates  '
              head2(lh+1:)='       RA            DEC     '
          ELSEIF(frameo.EQ.'ECLIPTICAL') THEN
              head1(lh+1:)='      Ecliptic coordinates   '
              head2(lh+1:)='    Longitude      Latitude  '
          ELSE
              lf=lench(frameo)
              write(99,331) frameo(1:lf)
              STOP '**** ephemc: Abnormal end ****'
          END IF
          head3(lh+1:)='    h  m  s        d  ''  "   '
          head4(lh+1:)='  =============  ============'
          lh=lh+29
      ELSEIF(field(i).EQ.'delta') THEN
          head1(lh+1:)=blank
          head2(lh+1:)='  Delta '
          head3(lh+1:)='   (AU) '
          head4(lh+1:)=' ======='
          lh=lh+8
      ELSEIF(field(i).EQ.'r') THEN
          head1(lh+1:)=blank
          head2(lh+1:)='    R   '
          head3(lh+1:)='   (AU) '
          head4(lh+1:)=' ======='
          lh=lh+8
      ELSEIF(field(i).EQ.'elong') THEN
          head1(lh+1:)=blank
          head2(lh+1:)=' Elong'
          head3(lh+1:)=' (deg)'
          head4(lh+1:)=' ====='
          lh=lh+6
      ELSEIF(field(i).EQ.'phase') THEN
          head1(lh+1:)=blank
          head2(lh+1:)=' Phase'
          head3(lh+1:)=' (deg)'
          head4(lh+1:)=' ====='
          lh=lh+6
      ELSEIF(field(i).EQ.'mag') THEN
          outmag=(hmag.GT.-100.d0)
          IF(outmag) THEN
              head1(lh+1:)=blank
              head2(lh+1:)='  Mag '
              head3(lh+1:)='      '
              head4(lh+1:)=' ====='
              lh=lh+6
          END IF
      ELSEIF(field(i).EQ.'glat') THEN
          head1(lh+1:)=blank
          head2(lh+1:)=' Glat '
          head3(lh+1:)=' (deg)'
          head4(lh+1:)=' ====='
          lh=lh+6
      ELSEIF(field(i).EQ.'skyerr') THEN
          IF(defcov) THEN
              outerr=.true.
              ider=1
              head1(lh+1:)=blank
              head2(lh+1:)='       Sky plane error    '
              head3(lh+1:)='     Err1      Err2    PA '
              head4(lh+1:)='  ========  ======== ====='
              lh=lh+26
          END IF
      ELSEIF(field(i).EQ.'appmot') THEN
          fail=.false.
* Default format for apparent motion:
* IEPFOR = 1 -> (Vx, Vy)
* IEPFOR = 2 -> (V, PosAng)
          iepfor=1
          CALL sv2int('ephem.appmot.','format',cval,iepfor,.false.,
     +                found,fail1,fail)
          amunid='"/min'
          amuni=amunid
          CALL rdncha('ephem.appmot.','units',amuni,.false.,
     +                found,fail1,fail)
          IF(fail) STOP '**** ephemc: abnormal end ****'
* CVF = conversion factor from rad/d to selected unit
          CALL angvcf(amuni,cvf,fail)
          IF(fail) THEN
              la=lench(amuni)
              lad=lench(amunid)
              write(99,320) amuni(1:la),amunid(1:lad)
              amuni=amunid
              CALL angvcf(amuni,cvf,fail)
              IF(fail) STOP '**** ephemc: internal error (01) ****'
          END IF
          la=lench(amuni)
* Choice of the output format: normally F format is preferred
          amfor='(F10.4)'
* LFO = length of the output string
          lfo=10
          usexp=.false.
* Check whether F format can supply the required dynamic range
          IF(1.D0*cvf.GE.500.D0) usexp=.true.
          IF(3.D-3*cvf.LE.0.1D0) usexp=.true.
* Otherwise use exponential format
          IF(usexp) THEN
              amfor='(1P,E12.4)'
              lfo=12
          END IF
          lf1=lfo
          IF(iepfor.EQ.1) THEN
              lf2=lfo
          ELSE
              lf2=6
          END IF
          CALL filstr('App. motion',cval,lf1+lf2,inb1,0)
          head1(lh+1:)=cval
          IF(iepfor.EQ.1) THEN
              CALL filstr('RA',cval,lf1,inb1,0)
          ELSE
              CALL filstr('Vel',cval,lf1,inb1,0)
          END IF
          head2(lh+1:)=cval
          CALL filstr(amuni,cval,lf1,inb1,0)
          head3(lh+1:)=cval
          head4(lh+1:)='  =================='
          lh=lh+lf1
          IF(iepfor.EQ.1) THEN
              CALL filstr('DEC',cval,lf2,inb1,0)
              head2(lh+1:)=cval
              CALL filstr(amuni,cval,lf2,inb1,0)
              head3(lh+1:)=cval
              head4(lh+1:)='  =================='
          ELSE
              CALL filstr('PA',cval,lf2,inb2,0)
              head2(lh+1:)=cval
              CALL filstr('deg',cval,lf2,inb2,0)
              head3(lh+1:)=cval
              head4(lh+1:)=' ====='
          END IF
          lh=lh+lf2
          outmot=.true.
      ELSE
          lf=lench(field(i))
          write(99,330) field(i)(1:lf)
          STOP '**** ephemc: abnormal end ****'
      END IF
      IF(lh.GT.lrx) STOP '**** ephemc: lh > lrx ****'
 5    CONTINUE
 300  FORMAT('             (',A,') ')
 301  FORMAT('    (',A,')    ')
 330  FORMAT('Sorry, I don''t know how to produce field "',A,'"')
 331  FORMAT('Sorry, I don''t know "',A,'" reference system')
 320  FORMAT('WARNING(ephemc): I do not know how to use "',A,
     +    '" as units for apparent motion;'/
     +    17X,'using instead default units "',A,'"')
      WRITE(unit,100) head1(1:lh)
      WRITE(unit,100) head2(1:lh)
      WRITE(unit,100) head3(1:lh)
      WRITE(unit,100) head4(1:lh)
 100  FORMAT(A)

      oldrst=restar
      restar=.true.
* Start loop on ephemeris epochs
      DO 1 k=1,neph
      ip=srtord(k)
      tdt=teph(ip)
* Numerical integration
      IF(outerr) THEN
          CALL preobc(eltype,t0,idsta,tdt,elem,hmag,gmag,
     +                cove,obstyp,alpha,delta,mag,gamad,sig,axes)
      ELSE
          CALL preobs(eltype,t0,idsta,tdt,elem,obstyp,alpha,delta,
     +                hmag,gmag,mag)
      END IF
      restar=.false.

* COMPOSITION OF OUTPUT RECORD
* Time scale conversion
      mjdt=intlo(tdt)
      sect=(tdt-mjdt)*86400.d0
      CALL cnvtim(mjdt,sect,'TDT',mjdout,secout,scale)
      tout=secout/86400.d0+mjdout
*
      outrec=' '
      lr=0
      DO 6 i=1,nf
* Calendar date
      IF(field(i).EQ.'cal') THEN
          CALL mjddat(tout,day,month,year,hour)
          IF(month.LT.1 .OR. month.GT.12)
     +        STOP '**** ephemc: internal error (02) ****'
          WRITE(outrec(lr+1:),201) day,cmonth(month),year,hour
          lr=lr+19
* Modified Julian Day
      ELSEIF(field(i).EQ.'mjd') THEN
          WRITE(outrec(lr+1:),202) tout
          lr=lr+13
* Astrometric coordinates
      ELSEIF(field(i).EQ.'coord') THEN
          CALL sessag(alpha*hrad,siga,ia,ma,sa)
          IF(siga.NE.'+') STOP '**** ephemc: internal error (03) ****'
          CALL sessag(delta*degrad,sigd,id,md,sd)
          WRITE(outrec(lr+1:),203) ia,ma,sa,sigd,id,md,sd
          lr=lr+29
* Distance from the Earth
      ELSEIF(field(i).EQ.'delta') THEN
          WRITE(outrec(lr+1:),204) dis
          lr=lr+8
* Distance from the Sun
      ELSEIF(field(i).EQ.'r') THEN
          WRITE(outrec(lr+1:),204) dsun
          lr=lr+8
* Solar elongation
      ELSEIF(field(i).EQ.'elong') THEN
          WRITE(outrec(lr+1:),205) elo*degrad
          lr=lr+6
* Solar phase angle
      ELSEIF(field(i).EQ.'phase') THEN
          WRITE(outrec(lr+1:),205) pha*degrad
          lr=lr+6
* Magnitude
      ELSEIF(field(i).EQ.'mag') THEN
          IF(outmag) THEN
              WRITE(outrec(lr+1:),205) mag
              lr=lr+6
          END IF
* Sky plane error
      ELSEIF(field(i).EQ.'skyerr') THEN
          IF(outerr) THEN
              err1=sig(1)*degrad
              err2=sig(2)*degrad
* correction A. Milani 19/3/2000: remember right ascension increases
* from right to left
*             pa=ATAN2(axes(2,1),axes(1,1))
              pa=ATAN2(axes(2,1),-axes(1,1))
              anguni='d'
              IF(MAX(err1,err2).LT.1.d0) THEN
                  err1=err1*60
                  err2=err2*60
                  anguni=''''
                  IF(MAX(err1,err2).LT.1.d0) THEN
                      err1=err1*60
                      err2=err2*60
                      anguni='"'
                  END IF
              END IF
              WRITE(outrec(lr+1:),208) err2,anguni,err1,anguni,pa*degrad
              lr=lr+26
          END IF
* Galactic latitude
       ELSEIF(field(i).EQ.'glat') THEN
          WRITE(outrec(lr+1:),205) gallat*degrad
          lr=lr+6
* Apparent motion
      ELSEIF(field(i).EQ.'appmot') THEN
          IF(iepfor.EQ.1) THEN
              WRITE(outrec(lr+1:),amfor) adot*cvf
              lr=lr+lf1
              WRITE(outrec(lr+1:),amfor) ddot*cvf
              lr=lr+lf2
          ELSE
              velsiz=SQRT(adot**2+ddot**2)
              WRITE(outrec(lr+1:),amfor) velsiz*cvf
              lr=lr+lf1
              pa=ATAN2(adot,ddot)
              IF(pa.LT.0.D0) pa=pa+dpig
              WRITE(outrec(lr+1:),205) pa*degrad
              lr=lr+6
          END IF
      ELSE
          lf=lench(field(i))
          write(99,340) field(i)(1:lf)
          STOP '**** ephemc: internal error (04) ****'
      END IF
 6    CONTINUE
 201  FORMAT(I3,1X,A3,I5,F7.3)
 202  FORMAT(F13.6)
 203  FORMAT(2X,2I3,F7.3,2X,A1,I2,I3,F6.2)
 204  FORMAT(F8.4)
 205  FORMAT(F6.1)
 207  FORMAT(2F8.4)
 208  FORMAT(2(F9.3,A1),F6.1)
 340  FORMAT(' ERROR: illegal output field "',A,'"')
      IF(lr.GT.lrx) STOP '**** ephemc: lr > lrx ****'
      recv(ip)=outrec(1:lr)
      lrv(ip)=lr
 1    CONTINUE
      restar=oldrst

* Output of ephemeris records in the required order
      DO 3 ip=1,neph
      lr=lrv(ip)
      WRITE(unit,100) recv(ip)(1:lr)
 3    CONTINUE

      END
