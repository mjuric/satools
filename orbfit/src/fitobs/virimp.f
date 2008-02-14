c ==============================================
c  VIRtual IMPactor ephemerides
      subroutine virimp(tpc,dtpdet,axes,sig,ceicel,b,v,tc,eqc,gc,cc,
     +     hmag,gmag,iun20)
      IMPLICIT NONE
c ===============input=================
c magnitude
      DOUBLE PRECISION hmag,gmag
c output unit
      INTEGER iun20
c target plane coordinates: cartesian
      DOUBLE PRECISION tpc(2)
c computation of target ellipse
      DOUBLE PRECISION axes(2,2),sig(2),dtpdet(6,2)
c correspondent ellipse in orbital lements space
      DOUBLE PRECISION b(2,2),ceicel(4,2),v(6,6)
c initial conditions: epoch, elements
      DOUBLE PRECISION tc,eqc(6)
c normal and covariance matrices
      DOUBLE PRECISION gc(6,6),cc(6,6)
c ==============end interface=================
c option flag
      INTEGER ivir
c sigma level across the LOV
      DOUBLE PRECISION sigimp
c safe distance from the Earth
      DOUBLE PRECISION dsafe
c rectangle, and its copy in the elements space
      DOUBLE PRECISION tpstr(2),tpwea(2)
      DOUBLE PRECISION elems(6,4),del2(2),del4(4),dels(6)
      DOUBLE PRECISION delems(6),delemw(6),selems(6),selemw(6)
c observation data (proper motion, elongation, distance)
      INCLUDE 'phase.h'
c times
      DOUBLE PRECISION tmjd,tut,sec1,sec2
      INTEGER mjd1,mjd2
c call to seleph
      DOUBLE PRECISION  tut1,tdt1,tut2,tdt2,dt     
      INTEGER idsta
      CHARACTER*3 scale
c for preobs
      DOUBLE PRECISION alpha,delta,appmag
c station code, observation type
      INTEGER ids,iob1
c integer indexes, lengths, functions, units
      INTEGER j,jj,intlo,ln,iuneph,iunvir
c for outobc: covariance of the observations
      DOUBLE PRECISION gamad(2,2),axesky(2,2),sigsky(2)
c ephemeris options
      CHARACTER*80 fields
      DOUBLE PRECISION mass
      CHARACTER*45 file
      INCLUDE 'trig.h'
c file name
      CHARACTER*80 titnam
c      DOUBLE PRECISION av(5),dv(5)
c     DOUBLE PRECISION dxl,dyl,prscag
c tests
c     DOUBLE PRECISION aa(2,6),id(2,2),dtpcde(2,6)
c acll to preob4
c confidence boundary, line of max variation (alpha, delta, app. magnitude)
      INTEGER npo, npox, ibv, npo1, inl
      PARAMETER (npox=4000)
      DOUBLE PRECISION sigma, al(npox),de(npox),appmagv(npox)
c line of elements
      DOUBLE PRECISION elm(6,npox)
c menu empty items
      CHARACTER*20 menunam
      CHARACTER*50 s4,s5,s6,s7,s8,s9,s10
c multiple data for confidence boundary
      INCLUDE 'npoint.h'
c =====================================================
c chase is open for the virtual impactor; what to do?
 10   menunam='null'
      CALL menu(ivir,menunam,3,'how to catch it?=',
     +     'exploratory ephemerides=',
     +     'select observation time=',
     +     'output impact orbital elements=',
     +     s4,s5,s6,s7,s8,s9,s10)
      IF(ivir.eq.0)RETURN
      IF(ivir.eq.1)THEN
c ======= GENERATE EPHEMERIS =========
c select time interval, step
         CALL seleph(tut1,tdt1,tut2,tdt2,dt,idsta)
         CALL filnam('.','virimp','eph',file,ln)
         CALL filopn(iuneph,file(1:ln),'unknown')
         fields='cal,mjd,coord,mag,elong,glat,r,delta,appmot,skyerr'
         scale='UTC'
         IF(nint(abs(tdt2-tdt1)/dt).gt.500)THEN
            write(0,*)'Too many ephemeris points: ',
     +           nint(abs(tdt2-tdt1)/dt)
            write(0,*)'Select a time interval and time span to ',
     +        'ensure that there are fewer than 500 points.'
            goto 10
         ELSE
            CALL ephemc(iuneph,'EQU',tc,eqc,gc,.true.,tdt1,tdt2,
     +           dt,mass,hmag,gmag,idsta,scale,fields)
         ENDIF
         CALL filclo(iuneph,' ')
         WRITE(0,*)' look at the ephemerides in file ./virimp.eph '
      ELSEIF(ivir.eq.3)THEN
         CALL filopn(iunvir,'virimp.eq0','unknown')
c output header 
         CALL wromlh (iunvir,'ECLM','J2000')
         CALL wromlr (iunvir,'virimp',eqc,'EQU',tc,gc,.true.,
     +        cc,.true.,hmag,gmag,0.d0)
         CALL filclo(iunvir,' ')
      ELSEIF(ivir.eq.2)THEN
c ============PREDICT OBSERVATION==================
c ======two dimensional preimage===============
c compute rectangle on the MTP  enclosing all the impact points;
c note that the target plane point tpc is used as origin
         sigimp=5.d0
         dsafe=5.d0*4.2e-5
         DO j=1,2
            tpstr(j)=axes(j,1)*sig(1)*sigimp
            tpwea(j)=axes(j,2)*dsafe
         ENDDO
c find corresponding orbital elements at epoch
         CALL mulmav(b,2,2,tpstr,2,del2)
         CALL mulmav(ceicel,4,2,del2,2,del4)
         CALL vcopy(2,del2,dels)
         DO jj=1,4
            dels(2+jj)=-del4(jj)
         ENDDO
         CALL mulmav(v,6,6,dels,6,delems)
c        WRITE(0,*)tpstr
c        WRITE(0,*)delems
c linear map from ellipse
c        dxl=prscag(6,delems,dtpdet(1,1))
c        dyl=prscag(6,delems,dtpdet(1,2))
c        WRITE(0,*)dxl,dyl
c find corresponding orbital elements at epoch
         CALL mulmav(b,2,2,tpwea,2,del2)
         CALL mulmav(ceicel,4,2,del2,2,del4)
         CALL vcopy(2,del2,dels)
         DO jj=1,4
            dels(2+jj)=-del4(jj)
         ENDDO
         CALL mulmav(v,6,6,dels,6,delemw)
c        WRITE(0,*)tpwea
c        WRITE(0,*)delemw
c linear map from ellipse
c        dxl=prscag(6,delemw,dtpdet(1,1))
c        dyl=prscag(6,delemw,dtpdet(1,2))
c        WRITE(0,*)dxl,dyl
c check 
c        CALL transp(dtpdet,6,2,dtpcde)
c        CALL mulmat(dtpcde,2,6,v,6,6,aa)
c        WRITE(0,*)' aa=',aa
c        CALL mulmat(aa,2,2,b,2,2,id)
c        WRITE(0,*)'id=',id
c compute 4 corners
         CALL vsumg(6,eqc,delems,elems(1,1))
         CALL vsumg(6,elems(1,1),delemw,elems(1,2))
         DO jj=1,6
            selemw(jj)=-delemw(jj)
            selems(jj)=-delems(jj)
         ENDDO
         CALL vsumg(6,elems(1,1),selemw,elems(1,1))
         CALL vsumg(6,eqc,selems,elems(1,3))
         CALL vsumg(6,elems(1,3),delemw,elems(1,4))
         CALL vsumg(6,elems(1,3),selemw,elems(1,3))
c give time
         WRITE(0,*)' give time for prediction (MJD)'
         READ(*,*)tmjd
c universal time of the required observation 
         mjd1=intlo(tmjd)
         sec1=(tmjd-float(mjd1))*86400.d0
         CALL cnvtim(mjd1,sec1,'TDT',mjd2,sec2,'UTC')
         tut=sec2/86400.d0+float(mjd2)
         ids=500
         iob1=1001
c predict
         CALL preobc('EQU',tc,ids,tmjd,eqc,hmag,gmag,gc,
     +        iob1,alpha,delta,appmag,gamad,sigsky,axesky)
         CALL outobc(iun20,iob1,ids,tut,alpha,delta,appmag,adot,ddot,
     +        elo,dis,2,gamad,sigsky,axesky)
         DO jj=1,4
           CALL preobs('EQU',tc,ids,tmjd,elems(1,jj),iob1,al(jj),de(jj),
     +           hmag,gmag,appmag)
            al(jj)=al(jj)-alpha
            de(jj)=de(jj)-delta
c           write(0,*)jj,al(jj)*degrad,de(jj)*degrad
         ENDDO
c ======four dimensional preimage===============
         menunam='prednonl'
         CALL menu(inl,menunam,3,'How to handle nonlinearity?=',
     +        'linear map=',
     +        '2-body nonlinearity=',
     +        'full n-body nonlinearity=',
     +        s4,s5,s6,s7,s8,s9,s10)
         IF(inl.eq.0)RETURN
c input specification of set of points
         CALL asscbd(iun20,npox,npo,sigma,ibv)   
c        sigma=5.d0
c        ibv=0
c        inl=1
c        npo=20
         CALL preob4(tc,ids,tmjd,eqc,hmag,gmag,gc,
     +        cc,v,sigma,npo,ibv,inl,al(5),de(5),appmagv,elm,
     +        alpha,delta,appmag,gamad,sigsky,axesky,npo1)
         al(5+npo1)=al(5)
         de(5+npo1)=de(5)
c        DO j=1,npo1
c           DO jj=1,6
c              delemw(jj)=elm(jj,j)-eqc(jj)
c           ENDDO
c linear map from 4-d axis
c           dxl=prscag(6,delemw,dtpdet(1,1))
c           dyl=prscag(6,delemw,dtpdet(1,2))
c           WRITE(0,*)' alternate no. ',j,dxl,dyl,delemw
c        ENDDO
         DO j=1,npo1+5
            write(0,*)'diff. observation ', j,al(j)*degrad,de(j)*degrad
         ENDDO
         titnam='virtual impactor' 
         CALL plocbd(titnam,alpha,delta,5.d0,tut,al,de,5+npo1,iob1)
      ENDIF
      GOTO 10
      END







