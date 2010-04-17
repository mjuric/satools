c =====================================================================
c  OUTMUL
c =====================================================================
c output multiple observations
c =============INTERFACE===============================================
      SUBROUTINE outmul(titnam,filnam,t1,sigma,alpha,delta,
     +     alm,dem,hmagn,imim,imip,imi0,iff,aobs,dobs,iobs)
      IMPLICIT NONE
c =============INPUT===================================================
c file name
      CHARACTER*80 titnam
      CHARACTER*60 filnam
c first and last index of existing multiple solutions, 
c index of reference one, control for closed curve,obs type
      INTEGER imim,imip,imi0,iff,iobs
c observation time MJD, sigma value, nominal prediction
      DOUBLE PRECISION t1,sigma,alpha,delta
c observation: predicted  value alpha, delta, magnitude, actual
      DOUBLE PRECISION alm(imip),dem(imip),hmagn(imip),aobs,dobs
c =============END INTERFACE===========================================
c multiple data for confidence boundary
      INCLUDE 'npoint.h'
c trig constants
      INCLUDE 'trig.h'
c max no. orbits
      INCLUDE 'parmul.h'
c conversion to sessagesimal
      DOUBLE PRECISION seca,secd
      INTEGER inta,mina,intd,mind
      CHARACTER*1 siga,sigd
c conversion of time
      INTEGER iy,imo,iday
      DOUBLE PRECISION hour
c scalar temporaries, differences
      DOUBLE PRECISION dee,daa,ado,ddo
c file name
      INTEGER le
      CHARACTER*80 file
c loop indexes, units
      INTEGER n,iun7
c ======================================================================
c open output file
      CALL rmsp(filnam,le)
      file=filnam(1:le)//'.cbd'
      CALL filopn(iun7,file,'unknown')
c date and sigma value
      CALL mjddat(t1,iday,imo,iy,hour)
      WRITE(iun7,297)iday,imo,iy,hour,sigma
 297  FORMAT(i3,i3,i5,f8.4,f5.2)
c line of variations
      DO n=imim,imip
        daa=alpha+alm(n)
        daa=mod(daa,dpig)
        IF(daa.lt.0.d0)daa=daa+dpig
        IF(daa.gt.dpig)daa=daa-dpig
        daa=daa*degrad/15
        IF(daa.lt.0.d0.or.daa.gt.24.d0)THEN
           write(99,*)' outmul: daa out of range ', daa
        ENDIF
        CALL sessag(daa,siga,inta,mina,seca)
        dee=(delta+dem(n))*degrad
        CALL sessag(dee,sigd,intd,mind,secd)
c proper motion in arcsec/hour
        ado=adotv(n)*secrad/24.d0
        ddo=ddotv(n)*secrad/24.d0
c output
        IF(siga.eq.'+')siga=' '
        WRITE(iun7,396)n,siga,inta,mina,seca,sigd,intd,mind,secd,
     +       disv(n),ado,ddo,hmagn(n)
 396    FORMAT(i3,1x,a1,i2,1x,i2,1x,f4.1,2x,a1,i2,1x,i2,1x,f4.1,
     +       1x,f8.5,1x,f8.2,1x,f8.2,1x,f5.2)
      ENDDO
      CALL filclo(iun7,' ')
c graphics output
      IF(iff.eq.1)THEN
         CALL plocbd(titnam,alpha,delta,sigma,t1,
     +         alm(imim),dem(imim),imip-imim+1,iobs)
      ELSEIF(iff.eq.2)THEN
         CALL ploobs(titnam,alpha,delta,sigma,t1,
     +         alm(imim),dem(imim),imip-imim+1,
     +                 aobs,dobs)
      ENDIF
      RETURN
      END





