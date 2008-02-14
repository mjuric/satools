c ================================================
c propagation of catalogs (multiline format) 
      program catpro
c ================================================
      implicit none
c ================================================
c variable dimensions, actual numbers
      integer nx
      parameter (nx=200000)
      CHARACTER*6 progna
      CHARACTER*7 prognp
      INTEGER iunlog, iunout
      CHARACTER*80 catnam0,catnam1,run
      CHARACTER*9 name
      CHARACTER*19 namid
      INTEGER ld
      CHARACTER*3 eltype
      CHARACTER*10 rsys,epoch
      DOUBLE PRECISION tref,t
      DOUBLE PRECISION eq(6),covar(6,6),normal(6,6),h,gmag,mass
      DOUBLE PRECISION ek(6),enne
      LOGICAL ireq,found,fail1,fail,ex,defcov,defnor,eof,covpro
      INTEGER l0,l1,icov,no,i,ndone
      INCLUDE 'proout.h'
c batch control
      INCLUDE 'comdif.h'
c ======== constant of gravitation ==============
      INCLUDE 'sunmass.h'
c ======== time spans for JPL data etc. =========
      INCLUDE 'timespan.h'
c ================================================
      call filopn(ipirip,'catpro.pro','unknown')
      call filopn(ierrou,'catpro.err','unknown')
      numerr=0
      call filopn(iuncla,'catpro.clo','unknown')
      numcla=0
      progna='catpro'
      prognp='catpro.'
      run=progna
c ================================================
c input options
      call cinopt(progna,run,iunlog,tref,catnam0,catnam1,covpro)
c check availability of JPL ephemrides
      IF(tref.lt.tejpl1.or.tref.gt.tejpl2)THEN
         WRITE(0,*)' JPL ephemerides not available for t=',t
         WRITE(0,*)' but only for interval ',tejpl1,tejpl2
         STOP
      ENDIF
c read the name of the elements file, inquire
      ireq=.true. 
      fail=.false.
      fail1=.false.
      CALL rdncha(prognp,'catnam0',catnam0,ireq,found,
     +     fail1,fail)
      CALL rmsp(catnam0,l0)
      INQUIRE(file=catnam0(1:l0),exist=ex)
      write(0,*)' file =',catnam0(1:l0),' exists=',ex,' found=',found,
     +      ' fail=',fail
      IF(.not.found.or.fail.or..not.ex)THEN
         write(0,*) found,fail,ex
         write(0,*) 'catalog not found:',catnam0(1:l0)
         STOP
      ENDIF
      CALL rdncha(prognp,'catnam1',catnam1,ireq,found,
     +     fail1,fail)
      CALL rmsp(catnam1,l1)
      write(0,*)' file =',catnam1(1:l1)
      IF(.not.found.or.fail)THEN
         write(0,*) 'catalog name not found:',catnam1(1:l1)
         STOP
      ENDIF
c batch operations to reduce output
      batch=.true.
c =============================================
c get reference system, etc. from old catalog to write into new catalog.
      CALL oporbf(catnam0(1:l0))
      CALL rdorb(namid,eq,eltype,t,covar,defcov,normal,defnor,h,gmag,
     +     mass,rsys,epoch,no,eof)
      CALL clorbf
c =====================================================================
c propagate with covariance if available from input file
      IF(covpro)THEN
         icov=2
      ELSE
         icov=1
      ENDIF 
c open new cat and write header
      call filopn(iunout,catnam1(1:l1),'unknown')
      IF(icov.eq.2)THEN
         CALL wromlh(iunout,rsys,epoch)
      ELSE
         CALL wro1lh(iunout,rsys,epoch,'KEP')
      ENDIF
c =======================================================================
c Begin main loop
      CALL oporbf(catnam0(1:l0))
      ndone=0
      DO 10 i=1,nx
c input orbit at common epoch, with covariance    
         REWIND (ipirip)
c        WRITE(0,*) 'Reading no. ',i
         CALL rdorb(namid,eq,eltype,t,covar,defcov,normal,defnor,h,gmag,
     +        mass,rsys,epoch,no,eof)
         IF(eof) GOTO 5
c convert name in case it is of the form nam0=namp
         ld=index(namid,'=')-1
         IF(ld.lt.0)THEN
            name=namid(1:9)
         ELSE
            name=namid(1:ld)
         ENDIF
c check availability of JPL ephemrides
         IF(t.lt.tejpl1.or.t.gt.tejpl2)THEN
            WRITE(0,*)' JPL ephemerides not available for t=',t
            WRITE(0,*)' but only for interval ',tejpl1,tejpl2
            GOTO 10
         ENDIF
c check availability of covariance and normal matrices
         IF(icov.eq.2)THEN
            IF(.not.defcov.or.(.not.defnor))THEN
               WRITE(0,*)' matrix missing for ',name,defcov,defnor
               GOTO 10
            ENDIF
         ENDIF
c ================================================================
c propagation to time tref
         IF(icov.eq.1)THEN
c state vector only
            CALL proele(eltype,t,eq,tref,eq)
            defcov=.false.
         ELSEIF(icov.eq.2)THEN
            CALL proelc(eltype,t,eq,covar,normal,tref,eq,covar,normal)
         ENDIF 
c write output in multiline format (if covariance is required), 
c single line format otherwise. 
         IF(icov.eq.2)THEN
            CALL wromlr(iunout,name,eq,eltype,tref,covar,defcov,
     +                  normal,defnor,h,gmag,mass)
         ELSE
c output single line catalog record
              CALL coocha(eq,eltype,gms,ek,'KEP',enne)
              CALL wro1lr(iunout,name,ek,'KEP',tref,h,gmag)
         ENDIF
         ndone=ndone+1
 10   ENDDO
      WRITE(0,*) 'CANNOT COMPLETE CATALOG, nx=', nx
c     end of input file
 5    CALL clorbf
      i=i-1
      WRITE(0,*)' total number of orbits ',i,' propagated ',ndone
      stop
      end




