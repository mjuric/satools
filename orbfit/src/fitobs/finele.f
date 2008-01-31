c ===================================================================
c FINELE
c ===================================================================
c input of initial conditions
      SUBROUTINE finele(progna,iar,obs0,astna0,t0,eq0,h0,gma0,ini0,
     +      cov0,c,gam,iun20)
      IMPLICIT NONE
c ==========INPUT===================
c file names, i/o control 
c asteroid full name
      CHARACTER*18 astna0
c program name
      CHARACTER*6 progna
c output unit
      INTEGER iun20
c flag for arcs 1-2
      INTEGER iar
c logical flag: observations (hence name nam0) available
      LOGICAL obs0
c ==========OUTPUT==================
c epoch time (MJD), elements (equinoctal), absolute magnitude, opp.effect
      DOUBLE PRECISION eq0(6),t0,h0,gma0
c successful input flag 
      LOGICAL ini0 
c covariance matrices
      DOUBLE PRECISION c(6,6),gam(6,6)
      LOGICAL cov0
c =========END INTERFACE=============
c asteroid name for elements (18 characters)
      CHARACTER*18 namel0
      INTEGER lnam
c logical input flags
      LOGICAL ireq, found, fail, fail1
c file name for elements
      CHARACTER*60 elefi0
c unit numbers, length of names 
      INTEGER le,lench
c program name with dot
      CHARACTER*7 prognp
c variables for reading routines
      DOUBLE PRECISION elem(6)
      DOUBLE PRECISION mass
      CHARACTER*(80) comele
      include 'sunmass.h'

      CHARACTER*3 coox
      DOUBLE PRECISION enne
c ====================================
      lnam=lench(astna0)
c this arc does not exist
      IF(lnam.eq.0)RETURN
c ====================================================
c option names
      prognp=progna//'.'
c check that the asteroid name has been read
      fail=.false.
      ireq=.false.
      IF(iar.eq.1)THEN
         CALL rdncha(prognp,'namel0',namel0,ireq,found,
     +            fail1,fail)
      ELSEIF(iar.eq.2)THEN
         CALL rdncha(prognp,'namelp',namel0,ireq,found,
     +            fail1,fail)
      ENDIF
      IF(.not.found.or.fail)THEN
         namel0=astna0
         write(*,*)namel0
      ENDIF
      CALL rmsp(namel0,lnam)
c read the name of the elements file, inquire
      ireq=.false. 
      IF(iar.eq.1)THEN
         CALL rdncha(prognp,'elefi0',elefi0,ireq,found,
     +            fail1,fail)
      ELSEIF(iar.eq.2)THEN
         CALL rdncha(prognp,'elefip',elefi0,ireq,found,
     +            fail1,fail)
      ENDIF
      IF(.not.found.or.fail)THEN
c the arc exists, but the input elements file is not specified
         write(*,*)'elements file name not found, trying ''ast.cat'''
         write(iun20,*)'elem file name not found, trying ''ast.cat'''
         elefi0='ast.cat'
      ENDIF
      CALL rmsp(elefi0,le)
      INQUIRE(file=elefi0(1:le),exist=found)
      IF(found)THEN
         ini0=.false.
         CALL rdelem(iun20,namel0(1:lnam),1,elefi0(1:le),1,ini0,cov0,
     +        coox,t0,elem,gam,c,mass,h0,gma0,comele)
c error case
         IF(.not.ini0)THEN
            write(*,*)'asteroid ',namel0,' not found in ',elefi0
            write(iun20,*)'asteroid ',namel0,' not found in ',elefi0
            RETURN
         ENDIF
c set default for magnitude data
c         IF(h0.lt.-1.d6)THEN
c leave it            
c         ENDIF
         IF(gma0.lt.-1.d6)THEN
            gma0=0.15d0
         ENDIF
c coordinate change to equinoctal
         CALL coocha(elem,coox,gms,eq0,'EQU',enne)
c initial conditions found
         CALL wriequ(iun20,namel0,t0,eq0)
      ELSE
         WRITE(*,*) 'File ',elefi0(1:le),' not found!'
      ENDIF
      RETURN
      END
