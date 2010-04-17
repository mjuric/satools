c===============================
c CLOCMS
c close approach control
      SUBROUTINE clocms(idc,tt,xxpla)
      IMPLICIT NONE
      INTEGER idc
      DOUBLE PRECISION tt,xxpla
      include 'proout.h'
      include 'parbep.h'
      include 'masses.h'
      include 'iclap.h'
      IF(iorb.eq.11)THEN
         if(iclap.ne.0.and.idc.ne.0)then
* to be improved with a real safety feature
            write(99,*)'t =',tt,' close approach to=',ordnam(idc)
            write(iuncla,*)'t =',tt,' close approach to =',ordnam(idc)
         endif 
      ELSEIF(iorb.eq.9)THEN
         if(idc.ne.0)then
            write(99,*)'t =',tt,' close approach code=',idc
            write(iuncla,*)'t =',tt,' close approach code =',idc
         endif
      ENDIF
      RETURN
      END
