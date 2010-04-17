      SUBROUTINE selpert(name,found)
      IMPLICIT NONE
      LOGICAL found
      CHARACTER*(*) name
      CHARACTER*9 nam1
      CHARACTER*30 string
      INCLUDE 'parbep.h'
      INCLUDE 'combep.h'
      INCLUDE 'selast.h'
      INCLUDE 'bifina.h'
c controls of the force model
      include 'model.h'
c
      INTEGER iabe,ln,ls,ia,iat
c
      found=.false.
      IF(iast.eq.0)RETURN
c
      nam1=name
      CALL rmsp(nam1,ln)
      call filopn(iabe,filbec,'old')
c
      read(iabe,*)
      read(iabe,*)
      iat=0
      do  ia=1,iast
         read(iabe,201,err=202,end=202)masbep(ia),string
         call rmsp(string,ls)
         IF(ls.eq.ln.and.nam1(1:ln).eq.string(1:ls))THEN
            write(99,*)' self perturbation of ',nam1(1:ln),' avoided'         
            found=.true.
         ELSE
            iat=iat+1
            astid(iat)=ia
c           write(99,*) string(1:ls),' with mass OK'
         ENDIF
      enddo
      iatrue=iat
c     write(99,*)' iatrue ',iatrue, astid
      goto 203
 201  FORMAT(1P,E18.10,1X,A)
 202  write(99,*)'selpert: too many asteroids requested, iast=',iast
      iast=ia-1
      write(99,*)'selpert: asteroids available ',iast
 203  call filclo(iabe,' ')
      RETURN
      END
      SUBROUTINE selpert2(nam0,namp,nfound)
      IMPLICIT NONE 
      CHARACTER*(*) nam0,namp
      LOGICAL found0,foundp
      INTEGER nfound
      INCLUDE 'model.h'
      CALL selpert(nam0,found0)
      CALL selpert(namp,foundp)
      IF(found0.and.foundp)THEN
         write(99,*)' please do not try to identify ',nam0,' with ',namp
         write(99,*)' All perturbations by massive asteroids disabled'
         nfound=2
         iast=0
      ELSEIF(found0)THEN
         write(99,*)' you should not do identification with an'
         write(99,*)' asteroid with mass, such as ',nam0
         write(99,*)' perturbations by ',nam0,' disabled'
         nfound=1
      ELSEIF(foundp)THEN
         write(99,*)' you should not do identification with an'
         write(99,*)' asteroid with mass, such as ',namp
         write(99,*)' perturbations by ',namp,' disabled'
         nfound=1
      ELSE
         nfound=0
      ENDIF
      RETURN
      END

