c =====================================================================
c  Input first guess from identif
      SUBROUTINE iniide(nam0,namp,eqide,tide,inide,iun20)
      implicit none
      character*6 nam0,namp
      double precision eqide(6),tide
      integer iun20,iun3
      logical inide
c ======END INTERFACE====================================================
      logical found,ireq,fail1,fail
      character*60 elefide
      ireq=.false.
      call rdncha('fitobs.','elefide',elefide,ireq,found,
     +            fail1,fail)
      if(.not.found)then
         inide=.false.
         write(0,*)' ***** identif ele. file name not found ***'
         return
      endif
      INQUIRE(file=elefide,exist=inide)
      if(inide)then
         CALL filopn(iun3,elefide,'OLD')
         CALL inasti(iun3,nam0,namp,eqide,tide,inide)
         CALL filclo(iun3,' ')
         if(.not.inide)then
            write(0,*)'proposed identification ',nam0,' with ',namp
            write(0,*)'not found in',elefide
         endif
         write(iun20,*)' proposed starting value from ',elefide
         write(0,*)' proposed starting value from identif'
         write(iun20,208)nam0,namp,tide
         write(0,208)nam0,namp,tide
 208    format(a6,1x,a6,' ident. elem (a,h,k,p,q,lam), epoch=',f8.1)
         write(iun20,104) eqide
         write(0,104) eqide
 104     format(6f13.7)
      else
         write(0,*)'file ',elefide,' not found'       
      endif 
      return
      end
*****************************************************************
*
*  INASTI
*
*  INPUT  : iun  = unit file
*           nam0,namp = two asteroid names (6 characters)
*
*  OUTPUT : eq   = vector of equinoctial elements of identified asteroid
*           ta   = epoch time of the elements  
*           inide= .true. if found
*****************************************************************
      subroutine inasti(iun,nam0,namp,eq,ta,inide)
      implicit none
      double precision eq(6),ta
      character*6 nam1,nam2,nam0,namp
      integer nidex,i,nrev,iun
      logical inide
      double precision deltaq,deltalt,dista,prob
      parameter (nidex=1000)
      ta=50500.d0
      do 1 i=1,nidex
c  read first guess file
         read(iun,103,end=2)deltaq,deltalt,dista,prob,nam1,nam2,nrev,eq
 103     format(2(f19.6,1x),f14.10,1x,d12.3,1x,a6,1x,a6,i3,
     +                  1p,6d20.12)
         if(nam0.eq.nam1.and.namp.eq.nam2)then
            inide=.true.
            return
         endif
 1    continue
 2    write(0,*)' identification ',nam0,' = ',namp,' not found'
      inide=.false.
      return
      end

