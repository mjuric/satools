      subroutine fszer2(nrecl,ksize,nrfile,namfil)
c
c++++++++++++++++++++++++
c  this subroutine opens the file, 'namfil', with a phony record length, reads 
c  the first record, and uses the info to compute ksize, the number of single 
c  precision words in a record.  
c
c  the subroutine also sets the values of  nrecl, nrfile, and namfil.

* changed by sabrina baccili on Wed Oct 30
*      save

      implicit double precision(a-h,o-z)
      character*6 ttl(14,3),cnam(400)
      character*(*) namfil
      character*150 namtmp
      logical found
c     logical fail,fail1

      dimension ss(3)

      integer ipt(3,12),lpt(3)
      save
* end change

* NEEDED common blocks:
      INCLUDE 'comlib.h'

c  *****************************************************************
c  *****************************************************************
c
c  the parameters nrecl, nrfile, and namfil are to be set by the user
c
c  *****************************************************************

c  nrecl=1 if "recl" in the open statement is the record length in s.p. words
c  nrecl=4 if "recl" in the open statement is the record length in bytes
c  (for unix, it is probably 4)
c
      nrecl=4

      IF(iiclib.NE.36) STOP '**** fszer2: internal error (01) ****'
c changed by A. Milani (October 24, 1998)
c  binary ephemeris file: old method to search for it with a name namfil
c was used in bineph, now incompatibele with fitobs/orbfit
c      fail=.false.
c     CALL rdncha('JPLDE.','file',namfil,.false.,found,fail1,fail)
c      IF(fail1) STOP '**** fszer2: abnormal end ****'
c      IF(.NOT.found) THEN
c  jpleph is alwaysthe external name of the binary ephemeris file
          namfil='jpleph'
          INQUIRE(FILE=namfil,EXIST=found)
          IF(found) GOTO 2
          namtmp=libdir(1:lenld)//namfil
          namfil=namtmp
          INQUIRE(FILE=namfil,EXIST=found)
          IF(found) GOTO 2
          GOTO 10
c      END IF
 2    CONTINUE
c  nrfile is the internal unit number used for the ephemeris file
      CALL filass(nrfile,namfil)
* end of change

c  *****************************************************************
c  *****************************************************************

c  **  open the direct-access file and get the pointers in order to 
c  **  determine the size of the ephemeris record

      mrecl=nrecl*1000

        open(nrfile,
     *       file=namfil,
     *       access='direct',
     *       form='unformatted',
     *       recl=mrecl,
     *       status='old',
     *       err=10)

      read(nrfile,rec=1)ttl,cnam,ss,ncon,au,emrat,ipt,numde,lpt

      close(nrfile)

c  find the number of ephemeris coefficients from the pointers

      kmx = 0
      khi = 0

      do 1 i = 1,12
         if (ipt(1,i) .gt. kmx) then
            kmx = ipt(1,i)
            khi = i
         endif
 1    continue
      if (lpt(1) .gt. kmx) then
          kmx = lpt(1)
          khi = 13
      endif

      nd = 3
      if (khi .eq. 12) nd=2

      if(khi.eq.13) then
          ksize = 2*(lpt(1)+nd*lpt(2)*lpt(3)-1)
      else
          ksize = 2*(ipt(1,khi)+nd*ipt(2,khi)*ipt(3,khi)-1)
      endif

      return

 10   continue
      lf=lench(namfil)
      write(99,100) namfil(1:lf)
 100  FORMAT('ERROR opening file ',A)
      STOP '**** fszer2: abnormal end ****'

      end
