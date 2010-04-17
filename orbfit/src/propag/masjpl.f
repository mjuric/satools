c ================================
c MASJPL 
c
c version 1.4, A. Milani, Nov. 10, 1997
c 1.5.2 corrected January 3, 1997
c  input of planetary masses from jpl header
c  it assumes that dpleph has already been called
c ================================
      subroutine masjpl
      implicit none
c controls of the force model
      include 'model.h'
c controls of close approaches monitoring
      include 'iclap.h'
c name of asteroid binary file
      include 'bifina.h'
c asteroid parameter and common
      include 'parbep.h'
      include 'combep.h'
      include 'selast.h'
c planetary masses, constants, etc.
      include 'masses.h'
c ======== constant of gravitation ==============
      include 'sunmass.h'
      double precision gmerc,gm1,gmsun
c ======== JPL EPHEM ============================
c  jpl header data read by dpleph and subr.  
      character*6 cnam(400),ttl(14,3)
      common/chrhdr/cnam,ttl
c standard JPL header
      include 'jplhdr.h'
c ===============================================
c  loop indexes, lengths, output units
      integer m,mm,ia,i,j,ln,iabe
      logical openfl
c functions
      integer lench
c  strings with asteroid names
      character*30 astnam(nbepx)
      character*30 string
      integer mlun,mea,mjup,mnep
c  strings with planet names
      character*30 nomi(11)
      data nomi/'MERCURY','VENUS','EARTH_MOON','MARS',
     + 'JUPITER','SATURN','URANUS','NEPTUNE','PLUTO','MOON','EARTH'/

c      LOGICAL first
c      SAVE first,iabe
c      DATA first/.true./

c  number of planets in our integration
c  also sets the distance for close approaches
      npla=7
      if(imerc.gt.0)then
         npla=npla+1
         m=0
         mlun=9
         mea=3
         mjup=5
         mnep=8
      else
         m=1
         mlun=8
         mea=2
         mjup=4
         mnep=7
      endif
      if(iplut.gt.0)then
         npla=npla+1
         mlun=mlun+1
      endif
c  distance defining a close approach: choise in model.opt
      do 39 j=1,npla
        dmin(j)=dmea
c let dist for mercury be smaller by factor two.
        IF(imerc.gt.0.and.j.eq.1)dmin(j)=dmin(j)/2.d0
        if(j.ge.mjup.and.j.le.mnep)dmin(j)=dmjup
 39   continue
c
c  gms in solar masses
      gmsun=cval(18)
c  flags to require data
      do  mm=1,12
        listpl(mm)=0
      enddo
c  setup of the planetary masses
      do 40 mm=1,npla
          m=m+1
          gm(mm)=cval(8+m)/gmsun
          ordnam(mm)=nomi(m)
          if(m.eq.3)then
             itarg(mm)=13
          else
             itarg(mm)=m
          endif
          listpl(m)=2
 40   continue
c store mass of Earth-Moon plus Sun (in solar masses)
      gmse=(gmsun+cval(11))/gmsun
c  store mercury/sun mass ratio; Mercury position is needed anyway, 
c to use Sun-Mercury center of mass
      if(imerc.eq.0)then
          gmerc=cval(9)/gmsun
          listpl(1)=2
      endif
c  the moon as possible additional massive body (then replace E+M center
c  of mass with E).
      if(ilun.gt.0)then
         npla=npla+1
c  close approaches to the Moon are not reported separately 
c  from close approaches to Earth
         dmin(mlun)=dmoon
c  mass of the Moon is computed from Earth/Moon mass ratio
         emrat=cval(8)
         gm(mlun)=cval(11)/((1.d0+emrat)*gmsun)
         ordnam(mlun)=nomi(10)
         itarg(mlun)=10
         listpl(10)=2
c  if the Moon is included, the Earth is included as a single body
c  and not as E-M center of mass
         gm(mea)=cval(11)/((1.d0+1.d0/emrat)*gmsun)
         ordnam(mea)=nomi(11)
         itarg(mea)=3
         listpl(3)=2
      endif
c
      if(iast.ne.0)then
c part relative to massives asteroids
c read names of asteroids and asteroid masses from ascii file 'CPV.abe'
        call filopn(iabe,filbec,'old')
c       inquire(file=filbec,opened=openfl)
c       if(.not.openfl) then
c        IF(first) THEN
c            call filopn(iabe,filbec,'old')
c            write(99,*) 'opening filbec'
c            first=.false.
c        endif
        read(iabe,*)
        read(iabe,*)
        do  ia=1,iast
            read(iabe,201,err=202,end=202)masbep(ia),string
            ln=lench(string)
            astnam(ia)=string(1:ln)
c            astid(ia)=ia
        enddo
c        iatrue=iast
        goto 203
 201    FORMAT(1P,E18.10,1X,A)
 202    write(99,*)'masjpl: too many asteroids requested, iast=',iast
        iast=ia-1
        write(99,*)'masjpl: asteroids available ',iast
c203    close(iabe)
 203    call filclo(iabe,' ')
c  asteroid close approach distance
c  temporary choice: 0.2 AU
        do 13 ia=1,iast
 13        dmin(npla+ia)=dmast
        do 11 ia=1,iatrue
c  asteroid names
           ordnam(npla+ia)=astnam(astid(ia))
c  asteroid masses (unita` masse solari)
 11        gm(npla+ia)=masbep(astid(ia))
      endif
c  gmsun in the chosen units (au, day)
      gm0=gms
      nmass=npla+iatrue
      do 12 i=1,nmass
        gm(i)=gm0*gm(i)
 12   continue
      gmse=gmse*gm0
c  if mercury is not included, its mass is included in the mass of the sun
      if(imerc.eq.0) then
         gmerc=gmerc*gm0
         gm1=gm0+gmerc
         gmu=gmerc
         gm0=gm1
      endif

      return
      end




