c ==================================================================
c yarkinit: initialisation of the yarkovsky force model for a given asteroid
c           written by A. Milani & D. Vokrouhlicky, Oct 99
      SUBROUTINE yarkinit(astnam,eltype,elem)
      IMPLICIT NONE
      CHARACTER*(*) astnam
      CHARACTER*3 eltype
      DOUBLE PRECISION elem(6)
      CHARACTER*80 file
      DOUBLE PRECISION lat,long,emiss,stefboltz,argu,argu2,tstarya,eta
      DOUBLE PRECISION elkep(6),pvya(3),qvya(3),nvya(3),enne,cgam,obli
      INTEGER unit,le
      INCLUDE 'yarkov.h'  
      INCLUDE 'yarkom.h'
      INCLUDE 'trig.h'
      INCLUDE 'model.h'
      INCLUDE 'sunmass.h'
      INCLUDE 'sysdep.h'
c yar is the logical flag for the existence of the physical data 
c allowing computation of Yarkovsky; otherwise, the non gravitational
c force is set to zero 
c
      IF(iyark.eq.0)RETURN
      yarini=.true.
c convert elements to keplerian
      call coocha(elem,eltype,gms,elkep,'KEP',enne)
c compute the name of the file which could contain the yarkovsky data
      CALL filnam(yardir,astnam,'yar',file,le)
      INQUIRE(file=file(1:le),exist=yarfil)
      IF(yarfil)THEN
         call filopn(unit,file(1:le),'old')
         read(unit,*,end=111)
c ecliptic latitude and longitude of the spin axis
         read(unit,*,end=111)long
         read(unit,*,end=111)lat
c - via common   yarkp(1-3) ... sx, sy, sz (unit vector of the
c                               body's spin axis orientation)
c                yarkp(4-5) ... k_0 and k_1 parameters of the
c                               surface thermal conductivity
c                               [K(T) = k_0 + k_1 T_av^3]
c                yarkp(6) ... density of the surface layer
c                yarkp(7) ... radius of the body
c                yarkp(8) ... rotation frequency
c                yarkp(9) ... surface absorptivity
         yarkp(1)=dcos(lat*radeg)*dcos(long*radeg)
         yarkp(2)=dcos(lat*radeg)*dsin(long*radeg)
         yarkp(3)=dsin(lat*radeg)
         read(unit,*,end=111)yarkp(4)
         read(unit,*,end=111)yarkp(5)
         read(unit,*,end=111)yarkp(6)
         read(unit,*,end=111)yarkp(7)
         read(unit,*,end=111)yarkp(8)
         read(unit,*,end=111)yarkp(9)
c precompute some variables for the seasonal variant of the Yarkovsky
c effect:
c - constants
         emiss=0.9d0
         stefboltz=5.66962d-8
c - mean motion & solar radiation flux at r=a
         fmeaya=(1.9909837d-7)/elkep(1)/dsqrt(elkep(1))
         radfluya=1371.d0/elkep(1)/elkep(1)
c - subsolar temperature
         tstarya=(yarkp(9)*radfluya/emiss/stefboltz)**0.25d0
         thfacya=dsqrt(fmeaya)/stefboltz/(tstarya**3)
c - projections s_P and s_Q of the spin axis computed
         pvya(1)=dcos(elkep(4))*dcos(elkep(5))-
     .           dcos(elkep(3))*dsin(elkep(4))*dsin(elkep(5))
         pvya(2)=dsin(elkep(4))*dcos(elkep(5))+
     .           dcos(elkep(3))*dcos(elkep(4))*dsin(elkep(5))
         pvya(3)=dsin(elkep(3))*dsin(elkep(5))
         qvya(1)=-dcos(elkep(4))*dsin(elkep(5))-
     .           dcos(elkep(3))*dsin(elkep(4))*dcos(elkep(5))
         qvya(2)=-dsin(elkep(4))*dsin(elkep(5))+
     .         dcos(elkep(3))*dcos(elkep(4))*dcos(elkep(5))
         qvya(3)=dsin(elkep(3))*dcos(elkep(5))
         nvya(1)=dsin(elkep(3))*dsin(elkep(4))
         nvya(2)=-dsin(elkep(3))*dcos(elkep(4))
         nvya(3)=dcos(elkep(3))
         spya=yarkp(1)*pvya(1)+yarkp(2)*pvya(2)+yarkp(3)*pvya(3)
         sqya=yarkp(1)*qvya(1)+yarkp(2)*qvya(2)+yarkp(3)*qvya(3)
         cgam=yarkp(1)*nvya(1)+yarkp(2)*nvya(2)+yarkp(3)*nvya(3)
         obli=dacos(cgam)/radeg
         write(0,*)' Obliquity of the spin axis; Yarkovsky: ',obli
c - compute the \alpha(k) and \beta(k) coefficients
         eta=dsqrt(1.d0-elkep(2)*elkep(2))
         etaya75=eta**0.75d0
c -- \beta_1(x) ... \beta_7(x) functions
         argu=elkep(2)
         argu2=argu*argu
         beya(1)=eta*(1.d0+argu2*(-1152.d0+argu2*(48.d0-argu2))/9216.d0)
         argu=2.d0*elkep(2)
         argu2=argu*argu
         beya(2)=eta*argu*(1.d0+argu2*(-1920.d0+argu2*(60.d0-argu2))
     .           /23040.d0)
         argu=3.d0*elkep(2)
         argu2=argu*argu
         beya(3)=3.d0*eta*argu2*(1.d0+argu2*(-40.d0+argu2)/640.d0)/8.d0
         argu=4.d0*elkep(2)
         argu2=argu*argu
         beya(4)=eta*argu2*argu*(1.d0+argu2*(-48.d0+argu2)/960.d0)/12.d0
         argu=5.d0*elkep(2)
         argu2=argu*argu
         beya(5)=5.d0*eta*argu2*argu2*(1.d0-argu2/24.d0)/384.d0
         argu=6.d0*elkep(2)
         argu2=argu*argu
         beya(6)=eta*argu2*argu2*argu*(1.d0-argu2/28.d0)/640.d0
         argu=7.d0*elkep(2)
         argu2=argu*argu
         beya(7)=7.d0*eta*argu2*argu2*argu2/46080.d0
c -- \alpha_1(x) ... \alpha_7(x) functions
         argu=elkep(2)
         argu2=argu*argu
         alya(1)=1.d0+argu2*(-3456.d0+argu2*(240.d0-7.d0*argu2))/9216.d0
         argu=2.d0*elkep(2)
         argu2=argu*argu
         alya(2)=argu*(1.d0+argu2*(-960.d0+argu2*(45.d0-argu2))/5760.d0)
         argu=3.d0*elkep(2)
         argu2=argu*argu
         alya(3)=3.d0*argu2*(1.d0+argu2*(-200.d0+7.d0*argu2)/1920.d0)/
     .           8.d0
         argu=4.d0*elkep(2)
         argu2=argu*argu
         alya(4)=argu*argu2*(1.d0+argu2*(-36.d0+argu2)/480.d0)/12.d0
         argu=5.d0*elkep(2)
         argu2=argu*argu
         alya(5)=argu2*argu2*(1.d0-7.d0*argu2/120.d0)/76.8d0
         argu=6.d0*elkep(2)
         argu2=argu*argu
         alya(6)=argu*argu2*argu2*(1.d0-argu2/21.d0)/640.d0
         argu=7.d0*elkep(2)
         argu2=argu*argu
         alya(7)=7.d0*argu2*argu2*argu2/46080.d0
c close the input file
         call filclo(unit,' ')
      ELSE
         WRITE(0,*)' Yarkovsky datafile not found:',file(1:le)
         stop     
      ENDIF
      WRITE(0,*)' Yarkovsky data loaded for asteroid ', astnam
      RETURN
 111  yarfil=.false.
      WRITE(0,*)' incomplete yarkovsky file for asteroid ', astnam
      RETURN
      END
