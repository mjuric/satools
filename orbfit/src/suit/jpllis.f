* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: October 13, 1997
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         J P L L I S                           *
*  *                                                               *
*  *      Get the list of masses and IDs from JPL DE header        *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    NAMES     -  Planet names
*           N         -  Number of planets
*
* OUTPUT:   GMP       -  G*M(planet)
*           ID        -  Planet ID number
*           FAIL      -  Error flag
*
      SUBROUTINE jpllis(names,n,gmp,id,fail)
      IMPLICIT NONE

      INTEGER n
      CHARACTER*(*) names(n)
      DOUBLE PRECISION gmp(n)
      INTEGER id(n)
      LOGICAL fail

      DOUBLE PRECISION et2(2),pv(6,12),pnut(4)
      INTEGER list(12),i,ln,emopt
      CHARACTER*12 n1
      LOGICAL emwarn

* JPLDE header
      DOUBLE PRECISION cval(400),ss(3),au,emrat
      INTEGER denum,ncon,ipt(3,13),lpt(3)
      CHARACTER*6 cnam(400),ttl(14,3)
      COMMON/ephhdr/cval,ss,au,emrat,denum,ncon,ipt,lpt
      COMMON/chrhdr/cnam,ttl

      INTEGER lench
      EXTERNAL lench

      DATA et2/2*0.d0/
      DATA list/12*0/

* Dummy call to STATE for reading JPLDE header
      CALL state(et2,list,pv,pnut,1)

* Choice for Earth/Moon: 1=barycenter, 2=distinct bodies
      emopt=0
      emwarn=.false.
      DO 1 i=1,n
      n1=names(i)
      CALL upcase(n1)
      IF(n1.EQ.'MERCURY') THEN
          id(i)=1
          gmp(i)=cval(9)
          IF(cnam(9).NE.'GM1')
     +        STOP '**** jpllis: internal error (C01) ****'
      ELSEIF(n1.EQ.'VENUS') THEN
          id(i)=2
          gmp(i)=cval(10)
          IF(cnam(10).NE.'GM2')
     +        STOP '**** jpllis: internal error (C02) ****'
      ELSEIF(n1.EQ.'EARTH') THEN
          id(i)=3
          gmp(i)=cval(11)*emrat/(1+emrat)
          IF(cnam(11).NE.'GMB')
     +        STOP '**** jpllis: internal error (C03) ****'
          IF(emopt.NE.0 .AND. emopt.NE.2) emwarn=.true.
          emopt=2
      ELSEIF(n1.EQ.'MOON') THEN
          id(i)=10
          gmp(i)=cval(11)/(1+emrat)
          IF(cnam(11).NE.'GMB')
     +        STOP '**** jpllis: internal error (C10) ****'
          IF(emopt.NE.0 .AND. emopt.NE.2) emwarn=.true.
          emopt=2
      ELSEIF(n1.EQ.'EARTH+MOON') THEN
          id(i)=13
          gmp(i)=cval(11)
          IF(cnam(11).NE.'GMB')
     +        STOP '**** jpllis: internal error (C13) ****'
          IF(emopt.NE.0 .AND. emopt.NE.1) emwarn=.true.
          emopt=1
      ELSEIF(n1.EQ.'MARS') THEN
          id(i)=4
          gmp(i)=cval(12)
          IF(cnam(12).NE.'GM4')
     +        STOP '**** jpllis: internal error (C04) ****'
      ELSEIF(n1.EQ.'JUPITER') THEN
          id(i)=5
          gmp(i)=cval(13)
          IF(cnam(13).NE.'GM5')
     +        STOP '**** jpllis: internal error (C05) ****'
      ELSEIF(n1.EQ.'SATURN') THEN
          id(i)=6
          gmp(i)=cval(14)
          IF(cnam(14).NE.'GM6')
     +        STOP '**** jpllis: internal error (C06) ****'
      ELSEIF(n1.EQ.'URANUS') THEN
          id(i)=7
          gmp(i)=cval(15)
          IF(cnam(15).NE.'GM7')
     +        STOP '**** jpllis: internal error (C07) ****'
      ELSEIF(n1.EQ.'NEPTUNE') THEN
          id(i)=8
          gmp(i)=cval(16)
          IF(cnam(16).NE.'GM8')
     +        STOP '**** jpllis: internal error (C08) ****'
      ELSEIF(n1.EQ.'PLUTO') THEN
          id(i)=9
          gmp(i)=cval(17)
          IF(cnam(17).NE.'GM9')
     +        STOP '**** jpllis: internal error (C09) ****'
      ELSE
          ln=lench(names(i))
          write(99,100) names(i)(1:ln)
          fail=.true.
      END IF
 100  FORMAT(' ERROR: ',A,' is unknown among JPL planets')
 1    CONTINUE

      IF(emwarn) THEN
          write(99,101)
          fail=.true.
      END IF
 101  FORMAT(' ERROR in the list of JPL planets: please DO NOT select'/
     +       '       "Earth+Moon" (Earth-Moon barycenter) together'/
     +       '       with "Earth" and/or "Moon"')

      END
