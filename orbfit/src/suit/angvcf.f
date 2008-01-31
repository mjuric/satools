* Copyright (C) 2000 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: November 30, 2000
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         A N G V C F                           *
*  *                                                               *
*  *           Conversion factor for angular velocities            *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    UNIT      -  Description of unit
*
* OUTPUT:   CF        -  Conversion factor from rad/d to "unit"
*           ERROR     -  Error flag
*
      SUBROUTINE angvcf(unit,cf,error)
      IMPLICIT NONE

      CHARACTER*(*) unit
      DOUBLE PRECISION cf
      LOGICAL error

      INCLUDE 'trig.h'

      DOUBLE PRECISION ct
      CHARACTER*20 unang,untim
      LOGICAL nospli

      error=.true.

* Input string must be of the form 'unang/untim',
* where: "unang" is the angular unit (rad/deg/arcmin/arcsec)
*        "untim" is the time unit (d/h/min/s);
* for instance: 'deg/d' or 'arcsec/min'
      untim=unit
      CALL stspli(untim,'/',unang,nospli)
      IF(nospli) RETURN
      CALL locase(unang)
      CALL locase(untim)

* List of supported angular units
      IF(unang.EQ.'rad') THEN
          cf=1
      ELSEIF(unang.EQ.'deg') THEN
          cf=degrad
      ELSEIF(unang.EQ.'arcmin') THEN
          cf=degrad*60
      ELSEIF(unang.EQ.'''') THEN
          cf=degrad*60
      ELSEIF(unang.EQ.'arcsec') THEN
          cf=degrad*3600
      ELSEIF(unang.EQ.'"') THEN
          cf=degrad*3600
      ELSE
          RETURN
      END IF

* List of supported time units
      IF(untim.EQ.'d') THEN
          ct=1
      ELSEIF(untim.EQ.'day') THEN
          ct=1
      ELSEIF(untim.EQ.'h') THEN
          ct=24
      ELSEIF(untim.EQ.'hour') THEN
          ct=24
      ELSEIF(untim.EQ.'min') THEN
          ct=24*60
      ELSEIF(untim.EQ.'s') THEN
          ct=24*3600
      ELSE
          RETURN
      END IF
      cf=cf/ct
      error=.false.

      END
