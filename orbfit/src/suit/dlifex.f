* Copyright (C) 1996 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: August 27, 1996
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         D L I F E X                           *
*  *                                                               *
*  *                Delete a file (if it exists)                   *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    FILE      -  Name of the file to be deleted
*
      SUBROUTINE dlifex(file)
      IMPLICIT NONE

      CHARACTER*(*) file

      INTEGER unit
      LOGICAL found

      INQUIRE(FILE=file,EXIST=found)
      IF(found) THEN
          CALL filopn(unit,file,'old')
          CALL filclo(unit,'delete')
      END IF

      END
