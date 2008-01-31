* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: December 15, 1997
* Modified November 9, 1998 by Steven Chesley (chesley@dm.unipi.it)
* in order to handle three digit subscripts in IAU codes.
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         M P C P D S                           *
*  *                                                               *
*  * Computes MPC-style packed designation from official IAU code  *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    IAUCO    -  IAU code
*
* OUTPUT:   MPCCOD    -  MPC-style packed code
*           ERROR     -  Error flag (cannot understand input code)
*
      SUBROUTINE mpcpds(iauco,mpccod,error)
      IMPLICIT NONE

      CHARACTER*(*) iauco,mpccod
      LOGICAL error

      INTEGER ln,nd,i,head
      CHARACTER tsn*1

      INTEGER lench
      LOGICAL isnum,islett
      EXTERNAL lench,isnum,islett

      error=.false.
      mpccod=' '

      ln=lench(iauco)
      IF(ln.LE.0) GOTO 10

* Numbered asteroids
      IF(isnum(iauco(1:ln)) .AND. ln.LE.5) THEN
           DO 1 i=1,5-ln
           mpccod(i:i)='0'
 1         CONTINUE
           mpccod(5-ln+1:5)=iauco(1:ln)
           RETURN
      END IF

* Asteroid provisional designations (e.g., 1982QB1)
      IF(ln.LT.6 .OR. ln.GT.9) GOTO 2
      IF(.NOT.isnum(iauco(1:4))) GOTO 2
      IF(.NOT.islett(iauco(5:6))) GOTO 2
      nd=ln-6
      IF(nd.GT.0) THEN
          IF(.NOT.isnum(iauco(7:ln))) GOTO 2
      END IF

      IF(iauco(1:2).EQ.'19') THEN
          mpccod(1:1)='J'
      ELSEIF(iauco(1:2).EQ.'20') THEN
          mpccod(1:1)='K'
      ELSEIF(iauco(1:2).EQ.'18') THEN
          mpccod(1:1)='I'
      ELSE
          GOTO 2
      END IF

      mpccod(2:4)=iauco(3:5)
      IF(nd.EQ.0) THEN
          mpccod(5:6)='00'
      ELSEIF(nd.EQ.1) THEN
          mpccod(5:5)='0'
          mpccod(6:6)=iauco(7:7)
      ELSEIF(nd.EQ.2) THEN
          mpccod(5:6)=iauco(7:8)
      ELSEIF(nd.EQ.3) THEN
          read(iauco,103) head
 103      format(6x,i2)
          mpccod(5:5)=char(head+55)
          mpccod(6:6)=iauco(9:9)
      ELSE
          GOTO 2
      END IF
      mpccod(7:7)=iauco(6:6)
      RETURN

 2    CONTINUE

* Palomar-Leiden survey
      i=index(iauco(1:ln),'P-L')
      IF(i.GT.0) THEN
          IF(ln.NE.i+2) GOTO 3
          nd=i-1
          IF(nd.LT.1) GOTO 3
          mpccod(1:3)='PLS'
          DO 4 i=1,4-nd
          mpccod(3+i:3+i)='0'
 4        CONTINUE
          mpccod(8-nd:7)=iauco(1:nd)
          RETURN
      END IF
 3    CONTINUE

* Trojan surveys
      i=index(iauco(1:ln),'T-')
      IF(i.GT.0) THEN
          IF(ln.NE.i+2) GOTO 5
          tsn=iauco(i+2:i+2)
          IF(tsn.NE.'1' .AND. tsn.NE.'2' .AND. tsn.NE.'3') GOTO 5
          mpccod(1:1)='T'
          mpccod(2:2)=tsn
          mpccod(3:3)='S'
          nd=i-1
          DO 6 i=1,4-nd
          mpccod(3+i:3+i)='0'
 6        CONTINUE
          mpccod(8-nd:7)=iauco(1:nd)
          RETURN
      END IF
 5    CONTINUE

* Cannot understand input code
 10   CONTINUE
      mpccod=iauco
      error=.true.

      END
