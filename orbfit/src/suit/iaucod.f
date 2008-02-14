* Copyright (C) 1998 by OrbFit Consortium
* Version: December 15, 1997 Steven Chesley
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         I A U C O D                           *
*  *                                                               *
*  * Computes official IAU code from MPC-style packed designation  *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    MPCCOD    -  MPC-style code as used in observation archives
*                        5 characters for number, 7 characters for
*                        packed provisional designation 
*
* OUTPUT:   IAUDES    -  IAU code
*           ERROR     -  Error flag (cannot understand input code)
*
      SUBROUTINE iaucod(mpccod,iaudes,error)
      IMPLICIT NONE

      CHARACTER*(*) iaudes,mpccod
      LOGICAL error

      INTEGER ln,i,temp
      CHARACTER*2 head
      CHARACTER*3 tail

      INTEGER lench
      LOGICAL isnum
      EXTERNAL lench,isnum

      CHARACTER*12 numfield, desfield

      error=.false.
      iaudes=' '

      ln=lench(mpccod)
      IF(ln.LE.0) GOTO 10
      
      numfield=mpccod(1:5)
      call rmsp(numfield,ln)
* Numbered asteroids
      if(ln.ne.0) then
         iaudes=numfield
         do i=1,ln-1
            if (iaudes(i:i).eq.'0') then
               iaudes(i:i)=' '
            else
               goto 123
            endif
         enddo  
 123     call rmsp(iaudes,ln)
         return 
      endif
      
* Unnumbered asteroids
      desfield=mpccod(6:12)

      if(desfield(3:3).eq.'S')then
c Survey Asteroid
         iaudes=desfield(4:7)//desfield(1:1)//'-'//desfield(2:2)

      elseif (desfield(1:1).eq.'I' .or. 
     +        desfield(1:1).eq.'J' .or. 
     +        desfield(1:1).eq.'K')then
c Temporary designation
         if(desfield(5:6).eq.'00')then
c           1999AA
            tail=''
         elseif(desfield(5:5).eq.'0')then
c           1999AA1
            tail=desfield(6:6)
         elseif(isnum(desfield(5:5)))then
c           1999AA12
            tail=desfield(5:6)
         else
c           1999AA123
            temp=ichar(desfield(5:5))-55
            write(head,103) temp
            tail=head//desfield(6:6)
         endif
         temp=ichar(desfield(1:1))-55
         write(head,103) temp
 103     format(I2)
         iaudes=head//desfield(2:3)//desfield(4:4)//desfield(7:7)//tail
      else
c Unknown type
         write(0,*)'cannot understand MPC designation: ',mpccod
         goto 10
      endif
      return

 10   CONTINUE
      iaudes=mpccod
      error=.true.

      END
