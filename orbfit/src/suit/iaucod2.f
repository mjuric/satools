* Copyright (C) 1998 by OrbFit Consortium
* Version: May 2000 AM MES
* ---------------------------------------------------------------------
*
*  *****************************************************************
*  *                                                               *
*  *                         I A U C O D 2                         *
*  *                                                               *
*  * Computes official IAU code from MPC-style packed designation  *
*  *                                                               *
*  *****************************************************************
*
* INPUT:    MPCCOD    -  MPC-style packed 7 character code as used in orbit archives
*                        5 digits for numbered, 7 characters for unnumbered
*
* OUTPUT:   IAUDES    -  IAU code
*           ERROR     -  Error flag (cannot understand input code)
*
      SUBROUTINE iaucod2(mpccod,iaudes,error)
      IMPLICIT NONE

      CHARACTER*7 mpccod
      CHARACTER*9 iaudes
      LOGICAL error

      INTEGER ln,i,temp
      CHARACTER*2 head
      CHARACTER*3 tail

      INTEGER lench
      LOGICAL isnum
      EXTERNAL lench,isnum

      CHARACTER*5 numfield
      CHARACTER*7 desfield

      error=.false.
      iaudes=' '

      ln=lench(mpccod)
      IF(ln.LE.0) GOTO 10
      IF(ln.eq.5)THEN
* Numbered asteroids
         numfield=mpccod(1:5)
         call rmsp(numfield,ln)
         iaudes=numfield
         do i=1,ln-1
            if (iaudes(i:i).eq.'0') then
               iaudes(i:i)=' '
            else
               goto 123
            endif
         enddo  
 123     call rmsp(iaudes,ln)
         DO i=ln+1,9
           iaudes(i:i)='w'
         ENDDO
         return 
       ELSEIF(ln.eq.7)THEN      
* Unnumbered asteroids
          desfield=mpccod(1:7)
          if(desfield(3:3).eq.'S')then
c Survey Asteroid
          iaudes=desfield(4:7)//desfield(1:1)//'-'//desfield(2:2)//'ww'

          elseif (desfield(1:1).eq.'I' .or. 
     +            desfield(1:1).eq.'J' .or. 
     +            desfield(1:1).eq.'K')then
c Temporary designation
             if(desfield(5:6).eq.'00')then
c           1999AA
                tail='www'
             elseif(desfield(5:5).eq.'0')then
c           1999AA1
                tail=desfield(6:6)//'ww'
             elseif(isnum(desfield(5:5)))then
c           1999AA12
                tail=desfield(5:6)//'w'
             else
c           1999AA123
                temp=ichar(desfield(5:5))-55
                write(head,103) temp
                tail=head//desfield(6:6)
             endif
             temp=ichar(desfield(1:1))-55
             write(head,103) temp
 103         format(I2)
         iaudes=head//desfield(2:3)//desfield(4:4)//desfield(7:7)//tail
          else
c Unknown type
             write(0,*)'cannot understand MPC designation: ',mpccod
             goto 10
          endif
          return
      ELSE
c wrong length of designator
          write(0,*) 'designation ',mpccod,' not understood'          
      ENDIF
 10   CONTINUE
      iaudes=mpccod
      error=.true.

      END






