c =======================================
c FILNAM
c
c computes  file name in assigned directory
c ========================================
      SUBROUTINE filnam(eledir,astnam,suffix,file,le)
      IMPLICIT NONE

      CHARACTER*(*) eledir
      CHARACTER*(*) astnam
      CHARACTER*(*) suffix
      CHARACTER*(*) file
      INTEGER le
c==== end interface
      INTEGER ldir,l1,l2,l3
      PARAMETER (ldir=60)
      CHARACTER*200 tmpstr
      INTEGER lench
      INCLUDE 'sysdep.h'

      l1=lench(eledir)
      l2=lench(astnam)
      l3=lench(suffix)
      IF(l1+l2+l3+2.gt.200)THEN
         WRITE(0,*) 'filnam: Filename too long:',
     +        eledir(1:l1),dircha,astnam(1:l2),'.',suffix(1:l3)
         STOP
      ENDIF
      tmpstr=eledir(1:l1)//dircha//astnam(1:l2)//'.'//suffix(1:l3)
      CALL rmsp(tmpstr,le)
      file=tmpstr(1:le)
      IF(le.gt.ldir-4)THEN
         WRITE(0,*)'filnam: possible file name truncation',astnam,file
      ENDIF
      RETURN
      END

