c ================================================================
c MENU
c ================================================================
      SUBROUTINE menu(ifl,menunam,nopt,s,
     +             s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)
      IMPLICIT NONE
      INTEGER ifl,nopt,ll,iunit,lench
      CHARACTER*(*) s,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10
      CHARACTER*20 menunam
      CHARACTER*120 helpfi,ddocd1
      INCLUDE 'doclib.h'
c
      IF(nopt.lt.2.or.nopt.gt.10)then
         write(99,*) ' this menu can only handle betw. 2 and 10 options'
         ifl=0
         return
      ENDIF
 3    continue
      ll=index(s,'=')
      write(99,*) s(1:ll-1)
      ll=index(s1,'=')
      write(99,*)' 1 = ', s1(1:ll-1)
      ll=index(s2,'=')
      write(99,*)' 2 = ', s2(1:ll-1)
      IF(nopt.lt.3) goto 2
      ll=index(s3,'=')
      write(99,*)' 3 = ', s3(1:ll-1)
      IF(nopt.lt.4) goto 2
      ll=index(s4,'=')
      write(99,*)' 4 = ', s4(1:ll-1)
      IF(nopt.lt.5) goto 2
      ll=index(s5,'=')
      write(99,*)' 5 = ', s5(1:ll-1)
      IF(nopt.lt.6) goto 2
      ll=index(s6,'=')
      write(99,*)' 6 = ', s6(1:ll-1)
      IF(nopt.lt.7) goto 2
      ll=index(s7,'=')
      write(99,*)' 7 = ', s7(1:ll-1)
      IF(nopt.lt.8) goto 2
      ll=index(s8,'=')
      write(99,*)' 8 = ', s8(1:ll-1)
      IF(nopt.lt.9) goto 2
      ll=index(s9,'=')
      write(99,*)' 9 = ', s9(1:ll-1)
      IF(nopt.lt.10) goto 2
      ll=index(s10,'=')
      write(99,*)'10 = ', s10(1:ll-1)
c
c room to increase
c
 2    write(99,103)
 103  format(' 0 = exit; -1=help')
      write(99,*)' selection?  '
      read(*,*,err=3)ifl
c wrong flag and exit
 4    IF(ifl.lt.-1.or.ifl.gt.nopt)THEN
            write(99,*)ifl,' option not understood'
            goto 3
      ELSEIF(ifl.eq.-1)THEN
         ddocd1=ddocd
         ll=lench(ddocd1)
         helpfi=ddocd1(1:ll)//'/'//menunam
         CALL rmsp(helpfi,ll)
         helpfi=helpfi(1:ll)//'.help'
         CALL filopn(iunit,helpfi,'OLD')
         CALL filcat(iunit)
         CALL filclo(iunit,' ')
         write(99,*)' selection?  '
         read(*,*,err=3)ifl
         GOTO 4
      ENDIF
      RETURN
      END
c ===========================================
      SUBROUTINE filcat(iunit)
      IMPLICIT NONE
      INTEGER iunit,i,imax,ll,lcom,lench
      PARAMETER (imax=100)
      CHARACTER*100 record
c =========================================
      DO i=1,imax
        READ(iunit,100,END=2)record
 100    FORMAT(a)
        ll=lench(record)
c        write(99,*)ll
c comments begin with %, for TeX compatibility
        lcom=index(record,'%')
        IF(lcom.gt.1)THEN
           write(99,100)record(1:lcom-1)
        ELSEIF(lcom.eq.0)THEN
           IF(ll.gt.0)THEN
              write(99,100)record(1:ll)
           ELSE
              write(99,*)
           ENDIF
        ENDIF
      ENDDO
 2    RETURN
      END



