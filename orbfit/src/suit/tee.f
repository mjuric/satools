c =====================================================================
c TEE (as in unix shell)
c =====================================================================
      subroutine tee(iun,string)
      implicit none
      integer iun
      character*80 string
      integer le
      le=index(string,'=')
      write(0,*)string(1:le-1)
      write(iun,*)string(1:le-1)
      return
      end
