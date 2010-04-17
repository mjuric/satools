c Copyright Orbfit Consortium 1999
c this routine determines an asteroid radius if needed for radar
      subroutine astrad(objid,iobs,m)
      implicit none
      integer m,iobs(m)
      character*9 objid(m)
c observation numbers: maximum, space left
c      INCLUDE 'parobx.h'
c      INTEGER nlef
c ====================================

      integer unit,ln,lnam,lnum,isav,i
      character*18 oid,number,name
      double precision hmag,diam,exponent
      logical needed
c radius of asteroid common
      include 'radius.h'

c First find out if we need the radius at all
      needed=.false.
      do i=1,m
         if(iobs(i)/100.eq.21)then
            needed=.true.
            isav=i
            goto 100
         endif
      enddo
 100  continue
      if(.not.needed)then
c bail out
c         write (*,*) 'Asteroid radius not required.'
         radius=-1d0
         return
      else
c get radius
         call filopl(unit,'astorb.rad')
         oid=objid(isav)
         call rmsp(oid,ln)
c read records in an infinite loop (it's F(UGLY)77)         
 1       continue
            read(unit,101,end=109) number,name,hmag,diam
            call rmsp(number,lnum)
            call rmsp(name,lnam)
            if(  oid(1:ln).eq.number(1:lnum) .or.
     +           oid(1:ln).eq.name(1:lnam)    )THEN
c              found object in file
               if(diam.gt.0d0)then
                  radius=diam/2d0/1.4998d8
                  write(99,102)'RADAR: Using radius = ',
     +                 radius*1.4998d8,' km from IRAS diameter.'
               else
                  exponent=0.5d0*(6.3d0-log10(0.2d0)-0.4d0*hmag)
                  radius=10**exponent/2d0/1.4998d8
                  write(99,102)'RADAR: Using radius = ',
     +                 radius*1.4998d8,' km from absolute magnitude.'
               endif
c exit infinite loop
               goto 109
            endif
         goto 1
      endif
      write(99,*)'*** astrad warning: ',name,' not found in astorb.dat.'
      radius=1d0/1.4998d8
      write(99,*)'RADAR: Using radius = ',radius*1.4998d8,' km.'
 109  call filclo(unit,' ')
      return

c ==============================================================
c                  num   nam    comp   Hmag  Gmag    col   diam
 101        format(a5,1X,A18,1X,15x,1X,f5.2,1X,5x,1X,4x,1X,f5.1)
 102        format(a,f6.2,a)
c ==============================================================

      end
