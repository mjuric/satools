c ====================================================
      subroutine our_mar(nam2,nam1)
      character*8 nam1
      character*6 nam2
      character*1 a
      character*2 c
       c=nam2(5:6)
       if((c.eq.'T1').or.(c.eq.'T2').or.(c.eq.'T3')
     +    .or.(c.eq.'PL'))
     +  then 
          nam1(1:5)=nam2(1:5)
          nam1(6:6)='-'
          nam1(7:7)=nam2(6:6)
          nam1(8:8)=' '
       else
c 
c second case: ix.a.b.iy (ix,iy 2digits integers)
c
          nam1(1:2)='19'
          nam1(3:8)=nam2(1:6)
       endif
       return
       end
c ====================================================
c  {\bf strnam}
c  from 18 character string to official designator
c  (to be used only for unnumbered asteroids)
      subroutine strnam(stri,nam,lnam)
      character*18 stri,st1,st2
      character*6 nam
      character*1 st
c  split double designators
      n=index(stri,' ')
      if(n.eq.0)then
         write(0,*)' no blank in ',stri
         nam=stri(1:6)
         lnam=18
         return
      endif
      st1='                  '
      st2='                  '
      st1=stri(1:n)
      st2=stri(n+1:18)
c  select first designator if it is an official one
      st=st1(1:1)
      if(st.eq.'1'.or.st.eq.'2'.or.st.eq.'3'.or.st.eq.'4'.or.
     +    st.eq.'5'.or.st.eq.'6'.or.st.eq.'7'.or.st.eq.'8'.or.
     +    st.eq.'9'.or.st.eq.'0')then
c  remove blanks, check length
         call rmsp(st1,lnam)
         nam=st1(1:6)
      elseif(st.eq.'A')then
c  remove blanks, check length
         call rmsp(st1,lnam)
         nam=st1(2:7)
c         write(9,*)stri,nam
      else
         call rmsp(st2,lnam)
         nam=st2(1:6)
      endif
      return
      end
c =======================================================
c  {\bf namcod}
c  from official designator to numeric code (8 digit)
c  the designator is supposed to be iy.a.b.ix for 
c  sporadic discovery, inum.cd for surveys
      subroutine namcod(nam,icode)
      character*13 nam
      character*1 st,a,b
      character*2 cd
c  modern designators: discriminate the two cases
      st=nam(5:5)
      if(st.eq.'1'.or.st.eq.'2'.or.st.eq.'3'.or.st.eq.'4'.or.
     +    st.eq.'5'.or.st.eq.'6'.or.st.eq.'7'.or.st.eq.'8'.or.
     +    st.eq.'9'.or.st.eq.'0'.or.st.eq.' '.or.st.eq.'U')then
c  get rid of notes (e.g. 'U') after the designator
         if(st.eq.'U')then
            nam(5:5)=' '
         endif
         if(nam(6:6).eq.'U')nam(6:6)=' '
c  read designator with year etc.
         read(nam,100,err=99)iy,a,b,ix
 100     format(i2,a1,a1,i2)
         na=64
         ia=ichar(a)-na
         ib=ichar(b)-na
         icode=1000000*iy+ia*10000+ib*100+ix
      else
c  survey case;
         read(nam,101,err=99)inum,cd
 101     format(i4,a2)
         if(cd.eq.'PL')then
            icode=10000*inum+1
         elseif(cd.eq.'T1')then
            icode=10000*inum+2
         elseif(cd.eq.'T2')then
            icode=10000*inum+3
         elseif(cd.eq.'T3')then
            icode=10000*inum+4
         else
            write(0,*)' unknown survey ',nam
         endif
      endif
      return
 99   write(0,*)' encoding error ',nam
      icode=0
      end
c =======================================================
c  {\bf codnam}
c  from numeric (8 digit) code number to official designator
      subroutine codnam(icode,nam)
      character*13 nam
      character*1 a,b
      character*2 cd
      nam='             '
c  discriminate survey names from names with year
      inum=icode/10000
      iz=icode-10000*inum
      ib=iz/100
      ix=iz-100*ib
      if(ib.eq.0)then
c  survey names
         if(ix.eq.1)then
            cd='PL'
         elseif(ix.eq.2)then
            cd='T1'
         elseif(ix.eq.3)then
            cd='T2'
         elseif(ix.eq.4)then
            cd='T3'
         else
            write(0,*)' unknown survey ',icod
         endif
         write(nam,100)inum,cd
 100     format(i4,a2)
      else
c  names with year
         iy=inum/100
         ia=inum-100*iy
         a=char(64+ia)
         b=char(64+ib)
         if(ix.eq.0)then
            write(nam,101)iy,a,b 
 101        format(i2,a1,a1)
         elseif(0.lt.ix.and.ix.lt.10)then
            write(nam,102)iy,a,b,ix
 102        format(i2,a1,a1,i1)
         elseif(ix.gt.9.and.ix.lt.100)then
            write(nam,103)iy,a,b,ix
 103        format(i2,a1,a1,i2)
         else
            write(0,*)' problems with identifyer ', icode
         endif
c          if(iy.ge.0.and.iy.le.9)then
c             write(nam,106)iy
c  106        format('0',i1)
c          elseif(iy.ge.10.and.iy.le.99)then
c            write(nam,107)iy
c 107        format(i2)
c         else
c            write(0,*)' problems with year in ',icode
c         endif
      endif
      return
      end
