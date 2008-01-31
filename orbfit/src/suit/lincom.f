c
      subroutine lincom(v1,a,v2,b,v3)
c
c    Computes the linear combination vector v3 of the 
c    three-elements vectors v1,v2 with coefficients a,b
c
      implicit double precision (a-h,o-z)
      dimension v1(3),v2(3),v3(3)
      do 40 i=1,3
        v3(i)=a*v1(i)+b*v2(i)
40    continue
      return
      end
