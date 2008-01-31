c+++++++++++++++++++++++++++++++++
c
      subroutine interp(buf,t,ncf,ncm,na,ifl,pv)
c
c+++++++++++++++++++++++++++++++++
c
c     this subroutine differentiates and interpolates a
c     set of chebyshev coefficients to give position and velocity
c
c     calling sequence parameters:
c
c       input:
c
c         buf   1st location of array of d.p. chebyshev coefficients of position
c
c           t   t(1) is dp fractional time in interval covered by
c               coefficients at which interpolation is wanted
c               (0 .le. t(1) .le. 1).  t(2) is dp length of whole
c               interval in input time units.
c
c         ncf   # of coefficients per component
c
c         ncm   # of components per set of coefficients
c
c          na   # of sets of coefficients in full array
c               (i.e., # of sub-intervals in full interval)
c
c          ifl  integer flag: =1 for positions only
c                             =2 for pos and vel
c
c
c       output:
c
c         pv   interpolated quantities requested.  dimension
c               expected is pv(ncm,ifl), dp.
c
c
      implicit double precision (a-h,o-z)
c
      save
c
      double precision buf(ncf,ncm,*),t(2),pv(ncm,ifl),pc(18),vc(18)

c
      data np/2/
      data nv/3/
      data twot/0.d0/
      data pc(1),pc(2)/1.d0,0.d0/
      data vc(2)/1.d0/
c
c       entry point. get correct sub-interval number for this set
c       of coefficients and then get normalized chebyshev time
c       within that subinterval.
c
      dna=dble(na)
      dt1=dint(t(1))
      temp=dna*t(1)
      l=idint(temp-dt1)+1

c         tc is the normalized chebyshev time (-1 .le. tc .le. 1)

      tc=2.d0*(dmod(temp,1.d0)+dt1)-1.d0

c       check to see whether chebyshev time has changed,
c       and compute new polynomial values if it has.
c       (the element pc(2) is the value of t1(tc) and hence
c       contains the value of tc on the previous call.)

      if(tc.ne.pc(2)) then
        np=2
        nv=3
        pc(2)=tc
        twot=tc+tc
      endif
c
c       be sure that at least 'ncf' polynomials have been evaluated
c       and are stored in the array 'pc'.
c
      if(np.lt.ncf) then
        do  i=np+1,ncf
          pc(i)=twot*pc(i-1)-pc(i-2)
        enddo
        np=ncf
      endif
c
c       interpolate to get position for each component
c
      do 2 i=1,ncm
        pv(i,1)=0.d0
        do 3 j=ncf,1,-1
c =====================================================
c OUT OF BOUNDS 
c PGF90-F-Subscript out of range for array buf (jplsub.f: 297)
c    subscript=2, lower bound=1, upper bound=1, dimension=3

          pv(i,1)=pv(i,1)+pc(j)*buf(j,i,l)
c =====================================================
    3   continue
    2 continue
c modification to avoid computing derivatives when not needed
      IF(ifl.le.1)THEN
         DO  i=1,ncm
           pv(i,2)=0.d0
         ENDDO
         RETURN
      ENDIF
c
c       if velocity interpolation is wanted, be sure enough
c       derivative polynomials have been generated and stored.
c
      vfac=(dna+dna)/t(2)
      vc(3)=twot+twot
      if(nv.lt.ncf) then
        do 4 i=nv+1,ncf
          vc(i)=twot*vc(i-1)+pc(i-1)+pc(i-1)-vc(i-2)
    4   continue
        nv=ncf
      endif
c
c       interpolate to get velocity for each component
c
      do 5 i=1,ncm
        pv(i,2)=0.d0
        do 6 j=ncf,2,-1
          pv(i,2)=pv(i,2)+vc(j)*buf(j,i,l)
    6   continue
        pv(i,2)=pv(i,2)*vfac
    5 continue
c
      return
c
      end
