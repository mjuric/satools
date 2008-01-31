c data to be shared with lower levels
      INTEGER igridx,ixx
      parameter (igridx=1000,ixx=igridx*2+1)
      double precision eq,eqp
      common/elems/eq(6),eqp(6)
      double precision dd,deltau,eps
      integer nmax,igrid,ngrid
      common/contr/dd,deltau,eps,nmax,igrid,ngrid
