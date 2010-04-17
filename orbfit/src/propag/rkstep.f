c ==========================================================
c RKSTEP
c automated RKG stepsize change
c ==========================================================
      SUBROUTINE rkstep(ep,npas,nrk,lf,h)
      IMPLICIT NONE
c     INCLUDE 'model.h'
      INCLUDE 'comint.h'
      INCLUDE 'parint.h'
      INCLUDE 'proout.h'
      DOUBLE PRECISION ep(itmax),h
      INTEGER npas,nrk,lf,l,i
c  printout data on aborted step
      l=iabs(lf)
      write (99,*) 'Non-convergence in rk-gauss. See .pro file.'
      write (ipirip,100)npas,nrk,lf,ep(l),eprk,h,isrk,(ep(i),i=1,l)
 100  format(' non convergence in rk at step ',i4,' nrk ',i3,' lf= ',i5
     */' last control ',d14.5,' convergence required ',d12.3/
     *' stepsize ',d14.6,'  order 2*',i2/
     *' controls ',5d12.3/(5d12.3/))
c change stepsize and retry
      h=0.8d0*h
c with h changed, kintrp can not be used;
c also the catatst prepared for the mulktistep needs to be redone
      nrk=0
      lf=2
      npas=0
      RETURN
      END
