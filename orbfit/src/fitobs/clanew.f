c ===========================================================
c CLANEW
c Newton's method to find virtual impactor along the line of variations
      SUBROUTINE clanew(tcl0,tcl1,tcl2,axes,sig,tpc,dtpdet,tc,t1,
     +     eqc,gc,cc,csinor,delnor,
     +     m,objid,iobs,tau,tutm,idsta,aln,den,rmsa,rmsd,sel,iun20,
     +     error,dminew,tpr)
      IMPLICIT NONE
c ==========INPUT=============
c output unit
      INTEGER iun20
c initial conditions: epoch, elements, propagation to cl. app. time
      DOUBLE PRECISION tc,eqc(6),t1
c =============INPUT/OUTPUT=========================
c time interval including the close approach under study
      DOUBLE PRECISION tcl0,tcl1,tcl2
c target plane coordinates, target plane ellipse
      DOUBLE PRECISION tpc(2),axes(2,2),sig(2),dtpdet(6,2)
c normal and covariance matrices
      DOUBLE PRECISION gc(6,6),cc(6,6)
c norms of residuals, of last correction
      DOUBLE PRECISION csinor,delnor
c number of observations, station codes, obs. type, object id
      INTEGER m,idsta(m),iobs(m)
      CHARACTER*(*) objid(m)
c selection flags
      INTEGER sel(m)
c observation times (ET, UT), alpha, delta, a priori rms
      DOUBLE PRECISION tau(m),tutm(m),aln(m),den(m),rmsa(m),rmsd(m)
c ==========OUTPUT============
c error return
      LOGICAL error
c new expected min.distance, current distance
      DOUBLE PRECISION dminew,tpr
c ==========END INTERFACE
c time of the final elements, new elements, state transition matrix
      DOUBLE PRECISION tcl,eq1(6),de1de0(6,6)
c arrays of close approach data and controls
      INCLUDE 'cloapp.h'
      INCLUDE 'closapl.h'
c ======== constant of gravitation ==============
      include 'sunmass.h'
c variables for Newton's method
      DOUBLE PRECISION s1,tpc1(2),dtpc(2),s1max
      DOUBLE PRECISION del2(2),del4(4),dels(6),delem(6),elem(6)
      DOUBLE PRECISION gnew(6,6),cnew(6,6),elnew(6),enne
      DOUBLE PRECISION de1dx1(6,6),dxde1(6,6)
c ======== call to propag =======================
      INTEGER ider
c cartesian coord., at time tcl0, of the asteroid, of the Earth, derivatives 
      DOUBLE PRECISION xast(6),xea(6),dxde(6,6),ddxde(3,6,6)
c various times (comment better!)
c      DOUBLE PRECISION tta,tcl,tcros
c distance, velocity
      DOUBLE PRECISION dmin0,vmin0
c ======== call to force =======================
      INTEGER nvar2,idc
      DOUBLE PRECISION dery(3),xpla(6)
c control of derivatives
      INTEGER ide
      COMMON/deriv/ide
c ======== basis adapted to MTP =================
      DOUBLE PRECISION v3(3,3),vt3(3,3),vsize
c relative positions, also in reference system adapted to MTP
      DOUBLE PRECISION dx(3),dv(3)
c relative positions at close approach
      DOUBLE PRECISION xc0(3),vc0(3)
c correspondent ellipse in orbital lements space
      DOUBLE PRECISION b(2,2),ceicel(4,2),v(6,6)
c for difcor
      DOUBLE PRECISION csinew,delnew
      INTEGER icor(6),inew,inter,ncor,itsav,iun20m
c      DOUBLE PRECISION tsave 
      DOUBLE PRECISION delcr
      LOGICAL succ
      INCLUDE 'comdif.h'
      INCLUDE 'parobx.h'
      DOUBLE PRECISION w(nob2x),csir(nob2x),x2(nobx)
c loop indexes
      INTEGER jj,i
c logical controls
      LOGICAL batchsel
c max distance (from nominal point on target plane) at which we can 
c reasonably apply a linear approx
      DOUBLE PRECISION linlim
      PARAMETER (linlim=0.02d0)
c max value of sigma considered as confidence boundary
      DOUBLE PRECISION siglim
      PARAMETER (siglim=3.d0)
c no continuation available so fare
      CALL ident(6,de1de0)
c =====================================================
c compute ellipse in the elements space 
      CALL slinel(dtpdet,gc,cc,ceicel,b,v)
c find closest point on MTP according to linear approx
      s1=-tpc(1)*axes(1,2)-tpc(2)*axes(2,2)
c control on the size of displacement s1 (in AU)
      s1max=linlim
      IF(abs(s1).gt.s1max)THEN
         WRITE(0,*)' clanew: Newton step too long s1=',s1,s1max
         s1=s1*s1max/abs(s1)
      ENDIF
c control on the size of change in the sigma space
      IF(s1/sig(2).gt.siglim)THEN
         WRITE(0,*)' clanew: too large deltasig=',s1/sig(2),siglim
         error=.true.
         RETURN 
      ENDIF
c new target point on MTP
      DO jj=1,2
         dtpc(jj)=s1*axes(jj,2)
         tpc1(jj)=tpc(jj)+dtpc(jj)
      ENDDO
      dminew=sqrt(tpc1(1)**2+tpc1(2)**2)
      WRITE(0,*)' displ.=',s1,' deltasig=',s1/sig(2),' dmin=',dminew
      WRITE(iun20,*)' displ.=',s1,' deltasig=',s1/sig(2),' dmin=',dminew
c find corresponding orbital elements at epoch
      CALL mulmav(b,2,2,dtpc,2,del2)
      CALL mulmav(ceicel,4,2,del2,2,del4)
      CALL vcopy(2,del2,dels)
      DO jj=1,4
         dels(2+jj)=-del4(jj)
      ENDDO
      CALL mulmav(v,6,6,dels,6,delem)
      CALL vsumg(6,eqc,delem,elem)
c setup of weights (non-zero!)
      CALL fitwgt(rmsa,rmsd,den,sel,iobs,w,m,.false.)
c solve for all elements
      inter=0
      CALL whicor(inter,icor,ncor,inew)
c zero iterations differential corrections
      itsav=itmax
      itmax=0
      iun20m=-iun20
      CALL difcor(m,w,sel,tc,iobs,tau,idsta,elem,aln,den,icor,inew,
     +     iun20m,delcr,elem,gnew,cnew,csinew,delnew,csir,x2,succ)
      itmax=itsav
c call private propagation routine, requiring derivatives
      ider=1
      tcl=tcl2
      CALL propag(tc,elem,tcl,xast,xea,ider,dxde1,ddxde)
c use accumulated state transition matrix
      CALL mulmat(dxde1,6,6,de1de0,6,6,dxde)
c compute new elements, with partial derivatives
      CALL cooder(xast,'CAR',gms,eq1,'EQU',enne,de1dx1)
c chain rule to obtain d(east1)/d(east0)
      CALL mulmat(de1dx1,6,6,dxde,6,6,de1de0)
c exploratory propagation (back to before close approach)
      iplam=0
      CALL proele('EQU',tcl,eq1,tcl1,elnew)
c store new target plane; batch mode
      batchsel=.true.
c      write(0,*)'tcl.tcl0.tcl1.tcl2 ',tcl,tcl0,tcl1,tcl2
      CALL mtpsel(batchsel,tc,t1,xc0,vc0,tcl0,tcl1,tcl2,error)
      IF(error)THEN
          WRITE(0,*)' tc,t1,tcl0,tcl1,tcl2 ',tc,t1,tcl0,tcl1,tcl2
          RETURN
      ENDIF
      dmin0=vsize(xc0)
      vmin0=vsize(vc0)
      WRITE(iun20,*)' close app. at t=',tcl0
      WRITE(iun20,*)' distance=',dmin0,' velocity=',vmin0
c propagate to  target epoch
      mtpon=.false.
      ider=1 
      CALL propag(tcl,eq1,tcl0,xast,xea,ider,dxde1,ddxde)
c chain rule to obtain d(xast1)/d(east0)
      CALL mulmat(dxde1,6,6,de1de0,6,6,dxde)
c right hand side at target (to obtain xpla)
      nvar2=3
      ide=0
      CALL force(xast,xast(nvar2+1),tcl0,dery,nvar2,idc,xpla)
      IF(idc.ne.iplam)THEN
         WRITE(0,*)' fclan: ???? idc=',idc,' iplam=',iplam
      ENDIF
c relative coordinates
      DO i=1,3
         dx(i)=xast(i)-xpla(i)
         dv(i)=xast(i+3)-xpla(i+3)
      ENDDO
c mtp reference system
      CALL mtpref(tpnor,xpla(4),v3,vt3)
c rotation to v3 reference system
      CALL mtprot(batchsel,vt3,dx,dv,dxde,gc,tpc,dtpdet,sig,axes,tpr)
      WRITE(0,171)sig
      WRITE(0,172)axes
      WRITE(iun20,171)sig
 171  FORMAT(' semiaxes of target ellipse: lengths ',2d14.5)
      WRITE(iun20,172)axes
 172  FORMAT(' directions ',2f10.6/12x,2f10.6)
c compute ellipse in the elements space 
      CALL slinel(dtpdet,gc,cc,ceicel,b,v)
c adopt new minimum as nominal
      CALL vcopy(6,elem,eqc)
      CALL mcopy(6,6,gnew,gc)
      CALL mcopy(6,6,cnew,cc)
      csinor=csinew
      delnor=delnew
      write(0,*)' Newton control=',dminew-tpr
      write(iun20,*)' Newton control=',dminew-tpr
      RETURN
      END
