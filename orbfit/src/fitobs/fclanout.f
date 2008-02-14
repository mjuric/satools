c ===============INTERFACE========================
      SUBROUTINE fclanout(iun20,inl,tc,eqc,elm,npo1,tpc,dtpdet,vt3,
     +        tcl1,tcl2,ibv,tcl0,astna0)
      IMPLICIT NONE
c ==============INPUT=============================
c output log unit, nonlinearity handling control (1=linear, 2=semilinear)
      INTEGER iun20,inl
c nominal initial conditions: epoch, elements
      DOUBLE PRECISION tc,eqc(6)
c time interval including the close approach under study, nominal time
      DOUBLE PRECISION tcl1,tcl2,tcl0
c string of virtual asteroids
      INTEGER npox
      PARAMETER (npox=4000)
      DOUBLE PRECISION elm(6,npox)
c target plane coordinates: cartesian, derivatives (transposed)
      DOUBLE PRECISION tpc(2),dtpdet(6,2)
c asteroid names (18 CHARACTERs)
      CHARACTER*18 astna0
c ======== basis adapted to MTP =================
      DOUBLE PRECISION vt3(3,3)
c output is to files an graphics, no output dummy variables
c =============END INTERFACE===========================
c ====== multiple target plane intersections============
c confidence boundary, line of max variation
      INTEGER ibv,npo,npo1
c points on confidence boundary: target plane, close app. manifold, elements 
      DOUBLE PRECISION xta(npox),yta(npox),xcl(npox),ycl(npox)
c relative positions, also in reference system adapted to MTP
      DOUBLE PRECISION dx(3),dv(3),xx(3),vv(3),dxxde(3,6),dxde3(3,6)
c times of the close approach under study
      DOUBLE PRECISION tta,tcl
c relative positions at close approach
      DOUBLE PRECISION xc(3),vc(3)
c close approach manifold 
c      DOUBLE PRECISION rr,rmin0,rr0,xcl0,ycl0
      DOUBLE PRECISION rr     
c propagated elements
      DOUBLE PRECISION eq1(6)
c parametrisation of confidence boundary
      DOUBLE PRECISION sigma,dxl,dyl
c counters for target plane intersections, minimum distances
      INTEGER jt,jc,n,nc,nt,npoc,npot
c functions
      DOUBLE PRECISION prscag
c trig constants
      INCLUDE 'trig.h'
c labels for plots
      CHARACTER*60 ylabel,xlabel,title
c plotting device
      integer idev
c unit numbers
      INTEGER iun7,iun8,iun9
c arrays of close approach data and controls
      INCLUDE 'cloapp.h'
      INCLUDE 'closapl.h'
c ===========================================================
c main loop on the number of output points
      nt=0
      nc=0
      CALL filopn(iun7,'mtp.fla','unknown')
      IF(inl.eq.2)THEN
         CALL filopn(iun8,'clo.fla','unknown')
         CALL filopn(iun9,'a.fla','unknown')
      ENDIF
      do 7 n=1,npo1
c ===========================================================
c chose method to handle nonlinearity
        IF(inl.eq.1)THEN
c linear map from ellipse
           dxl=prscag(6,elm(1,n),dtpdet(1,1))
           dyl=prscag(6,elm(1,n),dtpdet(1,2))
           nt=nt+1
           xta(nt)=dxl+tpc(1)
           yta(nt)=dyl+tpc(2)
           WRITE(iun7,107)xta(nt),yta(nt)
           CALL vsumg(6,eqc,elm(1,n),elm(1,n))
        ELSEIF(inl.eq.2)THEN
c full n-body propagation from ellipse
           CALL vsumg(6,eqc,elm(1,n),elm(1,n))
c to be used for xephem output-to be done later
c          CALL proele('EQU',tc,elm(1,n),tcl1,elm(1,n))
           mtpon=.true.
c          write(0,*)tc,tcl0,tcl2
           CALL proele('EQU',tc,elm(1,n),tcl2,eq1)
           WRITE(iun9,109)eq1
 109       FORMAT(6f20.15)
           mtpon=.false.
c check for existence of data
           IF(njt.ge.1.and.iplam.eq.ipla0)THEN
              jt=1
              tta=ttar(jt)
              IF((tta.gt.tcl1.and.tta.lt.tcl2).or.
     +             (tta.gt.tcl2.and.tta.lt.tcl1))THEN
              ELSE
                 write(0,*)' point ',n, ' time at target plane ',
     +              tta,' was ', tcl0
              ENDIF
              nt=nt+1
c reference system defined by tnor as first axis
              CALL mulmav (vt3,3,3,xtp(1,jt),3,xx)
              CALL mulmav (vt3,3,3,vtp(1,jt),3,vv)
              xta(nt)=xx(2)
              yta(nt)=xx(3)
c file output: target plane
              WRITE(iun7,107)xx,vv,ttar(jt)
 107          FORMAT(7e20.12)
c temporary output
              write(0,177)ttar(jt),xta(nt),yta(nt),nt,n
 177          FORMAT(f11.4,1x,f7.4,1x,f7.4,1x,i4,1x,i4)
              IF(njt.gt.1)THEN
                 WRITE(0,*)' warning: multiple intersection ',njt,n
              ENDIF
c reset counter for next orbit
              njt=0
           ELSEIF(njt.le.0)THEN
              write(0,*)' no target plane ',n
              GOTO 7
           ENDIF
c close approach manifold
           IF(njc.eq.1.and.iplam.eq.ipla0)THEN
              jc=1
              tcl=tcla(jc)
              IF(tcl.gt.tcl1.and.tcl.lt.tcl2)THEN
                 nc=nc+1
c reference system defined by tnor as first axis
                 CALL mulmav (vt3,3,3,xcla(1,jc),3,xc)
                 CALL mulmav (vt3,3,3,vcla(1,jc),3,vc)
c file output: close approach manifold
                 WRITE(iun8,107)xc,vc,tcla(jc) 
c true close approach manifold plotted on the MTP
                 rr=rmin(jc)/sqrt(xx(2)**2+xx(3)**2)
                 xcl(nc)=xx(2)*rr
                 ycl(nc)=xx(3)*rr
c temporary output
                 write(0,177)tcla(jc),xcl(nc),ycl(nc),nc
                 write(0,*)
                 IF(njc.gt.1)THEN
                   write(0,*)' warning: multiple approach ',njc
                 ENDIF
c reset counter
                 njc=0
              ENDIF
           ELSEIF(njc.le.0)THEN
              write(0,*)' no minimum distance ',n
              GOTO 7
           ENDIF
        ELSE
           WRITE(0,*)' fclan:this we have not invented yet ', inl
           RETURN           
        ENDIF
 7    continue
      CALL filclo(iun7,' ')
      IF(inl.eq.2)THEN
         CALL filclo(iun8,' ')
         CALL filclo(iun9,' ')
      ENDIF
c =====================================================================
c count of no. points, without hyperbolic cases
      IF(ibv.eq.1.and.nt.gt.0)THEN
         npot=nt+1
         xta(npot)=xta(1)
         yta(npot)=yta(1)
      ELSE
         npot=nt
      ENDIF
c ======================================================================
c graphics: target plane
      write(title,200)astna0,tcl0
 200  format(a18,' encounter at MJD ',f9.2)
 2    call getdev(idev)
      if(idev.eq.0)RETURN
      xlabel=' target plane xi (AU) '
      ylabel=' target plane zeta (AU) '
c      CALL plotob(xta,yta,tpc(1),tpc(2),npot,
c     +     xlabel,ylabel,title,idev,1)
c mark Earth
      IF(npot.gt.0)THEN
      CALL plotob(xta,yta,0.d0,0.d0,npot,
     +     xlabel,ylabel,title,idev,1)
      ELSE
         WRITE(0,*)' no target plane point available', npot
      ENDIF
      IF(inl.eq.1)GOTO 2
c graphics: close approach manifold
      IF(ibv.eq.1.and.nc.gt.0)THEN
         npoc=nc+1
         xcl(npoc)=xcl(1)
         ycl(npoc)=ycl(1)
      ELSE
         npoc=nc
      ENDIF
c      rr0=rmin0/sqrt(tpc(1)**2+tpc(2)**2)
c      xcl0=tpc(1)*rr0
c      ycl0=tpc(2)*rr0
      xlabel=' close approach manifold xi (AU) '
      ylabel=' close approach manifold zeta (AU) '
c If we are making .ps files then the MTP will be overwritten!
      if(idev.eq.5.or.idev.eq.6)then
         write(0,*)'The PostScript file ''giffv.ps'' is '//
     +        'about to be overwritten. If you wish to save it'//
     +        ' you should rename it before proceeding.'
         pause
      endif
c      CALL plotob(xcl,ycl,tpc(1),tpc(2),npoc,
c    +     xlabel,ylabel,title,idev,1)
c mark Earth
      CALL plotob(xcl,ycl,0.d0,0.d0,npoc,
     +     xlabel,ylabel,title,idev,1)
      GOTO 2
      END




