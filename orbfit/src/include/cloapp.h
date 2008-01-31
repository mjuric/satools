* Close approaches parameters
c Vers. 1.7.0 May 18, 1998; A. Milani
c storage space for multiple minima/multiple target plane crossing
      INTEGER njcx,njtx,njc,njt
      PARAMETER (njcx=20,njtx=20)
c modified target plane analysis: unit vector perpendicular to MTP
      DOUBLE PRECISION vmtp(3,njcx)
c currently (last) approached planet
      INTEGER iplam
c close approach time, relative position and velocity, min. distance
      DOUBLE PRECISION tcla(njcx),xcla(3,njcx),vcla(3,njcx),rmin(njcx)
c modified target plane intersection
      DOUBLE PRECISION ttar(njtx),xtp(3,njtx),vtp(3,njtx)
c common for al that
      COMMON/clos7/njc,njt,vmtp,tcla,xcla,vcla,rmin,
     +      ttar,xtp,vtp,iplam

