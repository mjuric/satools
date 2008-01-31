c =============================
c BIZARRE
c logical function deciding when it is a good idea to give up 
c differential corrections, both for difcor and difvin.
c uses parameters set in bizset
      LOGICAL FUNCTION bizarre(eq)
      IMPLICIT NONE
c input equinoctal elements
      DOUBLE PRECISION eq(6)
c controls
      DOUBLE PRECISION ecclim0, samin0,samax0,phmin0,ahmax0
      COMMON / bizcon /ecclim0, samin0,samax0,phmin0,ahmax0 
c local variables
      DOUBLE PRECISION ecc,a
c ==========================
      ecc=sqrt(eq(2)**2+eq(3)**2)
      a=eq(1)
      bizarre=.false.
      IF(ecc.ge.ecclim0)bizarre=.true.
      IF(a.ge.samax0.or.a.le.samin0)bizarre=.true.
      IF(a*(1.d0-ecc).le.phmin0.or.a*(1.d0+ecc).ge.ahmax0)bizarre=.true.
      RETURN
      END
c ===============================
c BIZSET
c assign/default values of bizarre orbit control parameters
      SUBROUTINE bizset(ecclim,samin,samax,phmin,ahmax)
      IMPLICIT NONE
c controls
      DOUBLE PRECISION ecclim, samin,samax,phmin,ahmax
      DOUBLE PRECISION ecclim0, samin0,samax0,phmin0,ahmax0
      COMMON / bizcon /ecclim0, samin0,samax0,phmin0,ahmax0 
c check if zero, then select default
      IF(ecclim.eq.0.d0)THEN
c exclude only almost hyperbolic
         ecclim0=0.99d0
      ELSE
         ecclim0=ecclim
      ENDIF
      IF(samin.eq.0.d0)THEN
c exclude inner Vulcanoids
         samin=0.1d0
      ELSE
         samin0=samin
      ENDIF
      IF(samax.eq.0.d0)THEN
c exclude long period comets
         samax0=200.d0
      ELSE
         samax0=samax
      ENDIF
      IF(phmin.eq.0.d0)THEN
c exclude sun dipping comets
         phmin0=0.001d0
      ELSE
         phmin0=phmin
      ENDIF
      IF(ahmax.eq.0.d0)THEN
c exclude long periodic comets
         ahmax0=400.d0
      ELSE
         ahmax0=ahmax
      ENDIF
      RETURN
      END

