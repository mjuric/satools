c ==========================================
c NODEDI
c nodal distances of two elliptic orbits
c ==========================================
      SUBROUTINE nodedi(eq,eqp,dnp,dnm)
      IMPLICIT NONE
c ===========INPUT=====================
c elements of asteroid, of Earth (equinoctal)
      DOUBLE PRECISION eq(6),eqp(6)
c ===========OUTPUT=================================
c output ascending node distance,  descending node distance
      DOUBLE PRECISION dnp,dnm
c ==========END INTERFACE================================
      DOUBLE PRECISION c(3),cp(3),x(6),xp(6),vlenz(3),vlenzp(3)
      INCLUDE 'parbep.h'
      INCLUDE 'masses.h'
      INCLUDE 'sunmass.h'
      DOUBLE PRECISION enne,ennep,vnod(3),vnl,ome,omep
      DOUBLE PRECISION ecc,eccp,cosf,cosfp,rp,rm,rpe,rme
      INTEGER i
      DOUBLE PRECISION prscal,vsize
c cartesian coordinates      
      CALL coocha(eq,'EQU',gms,x,'CAR',enne)
      CALL coocha(eqp,'EQU',gmse,xp,'CAR',ennep)
c  angular momentum      
      CALL prvec(x,x(4),c)
      CALL prvec(xp,xp(4),cp)
c ascending node
      CALL prvec(cp,c,vnod)
      vnl=vsize(vnod)
c  angular momentum unit vector, Lenz vector
      CALL  prvec(x(4),c,vlenz)
      DO i=1,3
        vlenz(i)=vlenz(i)/gms-x(i)/vsize(x)
      ENDDO
      ecc=vsize(vlenz)
      CALL  prvec(xp(4),cp,vlenzp)
      DO i=1,3
        vlenzp(i)=vlenzp(i)/gmse-xp(i)/vsize(xp)
      ENDDO
      eccp=vsize(vlenzp)
c true anomaly at mutual node= - arg. of perihelion
      cosf=prscal(vnod,vlenz)/(vnl*ecc)
      ome=acos(cosf)
      cosfp=prscal(vnod,vlenzp)/(vnl*eccp)
      omep=acos(cosfp)
c nodal points and distances
      rp=eq(1)*(1.d0-eq(2)**2-eq(3)**2)/(1.d0+ecc*cosf)
      rm=eq(1)*(1.d0-eq(2)**2-eq(3)**2)/(1.d0-ecc*cosf)  
      rpe=eqp(1)*(1.d0-eqp(2)**2-eqp(3)**2)/(1.d0+eccp*cosfp)
      rme=eqp(1)*(1.d0-eqp(2)**2-eqp(3)**2)/(1.d0-eccp*cosfp)            
      dnp=rp-rpe
      dnm=rm-rme
c      WRITE(0,*)rp,rpe,dnp,rm,rme,dnm,cosf,cosfp,ome,omep
      RETURN
      END
