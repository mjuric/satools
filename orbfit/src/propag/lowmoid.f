c This subroutine is provided courtesy of 
c Ted Bowell (elgb@lowell.edu) and 
c Larry Wasserman (lhw@lowell.edu)
*****************************************

	SUBROUTINE LOWMOID(RMIN)
C
C	Find minimum distance between two orbits (RMIN in AU)
C	The method is taken from G. Sitarski, Acta Astronomica, Vol 18, P171.
C	1968.
C	Sitarsky uses upper and lower case letters to designate parameters
C	for the two orbits. We use 1 and 2.
C	The orbital elements for the two bodies are passed in COMMOM ORB1 and ORB2
C	   APn - Argument of perihelion (deg)
C	   ANn - Longitude of ascending node (deg)
C	   AIn - Inclination (deg)
C	   AEn - Eccentricity
C	   AAn - Semi-major axis (AU)
C
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	LOGICAL FLAG,SEARCH
	DOUBLE PRECISION K,L,M,N
	COMMON/ORB1/AP1,AN1,AI1,AE1,AA1
	COMMON/ORB2/AP2,AN2,AI2,AE2,AA2
	COMMON/VEC1/PX1,QX1,PY1,QY1,PZ1,QZ1
	COMMON/VEC2/PX2,QX2,PY2,QY2,PZ2,QZ2
	COMMON/DATA/P1,P2,K,L,M,N,CON
	CON=3.141592653589793D0/180.D0
	Q1=(1.D0-AE1)*AA1
	Q2=(1.D0-AE2)*AA2
	P1=(1.D0+AE1)*Q1
	P2=(1.D0+AE2)*Q2
C---	Quantities K, L, M, and N
	CALL KLMN
	IX=0
	RMIN=1.D+30
C---	We need to repeat the procedure twice, one for each possible solution
C---	for V2 for each V1
	DO ICD=1,2
C---	   Start with -180 degrees and -179 degrees
	   V11=-180.D0*CON
	   V12=-179.D0*CON
C---	   Find the first V2 which solves the first equation of condition for
	   CALL EQ1SOLVE(V11,ICD,V21)
	   CALL EQU2EVAL(V11,V21,E2VAL1)
	   DO JJ=1,361
	      CALL EQ1SOLVE(V12,ICD,V22)
	      CALL EQU2EVAL(V12,V22,E2VAL2)
	      SEARCH=.FALSE.
	      IF(E2VAL1.EQ.1.D+30.AND.E2VAL2.NE.1.D+30) THEN
	         CALL BINSEARCH(ICD,V11,V12,V1FIN,V2FIN,FLAG)
	         SEARCH=.TRUE.
	      ELSEIF(E2VAL1.NE.1.D+30.AND.E2VAL2.EQ.1.D+30) THEN
	         CALL BINSEARCH(ICD,V11,V12,V1FIN,V2FIN,FLAG)
	         SEARCH=.TRUE.
 	      ELSEIF(E2VAL1.EQ.1.D+30.AND.E2VAL2.EQ.1.D+30) THEN
	         CONTINUE
	      ELSEIF(E2VAL1*E2VAL2.LT.0.) THEN
	         CALL BINSEARCH(ICD,V11,V12,V1FIN,V2FIN,FLAG)
	         SEARCH=.TRUE.
	      ENDIF
	      IF(SEARCH.AND.FLAG) THEN
	         CALL RCAL(V1FIN,V2FIN,RR)
	         IF(RR.LT.RMIN) RMIN=RR
	      ENDIF
	      V11=V12
	      V21=V22
	      E2VAL1=E2VAL2
	      V12=V11+1.D0*CON
	   ENDDO
	ENDDO
	RETURN
	END
	SUBROUTINE BINSEARCH(ICODE,V1L,V1H,V1,V2,FLAG)
C
C	The second equation may have a solution between V1LO and V1HI. Find it
C	by a binary search. FLAG is returned .TRUE. if a real solution is
C	found (ie between a positive and negative solution) and .FALSE.
C	if we are between a real solution and a non-valid solution
C
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	LOGICAL FLAG
	V1LO=V1L
	V1HI=V1H
	CALL EQ1SOLVE(V1LO,ICODE,V2LO)
	CALL EQU2EVAL(V1LO,V2LO,FLO)
	CALL EQ1SOLVE(V1HI,ICODE,V2HI)
	CALL EQU2EVAL(V1HI,V2HI,FHI)
1	V1MID=(V1LO+V1HI)/2.D0
	CALL EQ1SOLVE(V1MID,ICODE,V2MID)
	CALL EQU2EVAL(V1MID,V2MID,FMID)
	IF(FMID.EQ.1.D+30) THEN
	   FLAG=.FALSE.
	ELSE
	   FLAG=.TRUE.
	ENDIF
	IF(ABS(V1HI-V1LO).LT..0000001D0) THEN
	   V1=V1MID
	   V2=V2MID
	   RETURN
	ENDIF
	IF(FLO.NE.1.D+30.AND.FMID.EQ.1.D+30) THEN
C---	   Solution between FLO and FMID (V1LO and V1MID)
	   IF(FHI.NE.1.D+30) STOP 'Error in binary search 1'
	   FHI=FMID
	   V1HI=V1MID
	   GO TO 1
	ELSEIF(FHI.NE.1.D+30.AND.FMID.EQ.1.D+30) THEN
C---	   Solution between FMID and FHI (V1MID and V1HI)
	   IF(FLO.NE.1.D+30) STOP 'Error in binary search 2'
	   FLO=FMID
	   V1LO=V1MID
	   GO TO 1
	ELSEIF(FHI.EQ.1.D+30.AND.FLO.EQ.1.D+30) THEN
C---	   Something is wrong if both hi and lo are off, no matter what mid is
	   STOP 'Error in binary search 3'
	ELSEIF(FLO.EQ.1.D+30.AND.FMID*FHI.GT.0.D0) THEN
C---	   Solution between FLO and FMID (V1LO and V1MID)
	   FHI=FMID
	   V1HI=V1MID
	   GO TO 1
	ELSEIF(FLO.EQ.1.D+30.AND.FMID*FHI.LE.0.D0) THEN
C---	   Solution between FMID and FHI (V1MID and V1HI)
	   FLO=FMID
 	   V1LO=V1MID
	   GO TO 1
	ELSEIF(FHI.EQ.1.D+30.AND.FMID*FLO.GT.0.D0) THEN
C---	   Solution between FMID and FHI (V1MID and V1HI)
	   FLO=FMID
 	   V1LO=V1MID
	   GO TO 1
	ELSEIF(FHI.EQ.1.D+30.AND.FMID*FLO.LE.0.D0) THEN
C---	   Solution between FLO and FMID (V1LO and V1MID)
	   FHI=FMID
	   V1HI=V1MID
	   GO TO 1
	ELSEIF(FMID*FHI.LE.0.D0) THEN
C---	   Solution between FMID and FHI (V1MID and V1HI)
	   FLO=FMID
 	   V1LO=V1MID
	   GO TO 1
 	ELSEIF(FMID*FLO.LE.0.D0) THEN
C---	   Solution between FLO and FMID (V1LO and V1MID)
	   FHI=FMID
	   V1HI=V1MID
	   GO TO 1
	ENDIF
	RETURN
	END
	SUBROUTINE EQ1SOLVE(V1,ICODE,V2)
C
C	Given true anomaly for orbit 1 (V1) use the first of two conditon
C	equations to find the true anomaly for the second orbit (V2). 
C	Select possible solution ICODE=1 or 2
C    
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DOUBLE PRECISION K,L,M,N,LL,MM
	COMMON/ORB1/AP1,AN1,AI1,AE1,AA1
	COMMON/ORB2/AP2,AN2,AI2,AE2,AA2
	COMMON/DATA/P1,P2,K,L,M,N,CON
	R1=P1/(1.D0+AE1*COS(V1))
	Y1=R1*SIN(V1)
	X1=R1*COS(V1)
C---	W is w; T is t; S is s; MM is m; LL is l
	S=AE1*R1*Y1/P2
	T=M*Y1-N*(AE1*R1+X1)
	W=AE2*S+K*Y1-L*(AE1*R1+X1)
	MM=T**2+W**2
	LL=MM-S**2
	IF(LL.LT.0.D0) THEN
C---	  There is no solution for this value of V1.
	  V2=1.D30
	ELSE
C---	   There are two solutions for the true anomaly of the second orbit.
C---	   Choose one according to ICODE.
	   IF(ICODE.EQ.1) THEN
	      SA=(-T*S-W*SQRT(LL))/MM
	      CA=(-W*S+T*SQRT(LL))/MM
	   ELSE
	      SA=(-T*S+W*SQRT(LL))/MM
	      CA=(-W*S-T*SQRT(LL))/MM
	   ENDIF
	   V2=ATAN2(SA,CA)
	   IF(V2.LT.0.D0) V2=V2+2.D0*3.141592653589793D0
	ENDIF
	RETURN
	END
	SUBROUTINE EQU2EVAL(V1,V2,VAL)
C
C	Evaluate second condition equation for V1,V2.
C
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DOUBLE PRECISION K,L,M,N
	COMMON/ORB1/AP1,AN1,AI1,AE1,AA1
	COMMON/ORB2/AP2,AN2,AI2,AE2,AA2
	COMMON/DATA/P1,P2,K,L,M,N,CON
	IF(V2.EQ.1.D+30) THEN
	   VAL=1.D+30
	   RETURN
	ENDIF
	R1=P1/(1.D0+AE1*COS(V1))
	R2=P2/(1.D0+AE2*COS(V2))
	X1=R1*COS(V1)
	Y1=R1*SIN(V1)
	X2=R2*COS(V2)
	Y2=R2*SIN(V2)
	TERM1=AE2*R2*Y2
	TERM2=Y2*(K*X1+L*Y1)
	TERM3=-(AE2*R2+X2)*(M*X1+N*Y1)
	VAL=TERM1+TERM2+TERM3
	RETURN
	END
	SUBROUTINE RCAL(V1,V2,RR)
C
C	Calculate distance between orbits at V1 and V2
C
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DOUBLE PRECISION K,L,M,N
	COMMON/ORB1/AP1,AN1,AI1,AE1,AA1
	COMMON/ORB2/AP2,AN2,AI2,AE2,AA2
	COMMON/DATA/P1,P2,K,L,M,N,CON
	COMMON/VEC1/PX1,QX1,PY1,QY1,PZ1,QZ1
	COMMON/VEC2/PX2,QX2,PY2,QY2,PZ2,QZ2
	R1=P1/(1.D0+AE1*COS(V1))
 	R2=P2/(1.D0+AE2*COS(V2))
	X1=R1*COS(V1)
	Y1=R1*SIN(V1)
	X2=R2*COS(V2)
	Y2=R2*SIN(V2)
C---	Space cooords of orbit 1
	XX1=X1*PX1+Y1*QX1
	YY1=X1*PY1+Y1*QY1
	ZZ1=X1*PZ1+Y1*QZ1
C---	And of orbit 2
	XX2=X2*PX2+Y2*QX2
	YY2=X2*PY2+Y2*QY2
	ZZ2=X2*PZ2+Y2*QZ2
	XX=XX1-XX2
	YY=YY1-YY2
	ZZ=ZZ1-ZZ2
	RR=SQRT(XX**2+YY**2+ZZ**2)
	RETURN
	END
	SUBROUTINE KLMN
C
C	Compute K,L,M,N from Sitarski and the upper and lower case P's and Q's
C
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DOUBLE PRECISION K,L,M,N
	COMMON/ORB1/AP1,AN1,AI1,AE1,AA1
	COMMON/ORB2/AP2,AN2,AI2,AE2,AA2
	COMMON/VEC1/PX1,QX1,PY1,QY1,PZ1,QZ1
	COMMON/VEC2/PX2,QX2,PY2,QY2,PZ2,QZ2
	COMMON/DATA/P1,P2,K,L,M,N,CON
C---	Compute the capital P, Q values and save in VEC1 and VEC2
	AN1R=AN1*CON
	AP1R=AP1*CON
	AI1R=AI1*CON
	SAP1=SIN(AP1R)
	CAP1=COS(AP1R)
	SAI1=SIN(AI1R)
	CAI1=COS(AI1R)
	PX1=+CAP1
	QX1=-SAP1
	PY1=SAP1*CAI1
	QY1=CAP1*CAI1
	PZ1=SAP1*SAI1
	QZ1=CAP1*SAI1
	AN2R=AN2*CON
	AP2R=AP2*CON
	AI2R=AI2*CON
	SAP2=SIN(AP2R)
	CAP2=COS(AP2R)
	SAI2=SIN(AI2R)
	CAI2=COS(AI2R)
C---    And the lower case p,q values as well as K, L, M, N
	SDAN=SIN(AN2R-AN1R)
	CDAN=COS(AN2R-AN1R)
	PX2=CAP2*CDAN-SAP2*CAI2*SDAN
	PY2=CAP2*SDAN+SAP2*CAI2*CDAN
	PZ2=SAP2*SAI2
	QX2=-SAP2*CDAN-CAP2*CAI2*SDAN
	QY2=-SAP2*SDAN+CAP2*CAI2*CDAN
	QZ2=+CAP2*SAI2
	K=PX1*PX2+PY1*PY2+PZ1*PZ2
	L=QX1*PX2+QY1*PY2+QZ1*PZ2
	M=PX1*QX2+PY1*QY2+PZ1*QZ2
	N=QX1*QX2+QY1*QY2+QZ1*QZ2
	RETURN
	END
