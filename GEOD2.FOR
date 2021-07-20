      SUBROUTINE ADJST( NX, NDPY, NRW, PX, A, C, W, U, ANORM, V, X, PY ,
     1Y, VPV, VPV1, PD, PYY, AMTR1, AMTR2, N1, NGR, NBI, NTEST, KDPSI,
     1 DT, P, PSC,LOK,LOKMN,PINT,MXPS,MXDP,NSAT,DTS,XLO, NNGR, VARORB)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
      DIMENSION  PX( NX, NX), C( NRW, NBI), A( NRW, 3), W( NRW), U(NRW),
     1 ANORM( NX, NX), X(NX), Y(NDPY), PY (NDPY,NDPY), V(NRW), PD(1)
      DIMENSION PYY(1), AMTR1(NDPY,N1), AMTR2(N1,N1), NGR(1),NTEST(1)
      DIMENSION KDPSI(MXDP,MXPS), DT(MXDP,MXPS), P(1), PSC(1), LOK(1),
     1 PINT(1), VARORB(21), NNGR(1)
      REAL*8 LOK
C     SUBROUTINE ADJST
C     PHASE ADJUSTMENT WITH WEIGHTING OF PARAMETERS
C     USAGE
C         PHASE  ADJUSTMENT
C
C     SUBPROGRAMS REQUIRED
C                          MULTP
C                          ADMTR
C                          TRIN  (GALS SYSTEM SUBR.)
C
C  COMMON BLOCK AREAS
C
      COMMON /COUNT/  NGRPTS, MACOL, MCCOL, MROW, IPASSC, IACCC,
     1   IPASS, NREJT, NDFRJ , IELEV ,IVRJ , MDFM ,NPTS , ISEQ , IACC
     1 , NORB
C
      ND=N1
C
C     FORMING MATRIX  N OF NORMAL EQNS
C
C     INITIALIZATION
      JK=0
C  COMPUTE SIGN OF PD
      SIGN= PD(1)/DABS(PD(1))
      DO 2 I=1,MCCOL
      DO 1 K=1,MACOL
1     AMTR2(I,K)=0.D0
      DO 2 J=1,ND
      AMTR1(I,J)=0.D0
2     CONTINUE
      DO 3 I=1,NDPY
      Y(I)=0.D0
3     PYY(I)= PY(I,I)
C
C  START STATION DO-LOOP
C
      NBO= NBI - NORB
      CALL MULTP( MROW, NORB, NORB, NRW,NDPY, C, C, AMTR1)
      CALL MULTP(MROW, NORB, 1, NRW,NRW, C,W, V)
      IST=1
      IWR=1
      M=0
      DO 8 I=1,NGRPTS
      IF(NTEST(I).NE.10) M= M+1
      IF(NGR(I).EQ.0) GO TO 8
      IF(NNGR(I).EQ.0) GO TO 50
      K= 3*M -2
      L= NGR(I)
      KC= NORB +1+ IST*NBO- NBO
      CALL MULTP( L, 3, 3, NRW, NX, A(IWR,1),A(IWR,1),ANORM(K,K))
C                                     ANORM =A[A
C
C     ANORM  MATRIX OF NORMAL EQ[NS OF ORBITAL  MODE
C                                     ANORM =PX+A[A
C     FORM U - TEM
C
      CALL MULTP( L, 3, 1 ,NRW, NX, A(IWR,1), W(IWR), U(K) )
      KO= NORB +1
      CALL MULTP(L,NORB, NBO,NRW,NDPY, C(IWR,1 ),C(IWR,KO),AMTR1(1,KC))
      CALL MULTP(L,NBO , NBO,NRW,NDPY, C(IWR,KO),C(IWR,KO),AMTR1(KC,KC))
      JWR= 1
      JST= 1
      MJ= 0
      DO 13 J=1,NGRPTS
      IF(NTEST(J).NE.10) MJ= MJ +1
      IF(NGR(J).EQ.0) GO TO 13
      IF(NNGR(J).EQ.0) GO TO 51
      KJ= 3*MJ -2
      LJ= NGR(J)
      KCJ= NORB +1 +JST*NBO - NBO
C
C    FORM       P D (D P D + PS)-1 D P
C
      CALL MLTP2(P,PSC,IM,JM,   NO,DTS, MXDP, L ,LJ, KDPSI(1,I),KD
     1PSI(1,J),DT(1,I),DT(1,J), LOK(I),LOK(J), LOKMN, PINT , PD(I), PD(J
     2 ),XLO )
C
      IR= IWR + IM
      JR= JWR + JM
C          AMTR1=CT P C
      CALL MLTP4(C( IR, 1),PINT    ,C( JR,1),AMTR1, NORB,NORB,  NRW,NRW,
     1 NDPY,NO, -1.D0)
      CALL MLTP4(C( JR,1),PINT    , C(IR,KO), AMTR1(1,KC),NORB,NBO,NRW,
     1NRW,NDPY,NO, -1.D0)
      IF(J.LT.I)  GO TO 4
      CALL MLTP4(A(IR,1),PINT    ,A(JR,1),ANORM(K,KJ),3,3,NRW,NRW,NX,
     1 NO, -1.D0)
      CALL MLTP4(C(IR,KO),PINT    ,C( JR,KO) ,AMTR1(KC,KCJ),NBO,NBO,NRW
     1,NRW, NDPY, NO, -1.D0)
4     JST= JST + 1
51      CONTINUE
      JWR= JWR + NGR(J)
13      CONTINUE
      IST= IST +1
50      CONTINUE
      IWR= IWR+ NGR(I)
8     CONTINUE
      DO 22 I= 1,MACOL
      DO 22 J=I,MACOL
22      ANORM(J,I)= ANORM(I,J)
      CALL ADMTR( MACOL, MACOL,NX, NX, PX, ANORM, SIGN)
      DO 10 I=1,MCCOL
      DO 10 J= I,MCCOL
10    AMTR1(J,I)= AMTR1(I,J)
C                                      U=  A[W
C                                      AMTR1=C[C
      CALL ADMTR( MCCOL, MCCOL, NDPY, NDPY, AMTR1, PY, 1.D0)
      CALL TRINN( PY, MCCOL, 2, NDPY)
       III= 1
      DO 1111 I=1,NORB
      DO 1111 J=I,NORB
      VARORB(III)= PY(I,J)
      III= III + 1
1111  CONTINUE
      DO 26 I=1,MCCOL
      V(I + NORB)= 0.D0
      DO 26 J=I,MCCOL
      AMTR1(J,I)=0.D0
      AMTR1(I,J)=0.D0
26    PY(J,I)= PY(I,J)
C                                      PY(PY+C[C)-1
      IST=1
      IWR=1
      M=0
      DO 29 I=1, NGRPTS
      IF(NTEST(I).NE.10) M= M+1
      IF( NGR(I).EQ.0) GO TO 29
      IF(NNGR(I).EQ.0) GO TO 52
      K= 3*M -2
      L= NGR(I)
      KC= NORB +1+ IST*NBO- NBO
      CALL MULTP(L,NORB, 3  ,NRW,NDPY, C(IWR, 1),A(IWR,1 ),AMTR1(1 ,K ))
      CALL MULTP(L,NBO, 3,NRW,NDPY,C(IWR,KO), A(IWR,1), AMTR1(KC,K) )
C      V= CT P W
      CALL MULTP(L     , NBO, 1, NRW, NRW, C(IWR,KO),W(IWR), V(KC) )
       JWR= 1
      JST= 1
      MJ= 0
      DO 30 J=1,NGRPTS
      IF(NTEST(J).NE.10) MJ= MJ +1
      IF(NGR(J).EQ.0) GO TO 30
      IF(NNGR(J).EQ.0) GO TO 53
      KJ= 3*MJ -2
      LJ= NGR(J)
      KCJ= NORB + 1 + JST*NBO- NBO
      CALL MLTP2(P,PSC,IM,JM,   NO,DTS, MXDP, L ,LJ,  KDPSI(1,I),KD
     1PSI(1,J),DT(1,I),DT(1,J), LOK(I),LOK(J), LOKMN, PINT , PD(I), PD(J
     2 ),XLO )
C
      IR= IWR + IM
      JR= JWR + JM
C   FORM AT P D(DT P D  + PS)-1DT P A
C
      CALL MLTP4(C( JR, 1),PINT    ,A(IR,1),AMTR1(1,K),NORB,3,NRW,NRW,
     1 NDPY,NO, -1.D0)
      CALL MLTP4(C( IR,KO),PINT    ,A( JR,1),AMTR1(KC,KJ),NBO,3,NRW,NRW,
     1 NDPY,NO, -1.D0)
      CALL  MLTP4(C( JR,1),PINT    ,W( IR),  V    , NORB,1,NRW,NRW,NRW,
     1NO, -1.D0)
      CALL MLTP4(C( IR,KO), PINT    , W( JR),  V(KC), NBO,1,NRW,NRW,NRW,
     1NO, -1.D0)
      CALL MLTP4(A( IR,1),PINT    , W(JR),      U(K), 3,1,NRW,NRW,NX,
     1 NO, -1.D0)
      JST= JST +1
53      JWR= JWR + LJ
30      CONTINUE
      IST= IST +1
52      CONTINUE
      IWR= IWR+ NGR(I)
29    CONTINUE
C
C                                      AMTR1=C[A
      CALL MULTP( MCCOL, MCCOL, MACOL, NDPY, ND, PY, AMTR1, AMTR2)
C                                      AMTR2=(PY+C[C)-1C[A
      DO 5 I=1,MACOL
      DO 5 J=1,MACOL
      RES=0.D0
      DO 6 K=1,MCCOL
6     RES= RES +AMTR2(K,I)* AMTR1(K,J)
5     ANORM(I,J)= ANORM(I,J) -SIGN*RES
C                                      ANORN=A[C(  "  )-1C[A+A[A+PX
C
C     ANORM - REDUCED  NORMALS
C
      DO 27 I=1,MACOL
      DO 27 J=1,MCCOL
27    U(I)= U(I) -SIGN* AMTR2(J,I)*V(J)
C                                U= A"W  - A"C( PY + C"C)-1 C"W
C     U     - ABSOLUTE TERM
C    SAVE  NORMALS
      DO 28 I=1,MACOL
      DO 28 J=1,MACOL
28    AMTR2(I,J)= ANORM(I,J)
      CALL TRINN(ANORM,MACOL,2,NX)
C
C     THE WEIGHT COEFICIENT MATRIX (THE INVERSE OF NOR. EQN MATRIX)
C
C     SOLUTION FOR  X- VECTOR
      DO 9 I=1,MACOL
      DO 25 J=I,MACOL
25    ANORM(J,I)= ANORM(I,J)
      X(I)=0.D0
      DO 9 J=1,MACOL
9     X(I)=X(I)-ANORM(I,J)*U(J)
C                                      X=-ANORM-1 U
C     COMPUTE NUISANCE PARAMETERS ]Y
C
      DO 12 I=1,MCCOL
      DO 11 JA=1,MACOL
11    V(I)=V(I)+AMTR1(I,JA)*X(JA)
C                                      V = C[W   +C[AX
12    CONTINUE
      CALL  MULTP( MCCOL, MCCOL, 1, NDPY,NDPY, PY, V, Y)
      DO 14 I=1, MCCOL
14    Y(I)= -Y(I)
C                                      Y = -(PY+C[C)-1(C[W+C[AX)
C     COMPUTE VECTOR  V
      IST=1
      IWR=1
      M=0
      DO 17 K=1,NGRPTS
      IF(NTEST(K).NE.10) M= M+1
      IF(NGR(K).EQ.0) GO TO 17
      IF(NNGR(K).EQ.0)    GO TO 54
      IWRE= IWR+ NGR(K)-1
      KK= 3*M - 3
      KKK=  + NBO*IST -NBO
      DO 16 I=IWR, IWRE
      V(I)=W(I)
      DO 15 J=1,3
15    V(I)= V(I) +A(I,J)*X(J+KK)
      DO 24 J=1, NORB
24     V(I)= V(I) + C(I,J)*Y(J)
      DO 16 J= KO, NBI
16    V(I)= V(I)+ C(I,J)* Y(KKK+J  )
      IST= IST +1
54      IWR= IWR + NGR(K)
17    CONTINUE
C                                      V = AX+ CY+W
C
C     COMPUTE AND PRINT THE SQUARE SUM OF RESIDUALS
C
      DO 18 I=1,MROW
18     VPV1= VPV1+ SIGN*V(I)*W(I)
C                                      VPV1=-W[*K
C   COMPUTE STATION AND SAT RESIDUALS V AND VS (IN PINT)
      CALL RESID(P,PSC,MXDP, LOK,DT,PD,NSAT,DTS,NGRPTS,V,PINT,NGR,KDPSI,
     1 LOKMN, XLO)
      DO 19 I=1,MROW
19    VPV= VPV+ SIGN* V(I)**2
      XPX=0.D0
      DO 21 I=1,MACOL
      U(I)= 0.D0
      DO 20 J=1,MACOL
20      U(I)= U(I) + X(J)*PX(I,J)
21      XPX= XPX + U(I)*X(I)
      YPY=0.D0
      DO 23 I=1,MCCOL
23    YPY= YPY+ PYY(I)*Y(I)**2
      VPV= VPV+ SIGN*(XPX+YPY)
C                                      VPV=V[PV+X[PX X
      RETURN
      END
      SUBROUTINE ADMTR( NR, NC, ND1, ND2, A, B, SGN)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION:  9/24/82                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C     BOTH MATRICES MUST BE COMPATABLE,I.E. THE SAME DIMENSION
C
C     SGN=  SIGN  B= A+SNG*B
C     ND2  -  ROW  DIMENSION OF B
C     ND1  -  ROW  DIMENSION OF A
      DIMENSION A(1), B(1)
      KA=-ND1
      KB=-ND2
      DO 1 J=1,NC
      KA=KA+ND1
      KB=KB+ND2
      DO 1 I=1,NR
      IA=KA+I
      IB=KB+I
1     B(IB)= A(IA)+ SGN*B(IB)
      RETURN
      END
      SUBROUTINE CARGEO (X,Y,Z,ALAT,ALOG,H,RAN,A,ESQ)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C     DSQRT(XX)=SQRT(XX)
C     DATAN(XX)=ATAN(XX)
C     DSIN(XX)=SIN(XX)
C     DABS(XX)=ABS(XX)
C     DCOS(XX)=COS(XX)
      PI=3.1415926535D00
      D=Z/(DSQRT(X**2+Y**2))
      D2= D*D/(1.000000D00-ESQ)/(1.000000D00-ESQ)
      B= D2/(1.000000000D00+ D2)
      RAN= A/DSQRT(1.0000000D00-ESQ*B)
      B= DSQRT(B)
       H= DSQRT(X**2+Y**2+ (DABS(Z)+ RAN*ESQ*B)**2)- RAN
      EO2=  ESQ/(1.0000D00+ H/RAN)
      DD= D/(1.000000D00- EO2)
       ALAT= DATAN(DD)
1     BLAT=ALAT
      D2= DD**2
      B=DSQRT(D2 /(1.00000000000D00+D2 ) )
      RAN=A/ DSQRT(1.000000D 00- ESQ*B**2)
       H= DSQRT(X**2+Y**2+ (DABS(Z)+ RAN*ESQ*B)**2)- RAN
      EO2=  ESQ/(1.0000D00+ H/RAN)
      ALAT= D/(1.000000D00- EO2)
      DD= ALAT
      ALAT=DATAN(ALAT)
      IF(DABS(ALAT-BLAT).LE.2.5D-10) GO TO 2
      GO TO 1
2     ALOG=DATAN(Y/X)
      IF (ALOG) 3,4,4
3     IF (Y.LT.0.D0) ALOG=2.D00*PI+ALOG
      IF (Y.GT.0.D0) ALOG=PI+ALOG
      GO TO 5
4     IF (Y.LE.0.D0) ALOG=PI+ALOG
5     RETURN
      END
      SUBROUTINE CENTER(X, GEOCO, A1, ESQ, NST , SHFT, SGN)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP 5.03 (NOV81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C      SUBROUTINE CENTER
C     USING 1ST ORDER DIFFERENTIAL FORMUALE (EXACT FOR HEIGHT)
C     COMPUTES LAT, LOG AND HEIGHT FOR THE MONUMENT  FROM ECC POSITION
C    AND VICE VERSA
C     SGN =1  FROM MONUMENT TO ANTENA   =-1 THE OPPOSITE  WAY
      DIMENSION X(6,NST), GEOCO(3, NST), SHFT(3,NST)
      DO 1 I=1,NST
      DCSF=DCOS(GEOCO(1,I))
      DCSL=DCOS(GEOCO(2,I))
      DSSL=DSIN(GEOCO(2,I))
      DSSF=DSIN(GEOCO(1,I))
      W=DSQRT(1.000000D 00- ESQ*DSSF**2)
      RN= A1/W + GEOCO(3,I)
      RM= RN* (1.000000D 00 - ESQ)/W**2
      GEOCO(3,I)= GEOCO(3,I) + SGN* SHFT(3,I)
      GEOCO(1,I)= GEOCO(1,I) + SGN* SHFT(1,I) /RM
      GEOCO(2,I)= GEOCO(2,I) + SGN* SHFT(2,I) /(RN* DCSF)
C  X - CENTERING
      X(1,I)= X(1,I) +(-SHFT(1,I)*DSSF*DCSL-SHFT(2,I)*DSSL+SHFT(3,I)*
     1DCSF*DCSL)* SGN
      X(2,I)= X(2,I) +(-SHFT(1,I)* DSSF*DSSL + SHFT(2,I)*DCSL +SHFT(3,I)
     1 *DCSF*DSSL)*SGN
      X(3,I)= X(3,I) +( SHFT(1,I)*DCSF +SHFT(3,I)*DSSF)* SGN
1     CONTINUE
      RETURN
      END
       SUBROUTINE CHEBY (CH,DC,M,N,X,XP)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/09/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "CHEBY     " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - MARCH /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -N       ,X       ,CH      ,M
C                             XP      ,DC      ,        ,
C
C         VARIABLES ALTERED  -CH      ,DC      ,        ,
C
C         VARIABLES RETURNED -CH      ,DC      ,        ,
C
C         EXTERNAL ROUTINES  -FLOAT   ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
C   [CHEBY[ COMPUTES N-1ST CHEBYSHEV POLYNOMIALS
C           FOR ARGUEMENT [X[ IN VECTOR [CH[.
C           DERIVATIVE POLYNOMIALS IN [DC[ FOR
C           [X[ DERIVATIVE [XP[ IF [J[ = 1.
C
C
       DIMENSION DC(N),CH(N)
       IF (N) 10,10,20
   10  RETURN
C
   20  CH(1) = 1.D0
       CH(2) = X
       IF (N.LT.3) RETURN
       TX = X + X
           DO 100 I = 3,N
               CH(I) = CH(I-1) * TX - CH(I-2)
  100      CONTINUE
       IF (M.NE.1) RETURN
C
       DC(1) = 0.D0
       DC(2) = XP
       DC(3) = (TX + TX) * XP
       IF (N.LT.4) RETURN
           DO 200 I = 4,N
               J = I - 1
               K = I - 2
               DC(I) = DC(J) *DBLE(J) /DBLE(K) * TX
     ]               - DC(K) *DBLE(J) /DBLE(K-1)
  200      CONTINUE
       RETURN
C
       END
      SUBROUTINE CONTCD( N1, N2, GEO, PHWTM, X, VPV, FOFFS, NTEST,
     1SNAME, IN, SHIFT, RCRMX, NDLY, MGR )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*10 SNAME
      CHARACTER*1 IYES
C   VERSION: 01/28/83                  GEODOP 5.12 (POLISH)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C        WRITTEN BY P. LAWNIKANIS   NOV 22/73
C                        MODIFIED   DEC 17/73
C                      REMODIFIED   JAN 21/74
C                    REREMODIFIED   FEB  1/74
C                  REREREMODIFIED   FEB  7/74
C                REREREREMODIFIED   FEB 12/74
C
C
C  THIS ROUTINE WILL READ A .GEODOP.
C    CONTINUATION FILE FROM CARD INPUT OR .TAPE8. -
C
C USE THE SUMMARY PAGES FROM THE LAST
C   GOOD .GEODOP. RUN TO PREPARE THE DECK -
C
C
C A FORMAT GUIDE FOLLOWS .....
C           .I. = AN INTEGER DIGIT
C
C........1.........2.........3.........4.........5.........6.........7..
C*1*     I       III       III       III       III       III      IIIISF
C NO.STNS/NO.PASSES/ RJT-99PC/  RJT-ZDF/ RJT-LOEL/ DOP-99PC/TOT NO.DF/
85    FORMAT(1H0,A10,1X,I2,2F3.0,I5,F4.1,F8.1,F4.1,F4.0,F3.0,2X,2(2I3,F7
     1.3,1X),
     1 F7.1,2F6.1,F5.1,1X,2F11.1/122X,F11.1)
C
C
       COMMON /COUNT/ NSTNS,K,II,IJ,IK,IL,NPASS,N99PR,NZDFR,
     $                NLELR,ND99R,NFREE,NNTNS,NNNNS,IACCP
     1 , NORB
      COMMON /OPT/ AA,BB,CC,DD,EE,HH,HI,HJ,HK,HL,HN,OO,PP,QQ,SS,
     1 TT, ZZ, UU, VV, WW, XX, JJ, KF, LL, MM, NN, NNNN
      DIMENSION FOFFS(8,N1),NTEST(1),PHWTM(N2,N2), RCRMX(1), GEO(3,    N
     11), SHIFT(3,N1), SNAME( 1), X(6,N1),NDLY(1)
      DIMENSION MGR(1)
C      INTEGER SNAME
C--- SETUP OF 'TAPE' NAMES FOR IBM VERSION.
      DIMENSION ITAPE8(2),ITAPE9(2),NU(2),INPUT(2)
      DATA ITAPE8(1),ITAPE8(2),ITAPE9(1),ITAPE9(2),INPUT(1),INPUT(2)
     @     /' TAP','E8  ',' TAP','E9  ',  ' INP','UT  '  /
C--- SET UP I/O UNIT NUMBERS.
      IOUT=6
      IF99=5
      IF8=8
      IF9=9
C
C  INPUT CARD CONTINUATION FILE .....
C
C
C   ... NO. OF - STATIONS,PASSES,99 P.C. REJECTIONS,ZERO D.F.,LO EL. REJ
C                DOPPLERS REJECTED,TOTAL D.F.,AND VPV ...
      DO 28 I=1,N1
28    MGR(I)= N1
C
C  Input Card #3 (see GEODOPV Users's Guide p.9 for more details)
       WRITE(ISCN,'(A\)') ' # of previous    passes [0] :'
       READ(ISCN,*) NPASS
       WRITE(ISCN,'(1X,I6)')NPASS
       WRITE(ISCN,'(A\)') ' # of reject(99%) passes [0] :'
       READ(ISCN,*) N99PR
       WRITE(ISCN,'(1X,I6)')N99PR
       WRITE(ISCN,'(A\)') ' # of reject(0df) passes [0] :'
       READ(ISCN,*) NZDFR
       WRITE(ISCN,'(1X,I6)')NZDFR
       WRITE(ISCN,'(A\)') ' # of reject(elv) passes [0] :'
       READ(ISCN,*) NLELR
       WRITE(ISCN,'(1X,I6)')NLELR
       WRITE(ISCN,'(A\)') ' # of reject(99%) Dopplers[0]:'
       READ(ISCN,*) ND99R
       WRITE(ISCN,'(1X,I6)') ND99R
       WRITE(ISCN,'(A\)') ' # of degrees of freedom [0] :'
       READ(ISCN,*) NFREE
       WRITE(ISCN,'(1X,I6)') NFREE
       WRITE(ISCN,'(A\)') ' Square sum of residuals [0] :'
       READ(ISCN,*) VPV
       WRITE(ISCN,'(1X,F10.2)') VPV
      IF(NSTNS.EQ.0) NSTNS=N1
      NU(1)=INPUT(1)
      NU(2)=INPUT(2)
C
C   ... NAME,TEST,AND X Y Z FOR EACH STATION .....
C
C     NNTNS - NO. OF ACTIVE STATIONS (NTEST.NE.10)
      NNTNS =0
           DO 100 I=1,NSTNS
C  Input Card #4 (see GEODOPV Users's Guide p.10 for more details)
       WRITE(ISCN,'(A\)') ' Station name (A10)             [blank] :'
       READ(ISCN,'(A)') SNAME(I)
       WRITE(ISCN,'(1X,A10)') SNAME(I)
       WRITE(ISCN,'(A\)') ' Station code(0-recFr,1-satF,10-del)[0] :'
       READ(ISCN,*) NTEST(I)
       WRITE(ISCN,'(1X,I6)') NTEST(I)
       WRITE(ISCN,'(A\)') ' Station coordinates(m) ( X Y Z)[0 0 0] :'
       READ(ISCN,*) (X(J,I),J=1,3) 
       WRITE(ISCN,'(1X,3F12.2)') (X(J,I),J=1,3)
       WRITE(ISCN,'(A\)') ' Antenna offsets (m) (N E Up)   [0 0 0] :'
       READ(ISCN,*) (SHIFT(J,I),J=1,3) 
       WRITE(ISCN,'(1X,3F5.0)') (SHIFT(J,I),J=1,3)
       WRITE(ISCN,'(A\)') ' Dopp sigmas(cm) (Phase,Range r.)[2 15] :'
       READ(ISCN,*) PSS, RRS
       WRITE(ISCN,'(1X,2F5.0)') PSS,RRS
       WRITE(ISCN,'(A\)') ' Receiver delay & sigma (æs)   [600 50] :'
       READ(ISCN,*) NDLY(I),FOFFS(5,I)
       WRITE(ISCN,'(1X,I5,F5.0)') NDLY(I),FOFFS(5,I)
       FOFFS(5,I)= FOFFS(5,I)/100.
       WRITE(ISCN,'(A\)') ' Receiver freq off & sigma (Hz) [0 0.2] :'
       READ(ISCN,*) FOFFS(1,I), FOFFS(2,I) 
       WRITE(ISCN,'(1X,2F5.0)') FOFFS(1,I), FOFFS(2,I)
       FOFFS(2,I)= FOFFS(2,I)*60.D0
       WRITE(ISCN,'(A\)') ' Freq drift & sigma (10**10/day) [0 15] :'
       READ(ISCN,*) FOFFS(3,I), FOFFS(4,I) 
       WRITE(ISCN,'(1X,2F5.0)') FOFFS(3,I), FOFFS(4,I)
C
C   CALCULATE LATITUDE,LONGDITUDE,AND ELEVATION FROM X Y Z .....
C
      TST= X(1,I) +X(2,I) +X(3,I)
      IF(IN.EQ.8.OR.IN.EQ.9.AND.TST.EQ.0.D0)GO TO 250
       CALL CARGEO (X(1,I), X(2,I), X(3,I), GEO(1,I), GEO(2,I), GEO(3,I)
     1 ,YY, DD, EE)
250    CONTINUE
      IF(NTEST(I).EQ.10)  GO TO 27
C  SET ACTIVE STATION INDEX MGR
      NNTNS= NNTNS +1
      MGR(NNTNS)= I
27    CONTINUE
C    SET DEFAULT VALUES WEIGHTS OF DOPPLERS, DF, TIM. BIAS
C ASSIGN DEFAULT SIGMA
      IF(PSS.LT.1.D-03) PSS=2.D0
      IF(RRS.LT.1.D-03) RRS= 15.D0
      RCRMX(I)= PSS*1.D 04+ RRS
      IF(FOFFS(2,I).LE.1.D-06) FOFFS(2,I)= 12.D0
      IF(FOFFS(4,I).LE.1.D-06) FOFFS(4,I)=15.D0
      IF(NDLY(I).EQ.0)  NDLY(I)= 600
      IF(FOFFS(5,I).EQ.0.D0) FOFFS(5,I)=  .5D0
  100      CONTINUE
      IACCP= NPASS-N99PR-NZDFR- NLELR
      K= NNTNS * 3
C
C   ... UPPER TRIANGULAR ELEMENTS OF PHASE WEIGHT MATRIX ...
C
C   Input Card #5 (Users Guide p.12)
       WRITE(ISCN,'(A\)') ' Input station VARIANCES ? (y/n)    [n] :'
       READ(ISCN,'(A)',END=220) IYES
       WRITE(ISCN,'(1X,A1)') IYES
       IF(IYES.EQ.'Y'.OR.IYES.EQ.'y') THEN 
       WRITE(ISCN,'(A\)') ' X Y Z var.(m**2)(^Z ends)[625 625 ...] :'
       READ(ISCN,*,END=221) (PHWTM(I,I),I=1,K)
       WRITE(ISCN,'(6(1X,D11.5))') (PHWTM(I,I),I=1,K)
       ENDIF
CC
221    WRITE(ISCN,'(A\)') ' Input station COVARIANCES ? (y/n)  [n] :'
       READ(ISCN,'(A)',END=220) IYES
       WRITE(ISCN,'(1X,A1)') IYES
        IF(IYES.NE.'Y'.AND.IYES.NE.'y') GO TO 220 
210    WRITE(ISCN,'(A\)') ' Row Col NoEl (^Z to end):'
       READ(ISCN,*,END=220,ERR=220) IRW, ICOL, NOEL
       WRITE(ISCN,'(1X,3I5)')       IRW, ICOL, NOEL
       IF((IRW*ICOL*NOEL).EQ.0)  GO TO 220
       IF(IRW.GE.K.OR.ICOL.GT.K) GO TO 220
C  IRW  - STARTING ROW
C  ICOL - STARTING COL
C  NOEL - NO. OF ELEMENTS TO BE READ IN BY ROWS (UPPER TRIANGLE)
      ICNT= 0
      ICL= ICOL
      KM1= K-1
      DO 200 I=IRW,KM1
      IF(ICNT.GE.NOEL) GO TO 210
      KCL= ICL  +NOEL -ICNT  -1
      IF(KCL.GT.K) KCL= K
       WRITE(ISCN,'(A\)') ' Covariance(s) (m**2)     [0 ..] :'
       READ(ISCN,*,END=220) (PHWTM(I,J),J=ICL,KCL)
       WRITE(ISCN,'(6(1X,D11.5))') (PHWTM(I,J),J=ICL,KCL)
      ICNT= ICNT + KCL- ICL +1
      ICL= I +2
C
C   ... SET LOWER TRIANGULAR ELEMENTS ...
C
  200      CONTINUE
           GOTO 210
  220   IF(PHWTM(1,1).EQ.0.D0)PHWTM(1,1)=625.D0/XX
C
C    CHECK INPUT MODE PARAMETR
       WRITE(IOUT,115)
C
C
C   IN -  TAPE NO.  = 9 (REWOUND)  OR = 8(NOT REWOUND)
C
      IF(IN-IF8) 40,10,11
   11 IF(IN.GT.IF9) GO TO 40
      REWIND IN
C     N=7H"TAPE9"
      NU(1)=ITAPE9(1)
      NU(2)=ITAPE9(2)
      GO TO 12
C10   N= 7H"TAPE8"
   10 NU(1)=ITAPE8(1)
      NU(2)=ITAPE8(2)
C12   IOUT= IN
   12 READ(IN,END=29)
     @           NNN,MACOL,(J,I=1,NNN),(SNAME(MGR(J)),(GEO(I,MGR(J)),
     1 X(I,MGR(J) ), I=1,3), J=1,NNN),
     2   ((PHWTM(J,I),I=J,MACOL),J=1,MACOL), NFREE, VPV, NPASS, NLELR,
     2 NZDFR, N99PR, ND99R
C     IF(EOF(IOUT)) 29,12
      GO TO 12
29    IF(IN.EQ.IF9) REWIND IN
C
C   ... OUTPUT DATA TO PRINTER FOR VERIFICATION ...
C
   40 WRITE(IOUT,75)NU,NPASS,NSTNS,N99PR,NZDFR,HK,NLELR,ND99R
      WRITE(IOUT,90)
90    FORMAT(1H0,//,3X,45HST.NAME CD   STDV DELY STD.  FREQ. STD. DRIFT
     1,3X, 8HLATITUDE,8X, 9HLONGITUDE,2X,29HHEIGHT LAT/LONG/HGT ANT SHIF
     1T,3X, 1HX,10X,1HY,10X,1HZ)
           DO 300 I=1,NSTNS
C
C   CALCULATE DEG. MIN. SEC. FOR PRINTOUT .....
C
      CALL QUICK( A, B, C, GEO(1,I))
      CALL QUICK( D, E, F, GEO(2,I))
C
C  MODIFY FREQ. OFFSET(S) IF READ IN .....
C
      SGN= 1.
      IF(GEO(1,I).NE.0.D0) SGN=  GEO(1,I)/DABS(GEO(1,I))
      KK= A + .5D0* SGN
      L=B* SGN + .5D0
      C= C *SGN
      IF(GEO(2,I).NE.0.D0) SGN=  GEO(2,I)/DABS(GEO(2,I))
      M= D +.5D0* SGN
      N= E*SGN  +.5D0
      F= F *SGN
      H= GEO(3,I)
      IPS= RCRMX(I)/1000.D0
      PSS= IPS/10.D0
      RRS= RCRMX(I)- IPS*1000
      WRITE(IOUT,85)SNAME(I),NTEST(I),PSS,RRS ,NDLY(I),FOFFS(5,I),(FOFFS
     1(J,I),J=1,4), KK,L,C, M,N,F, H, (SHIFT(J,I),J=1,3), (X(J,I),J=1,3)
  300      CONTINUE
      WRITE(IOUT, 95) VPV,NFREE
      DO 206 I=1,K
      DO 206 J=I,K
206   PHWTM(J,I)= PHWTM(I,J)
           DO 400 I=1,K
      WRITE(IOUT,105) ( PHWTM(I,J),J=I,K)
  400      CONTINUE
      IF(  IN.EQ.1) GO TO 402
      CALL TRINN(PHWTM, K, 2, N2)
      DO 401 I=1,K
      DO 401 J=I,K
401   PHWTM(J,I)= PHWTM(I,J)
402   CONTINUE
      WRITE(IOUT, 25)
      WRITE(IOUT, 26)
25    FORMAT(//,37H  REMARKS- CODE=0 REC TIME FRAME(CBR)/15X,22H=1 SAT.T
     1IME FRAME( BR)/15X,22H=10 IGNORE THE STATION/11X,51HSTDV= STANDARD
     2 DEV.IN CM OF PHASE AND 30 SEC INTER.,/11X,
     129HDELY= RECEVER DELAY(1E-6 SEC)/11X,42HT.B.= TIMING BIAS CONST(1
     2SIGMA) (1E-4SEC))
26    FORMAT(1H ,  10X,35HFREQ= FREQ. OFFSET FROM 400MHZ (HZ)/
     1                                                   11X,36HSTD.= FR
     1EQ.SIGMA   HZ/MIN           /11X,30HDRIFT=FREQ. DRIFT(1E-10/DAY)
     3/2X,34HHEIGHT, ANT. SHIFT, X,Y,Z (METERS))
C  RETURN TO .GEODOP. .....
C
      CALL CENTER(X, GEO, DD, EE, NSTNS, SHIFT, 1.D0)
      RETURN
C
5     FORMAT(8X,I2,5(7X,I3),4X,I6,F10.3)
15      FORMAT(A10,I2,2F3.1  ,I4,F5.1,2F3.0,F2.0,F3.0,3F5.1,3F9.1)
35    FORMAT(6(2X,E11.5))
   75  FORMAT (     /,51X,'CONTINUATION DATA FROM ',A4,A3,/,1H+,49X,
     1////,'  TOTAL NUMBER OF PROCESSED PASSES =',I4,/,
     $                    '  NUMBER OF UNKNOWN STATIONS       =',I4,////
     $                  /,'  REJECTED ON 99 PER CENT     =',I4,/,
     $                    '  REJECTED ON ZERO DEG FRDM   =',I4,/,
     +                    '  REJECTED ON ',F4.1,' DEG MAX EL =',I4,//,
     $                    '  DOPPLERS REJECTED ON 99 PER CENT =',I4,
     1////)
95    FORMAT(//,'  VPV = ',F10.3,/'  DEGREES OF FREEDOM = ',I5,//,
     1  '    WEIGHT  COEFFICIENT MATRIX -' )
105   FORMAT(1H ,(3(3X,3E12.5)) )
  115  FORMAT (1H1)
C
       END
       SUBROUTINE COORD (TARG , TDEV , IORD , CXYZ , XSAT , XGND)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   NOTE: THE ONLY CHANGE FROM THE CDC VERSION IS THE INCREASE
C   OF THE XMET ARRAY DIMENSION FROM 200 TO 500.
C
C   FOR G1 OPERATION:
C   1. "IDUM1" ADDED TO "DAT" LABELED COMMON TO GIVE CORRECT ALIGNMENT.
C
C
C        "COORD     " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - FALL   /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -IORD    ,TARG    ,TDEV    ,CXYZ
C                             DATM    ,XGND    ,        ,
C
C         VARIABLES ALTERED  -XSAT    ,        ,        ,
C
C         VARIABLES RETURNED -XSAT    ,        ,        ,
C
C         EXTERNAL ROUTINES  -CHEBY   ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
C        [COORD[ COMPUTES SAT. STATE VECTOR "XSAT" FOR GND POINT "XGND"
C                FROM COEFFICIENTS "CXYZ" WITH CHEBYSHEV POLYNOMIAL TIME
C                BASE MATRIX , GIVEN TIME ARGUEMENT "TARG" , DERIVATIVE
C                "TDEV" , FOR DATUM SHIFT VECTOR "DATM" TO ORDER "IORD".
C
       COMMON /DAT/ ANOT , AZIM , CDOP , CELV , CPAP , DATM , DNOM ,
     +              DTIM , ECCS , ELEV , ELMX , FRCV , IRCV , METL ,
     +              METP , NDIR , TIME , XMET ,
     +                BT ,   C4 ,   CC ,   CM ,LB,IDUM1, PI ,   RD ,
     +                RH ,   SS ,   ST ,   WE ,   XP
       DIMENSION DATM(   3) , DNOM(  32) , DTIM(   4) , ELEV(  33) ,
     +           XMET(4,500)
C
       DIMENSION CHEB(  10) , DCHB(  10) , CXYZ(10,3) , XSAT(   6) ,
     +           XGND(   3)
C
       CALL CHEBY (CHEB , DCHB ,    1 , IORD , TARG , TDEV)
C
           DO 100 I = 1,3
               Y = CXYZ(1,I) - DATM(I)
               Z =        0.D0
                   DO 200 J = 2,IORD
                       Y = Y + CHEB(J) * CXYZ(J,I)
                       Z = Z + DCHB(J) * CXYZ(J,I)
  200              CONTINUE
               XSAT(I  ) = Y - XGND(I)
               XSAT(I+3) = Z
  100      CONTINUE
C
       RETURN
      END
       SUBROUTINE DDHHMM (MINI , IDHM)
C
C   VERSION:  9/24/82                  GEODOP IV+V   (JUL81)
C   CHECKED FOR RUNNING USING EITHER CDC OR IBM FORTRAN.
C    (G1 OR Q COMPILER W/WO 'AUTODBL(DBLPAD),OPT(3)' OPTIONS.)
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "DDHHMM    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - OCTOBER  /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -MINI    ,IDHM    ,        ,
C
C         VARIABLES ALTERED  -IDHM    ,        ,        ,
C
C         VARIABLES RETURNED -IDHM    ,        ,        ,
C
C         EXTERNAL ROUTINES  -*NONE*  ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
       DIMENSION IDHM(3)
C
       IDHM(1) =  MINI / 1440
       IDHM(2) = (MINI - IDHM(1) * 1440) / 60
       IDHM(3) =  MINI - IDHM(1) * 1440 - IDHM(2) * 60
       RETURN
       END
      SUBROUTINE DGEOCA (X,Y,Z,A,ESQ,ALAT,ALONG,H)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C     THIS SUBROUTINE IS IN DOUBLE PRECISION.
C     THIS SUBROUTINE CONVERTS GEODITIC COORD. INTO CARTISIAN COORD.
C     X,Y,Z_ARE THE CARTESIAN COORDINATE(OUT_PUT).
C     A AND ESQ_ARE THE ELEMENTS OF ELLIPSOID AS IN_PUT.
C     ALAT,ALONG,H_ARE THE LATITUDE ,LONGITUDE,H RESP. AS  IN_PUT.
C     DSQRT(XX)=SQRT(XX)
C     DSIN(XX)=SIN(XX)
C     DCOS(XX)=COS(XX)
      DENSQ=1.D0-ESQ*DSIN(ALAT)**2
      DEN=DSQRT(DENSQ)
      RAD=A/DEN
      X=(RAD+H)*DCOS(ALAT)*DCOS(ALONG)
      Y=(RAD+H)*DCOS(ALAT)*DSIN(ALONG)
      Z=(RAD*(1.D0-ESQ)+H)*DSIN(ALAT)
      RETURN
      END
      SUBROUTINE DIME(KTC, IDY,IHR,MIN)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   CHECKED FOR RUNNING USING EITHER CDC OR IBM FORTRAN.
C    (G1 OR Q COMPILER W/WO 'AUTODBL(DBLPAD),OPT(3)' OPTIONS.)
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C     KTC  TIM IN MIN
C     IDY  DAY NO
C     IHR  HOURS
C     MIN  MINUTES
      IDY= KTC/ 1440 +.5D0
      IHR=(KTC-IDY*1440)/60 +.5D0
      MIN=KTC-IDY*1440- IHR*60+.5D0
      RETURN
      END
       FUNCTION DIST (A , B , C)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION:  9/24/82                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "DIST      " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -A       ,B       ,C       ,
C
C         VARIABLES ALTERED  -DIST    ,        ,        ,
C
C         VARIABLES RETURNED -DIST    ,        ,        ,
C
C         EXTERNAL ROUTINES  -SQRT    ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
       DIST =DSQRT(A * A + B * B + C * C)
       RETURN
       END
      FUNCTION DRHOI(P,T,E,EL,PI,H)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
      Z=PI/2.-EL
      TZ=DSIN(Z)/DCOS(Z)
      W=16.D0*4.848D-06*TZ/T*(P+4810.D0*E/T)
      Z=Z-W
      SZ=1.D0/DCOS(Z)
      TZ=SZ*DSIN(Z)
      CONH=1.16D0-.15412D-03*H+.71633D-08*H*H
      DRHOI=.002277D0*SZ*(P+(1255.D0/T+0.05D0)*E-CONH*TZ*TZ)
      RETURN
      END
      SUBROUTINE DROTAT (ANGLE,NAXIS,NUM,ROT)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/09/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C     COMPUTES PRODUCT ROTATION MATRIX IN DOUBLE PRECISION
C     INPUTS
C     ANGLE=SET OF UP TO NINE CONSECUTIVE SINGLE AXIS ROTATIONS
C     NAXIS=SET OF AXES OF ROTATION FOR EACH ANGLE
C     NUM=NUMBER OF ROTATIONS INPUTTED
C     OUTPUT
C     ROT=3X3 PRODUCT ROTATION MATRIX
      DIMENSION ROT(3,3), R1(3,3), R2(3,3), ANGLE(NUM), NAXIS(NUM)
C     DSIN(XX)=SIN(XX)
C     DCOS(XX)=COS(XX)
C
      DO 5 K=1,NUM
C     FIND OTHER TWO AXES N2,N3
      N2=NAXIS(K)+1
      IF (N2.GT.3) N2=N2-3
      N3=N2+1
      IF (N3.GT.3) N3=N3-3
C     SET DIAGONAL ELEMENTS
      R1(NAXIS(K),NAXIS(K))=1.D0
      R1(N2,N2)=DCOS(ANGLE(K))
      R1(N3,N3)=R1(N2,N2)
C     SET NONZERO OFF-DIAGONAL ELEMENTS
      R1(N2,N3)=DSIN(ANGLE(K))
      R1(N3,N2)=-R1(N2,N3)
C     SET ZERO OFF-DIAGONAL ELEMENTS
      R1(NAXIS(K),N2)=0.D0
      R1(NAXIS(K),N3)=0.D0
      R1(N2,NAXIS(K))=0.D0
      R1(N3,NAXIS(K))=0.D0
C     IF FIRST ROTATION, SET ROT=R1
      IF (K.GT.1) GO TO 2
      DO 1 I=1,3
      DO 1 J=1,3
1     ROT(I,J)=R1(I,J)
      GO TO 5
C     IF NOT FIRST ROTATION SET R2=R1*ROT AND ROT=R2
2     DO 3 J=1,3
      DO 3 I=1,3
      R2(I,J)=0.D0
      DO 3 M=1,3
3     R2(I,J)=R2(I,J)+R1(I,M)*ROT(M,J)
      DO 4 I=1,3
      DO 4 J=1,3
4     ROT(I,J)=R2(I,J)
5     CONTINUE
      RETURN
      END

