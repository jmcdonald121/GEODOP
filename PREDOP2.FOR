      SUBROUTINE MULTP (NRA,NCA,NCB,ND1,ND2,A,B,RES)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/09/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C     SUBROUTINE COMPUTES A PRODUCT OF TWO GENERAL MATRICES  A[*B
C     NOTE  NO. OF ROWS OF  A = NO. OF ROWS OF B
C
C     ND1  -  ROW DIMENSION OF A AND B
C     ND2  -  ROW DIMENSION OF RES
      DIMENSION A(1), B(1), RES(1)
      KB=-ND1
      KR=-ND2
      DO 1 J=1,NCB
      KA=-ND1
      KB=KB+ND1
      KR=KR+ND2
      DO 1 I=1,NCA
      KA=KA+ND1
      LR=KR+I
      RES(LR)=0.D0
      DO 1 II=1,NRA
      LA=KA+II
      LB=KB+II
1     RES(LR)=RES(LR)+A(LA)*B(LB)
      RETURN
      END
       FUNCTION REDUCE (A,B,C)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 10/05/82                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   NO SUBPROGRAMS CALLED.
C
C
C        "REDUCE    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -A       ,B       ,C       ,
C
C         VARIABLES ALTERED  -REDUCE  ,        ,        ,
C
C         VARIABLES RETURNED -REDUCE  ,        ,        ,
C
C         EXTERNAL ROUTINES  -*NONE*  ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
      REDUCE=(A +(B +C/.600000000D 02)/.6000000000D 02)*.1745329252D-01
       RETURN
       END
       SUBROUTINE REJEKT (FUNC , DELT , IDNT , INDC)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/18/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   FOR G1 OPERATION:
C   1. "IDUM" ADDED TO "FIT" LABELED COMMON TO GIVE CORRECT ALIGNMENT.
C
C
C        "REJEKT    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - J.KOUBA .. PROGRAM GEODOP
C
C         VARIABLES USED     -NDOP    ,KDOP    ,INDC    ,RESD
C                             FUNC    ,DELT    ,IRJT    ,JDOP
C                             IDNT    ,NOTE    ,        ,
C
C         VARIABLES ALTERED  -IRJT    ,FUNC    ,KDOP    ,NOTE
C                             JDOP    ,        ,        ,
C
C         VARIABLES RETURNED -FUNC    ,        ,        ,
C
C         EXTERNAL ROUTINES  -ABS     ,        ,        ,
C
C         I/O DEVICES        -IPT     ,        ,        ,
C
C
C        [REJEKT[ TESTS THE RESIDUALS "RESD" OR A FUNCTION "FUNC"
C                 AND ZEROES ANY POINTS WITH DISCREPANCIES LARGER
C                 THAN "DELT" AS WELL AS UPDATE THE ERROR COUNT
C                 "IRJT". WHEN DONE , A WARNING LINE IS PRINTED.
C
       COMMON /FIT/    L ,    M ,N,IDUM, DACA , DECA , DOPC , KDOP ,
     +               DOP ,  TIM , RESD , XNOR
       COMMON /IOS/  ICD ,  INP ,  IOT ,  IPT , ISTA , ISTB , IRJT ,
     +              LOKH , LOKR , JDOP , LDOP , MDOP , NDOP
       COMMON /MJV/ IVAR , IFIX , IDAY , IHOR , IMIN , NOTE, KSAT(17)
       DIMENSION DACA(  14) , DECA(  14) , DOPC(  14) , KDOP(  48) ,
     +            DOP(  96) ,  TIM(  96) , RESD(  48) , IVAR(  14) ,
     +           IFIX(  14) , FUNC(  32) , NOTE(  32) , TEMP(  32)
C
       IRJT = 0
       J    = 0
           DO 100 I = 1,NDOP
               IF (KDOP(I).EQ.0) GO TO 100
               J          = J + 1
               IF (INDC.EQ.1) XT = RESD(J)
               IF (INDC.EQ.2) XT = FUNC(J)
               IF (DABS(XT).LE.DELT) GO TO 100
               FUNC(J)    = 0.D0
               KDOP(I)    = 0
               IRJT       = IRJT + 1
               NOTE(IRJT) = I
               TEMP(IRJT) = XT
  100      CONTINUE
       IF (IRJT.EQ.0) RETURN
       JDOP               = JDOP - IRJT
       WRITE (IPT, 5) IDNT , DELT , (NOTE(I) , TEMP(I) , I=1,IRJT)
       RETURN
C
    5  FORMAT (55X,7H***** [,A10,3H[ >,E9.3,19H REJECTED AT 99: - ,
     +             I3,E15.6,/,(103X,I3,E15.6))
       END
      FUNCTION FLTMX(IDATA)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   NOTE: CHANGES FROM CDC VERSION ARE:
C    1. USE OF CDC "SHIFT" FUNCTION ELIMINATED.
C
C   SUBPROGRAMS USED:
C    FORTRAN:        DBLE
C
C
C             FUNCTION TO CONVERT MX1502 DBLEING POINT DATA
C             INTO HOST MACHINE DBLEING POINT FORM
C
      DIMENSION IDATA(4)
      IF      (IDATA(1)+IDATA(2)+IDATA(3)+IDATA(4).EQ.0) GO TO 10
C     ITMP    =    SHIFT(IDATA(1),16)+SHIFT(IDATA(2),8)+IDATA(3)
      ITMP    =    IDATA(1)*65536+IDATA(2)*256+IDATA(3)
      IF      (ITMP.GT.8388607) ITMP=ITMP-16777216
      FLTMX   =   DBLE(ITMP)*2.D0**(IDATA(4)-151)
      RETURN
10    FLTMX   =    0.D0
      RETURN
      END
       FUNCTION MODULO (I,J)
C
C   VERSION:  9/25/82                  GEODOP IV+V   (JUL81)
C   CHECKED FOR RUNNING USING EITHER CDC OR IBM FORTRAN.
C    (G1 OR Q COMPILER W/WO 'AUTODBL(DBLPAD),OPT(3)' OPTIONS.)
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "MODULO    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -I       ,J       ,        ,
C
C         VARIABLES ALTERED  -MODULO  ,        ,        ,
C
C         VARIABLES RETURNED -MODULO  ,        ,        ,
C
C         EXTERNAL ROUTINES  -*NONE*  ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
       MODULO = I - (I - 1) / J * J
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
       SUBROUTINE DECOD1 (ISAT)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/23/83                  GEODOP 5.11 (POLISH)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   NOTE: CHANGES FROM CDC VERSION ARE:
C   1. SUBROUTINE NAME CHANGED FROM 'DECODE' TO 'DECOD1' SO
C   2. THE DIMENSION OF 'XMET' HAS BEEN CHANGED FROM '200' TO
C      '500'.
C   3. THE INITIALIZATION OF THE "FIXP" ARRAY HAS BEEN MODIFED TO
C      WORK WITH IBM OR CDC FORTRAN.
C
C   FOR G1 OPERATION:
C   4. "IDUM1" AND "IDUM" ADDED TO "DAT" AND "FIT" LABELED COMMON
C      BLOCKS RESPECTIVELY TO GIVE CORRECT ALIGNMENT.
C
C
C        "DECODE    " - AUTHOR        - P.G. LAWNIKANIS
C                     - COAUTHOR      - G. MACNINTCH
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - J.KOUBA .. PROGRAM PREDOP
C
C         VARIABLES USED     -ISAT    ,NDOP    ,DNOM    ,DOP
C                             JDOP    ,MDOP    ,IPT     ,IFIX
C                             NOTE    ,IVAR    ,IMIN    ,IHOR
C                             IDAY
C
C         VARIABLES ALTERED  -ISTB    ,JDOP    ,DOP     ,KDOP
C                             LDOP    ,NDOP    ,FIXD    ,IFIX
C                             FIXP    ,IMIN    ,LOKH    ,LOKR
C                             DDMN    ,TPER    ,DECA    ,DACA
C                             DOPC
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -IQUIK   ,SFIX    ,MODULO  ,MINUT
C                             ABS     ,DBLE   ,IABS    ,INT
C
C         I/O DEVICES        -IPT     ,        ,        ,
C
C
C      CHECKS MJV PASS DATA AND DECODES PARAMETERS
C
C     SET FLAG [ISTB[ = 1 ... DATA OK
C                     = 2 ... PASS REJECTED.
C
       COMMON /DAT/ ANOT , AZIM , CDOP , CELV , CPAP , DATM , DNOM ,
     +              DTIM , ECCS , ELEV , ELMX , FRCV , IRCV , METL ,
     +              METP , NDIR , TIME , XMET ,
     +                BT ,   C4 ,   CC ,   CM ,LB,IDUM1, PI ,   RD ,
     +                RH ,   SS ,   ST ,   WE ,   XP
       COMMON /DEC/ FIXD , DDMN , TPER , LOKM
       COMMON /FIT/    K ,    M ,N,IDUM, DACA , DECA , DOPC , KDOP ,
     +               DOP ,  TIM , RESD , XNOR
       COMMON /IOS/  ICD ,  INP ,  IOT ,  IPT , ISTA , ISTB , IRJT ,
     +              LOKH , LOKR , JDOP , LDOP , MDOP , NDOP
       COMMON /MJV/ IVAR , IFIX , IDAY , IHOR , IMIN , NOTE, KSAT(17)
C
       DIMENSION DATM(   3) , DNOM(  32) , DTIM(   4) , ELEV(  33) ,
     +           XMET(500,4),  DOP(  96) , KDOP(  48) , IFIX(  14) ,
     +           IVAR(  14) ,
     +           DACA(  14) , DECA(  14) , DOPC(  14) , RESD(  48) ,
     +  NOTE( 32), TIM( 96), FIXD( 14), FIXP(14,17)
      DATA FIXP/238* -1.D0/
       IF(FIXP(14, 1).LT.0.D0) FIXP(14, 1)= 31947.D0
       IF(FIXP(14, 2).LT.0.D0) FIXP(14, 2)= 31994.D0
       IF(FIXP(14, 3).LT.0.D0) FIXP(14, 3)= 32052.D0
       IF(FIXP(14, 4).LT.0.D0) FIXP(14, 4)= 31991.D0
       IF(FIXP(14, 5).LT.0.D0) FIXP(14, 5)= 33789.D0
       IF(FIXP(14, 6).LT.0.D0) FIXP(14, 6)= 33792.D0
       IF(FIXP(14, 7).LT.0.D0) FIXP(14, 7)= 32077.D0
       IF(FIXP(14, 8).LT.0.D0) FIXP(14, 8)= 32086.D0
       IF(FIXP(14, 9).LT.0.D0) FIXP(14, 9)= 32020.D0
       IF(FIXP(14,10).LT.0.D0) FIXP(14,10)= 32009.27034D0
       IF(FIXP(14,11).LT.0.D0) FIXP(14,11)= 32000.3632D0
       IF(FIXP(14,12).LT.0.D0) FIXP(14,12)= 32000.D0
       IF(FIXP(14,13).LT.0.D0) FIXP(14,13)= 32007.3939D0
       IF(FIXP(14,14).LT.0.D0) FIXP(14,14)= 32016.9512D0
       IF(FIXP(14,15).LT.0.D0) FIXP(14,15)= 32000.D0
       IF(FIXP(14,16).LT.0.D0) FIXP(14,16)= 32000.D0
       IF(FIXP(14,17).LT.0.D0) FIXP(14,17)= 33791.9914D0
       ISTB = 2
       JDOP = 0
       N    = 0
C
C   CHECK IF ISAT IS A LEGITIMATE SAT NO.
        DO 9 I=1,17
        IF(ISAT.EQ.KSAT(I)) GO TO 8
 9      CONTINUE
        GO TO 50
 8      K=I
C
C      CHECK + DECODE DOPPLERS-
C
           DO 100 I = 1,NDOP,2
               J    = 1
               L    = I / 2 + 1
               DT   = DNOM(L)
               DMAX = DT * 2.48D+6
               DMIN = DT * 1.36D+6
               DOPH = DOP(I  )
               DOPL = DOP(I+1)
               IF (DOPH.LE.DMIN.OR.DOPH.GE.DMAX) J = 0
               IF (DOPL.LT.DMIN.OR.DOPL.GE.DMAX) J = 0
               DDOP = DOPH - DOPL
               IF (DABS(DDOP).GT.(DT * 458.33333D0)) J = 0
               DOP(L)  = (DOPH + DDOP * .16363636D0) *DBLE(J)
      IONOS= DDOP*.163636D0*J*10.00000D0
      DOP(L)= DOP(L) + IONOS*1.00000000D 07
               JDOP    = J + JDOP
               KDOP(L) = J
  100      CONTINUE
       LDOP = L / 2
       NDOP = L - L / 32 * (L - 32)
       IF (JDOP.GE.MDOP) GO TO 10
       WRITE (IPT, 5) JDOP
       RETURN
C
C      CHECK + DECODE FIXED PARAMETERS-
C
   10  M = IFIX(12)
       IF (M.LT.800000000.OR.M.GT.880000000) GO TO 11
       I = M - 800000000
       FIXD(12) =DBLE(IQUIK(I,10000,10) * 1440 + I / 50000)
       IF (FIXD(12).EQ.FIXP(12,K)) GO TO 13
      IFIX(14)= IFIX(14) -800000000
      DO 200 I= 2,13
               M = IFIX(I)
               IF (M.GE.800000000.AND.M.LT.910000000.AND.(I/2).EQ.4)
     +                                                         GO TO 200
               IF (M.LE.790000000.OR.M.GT.880000000) GO TO 40
  200      IFIX(I) = M - 800000000
C
       FIXD( 1) =DBLE(IQUIK(IFIX(1),100000000,1) + IFIX(1) / 400000000
     +               * 100000000) * 1.D-5
       FIXD( 2) =(DBLE(IFIX( 2)) * 1.D-8 + 3.D0) * RD
       FIXD( 3) =DBLE(IFIX( 3)) * 1.D-5       * RD
       FIXD( 4) =DBLE(IFIX( 4)) * 1.D-8       * RD
       FIXD( 5) =DBLE(IFIX( 5)) * 1.D-7
       FIXD( 6) =DBLE(IFIX( 6))
       FIXD( 7) =DBLE(IFIX( 7)) * 1.D-5       * RD
       FIXD( 8) = SFIX (IFIX( 8)) * 1.D-8       * RD
       FIXD( 9) = SFIX (IFIX( 9)) * 1.D-7
       FIXD(10) =DBLE(IFIX(10)) * 1.D-5       * RD
       FIXD(11) =DBLE(IQUIK(IFIX(11),10000,100))
       FIXD(13) =DBLE(IFIX(13)) * 1.D-7
       FIXD(14) =DBLE(IFIX(14)) * 4.D-5 + 32000.D0
      IF(DABS(FIXD(14)-FIXP(14,K)).GT.300.D0) FIXD(14)= FIXP(14,K)
           DO 270 J = 1,14
               FIXP(J,K) = FIXD(J)
  270      CONTINUE
       IF (ISAT.NE.IDINT(FIXD(11))) WRITE (IPT,65)
       GO TO 12
C
   11  IF (FIXP(12,K).LT.0.D0) GO TO 40
   13      DO 280 J = 1,14
               FIXD(J) = FIXP(J,K)
  280      CONTINUE
C
C      DECODE + SET VARIABLE PARAMETER DATA-
C
   12  LOKM = (IQUIK(IVAR(7),100000000,10000000) + IVAR(7) / 400000000
     +         * 10) * 2 + 23
       LOKM = MODULO (LOKM , 30) - 1
      MMMM= MODULO(IMIN +1, 30) - 1
      IF(IRCV.EQ.0.AND.LOKM.NE.MMMM) WRITE(IPT, 55)
       I    = IMIN - LOKM
       A    = I + 14
       IHOR = IHOR + (A + .1D0) / (A - .1D0) - 1
      IMIN= IABS(I)/16* 30 +LOKM- IABS(I)/32*30
       LOKH = MINUT (   0 , IHOR , IMIN)
       LOKR = MINUT (IDAY , IHOR , IMIN)
       DDMN = FIXD(2) + FIXD(2)
       TPER =DBLE (LOKH) - FIXD(1)
       J    = IQUIK (IMIN , 4 , 2)
       L    = 14
           DO 300 I=1,14
               B = 2000.D0
               C = 2000.D0
               M = IVAR(I)
               IF (M.GT.000000000.AND.M.LT.750000000) GO TO 20
               IF (M.NE.0) WRITE (IPT,35) I , M
               B = 0.D0
               L = L - 1
               M = 0
   20          IDF = M / 100000000
C
               ISE = IQUIK(M,10000000,10000) * (1 - IQUIK(IDF/2,2,1) *2)
      IF(IABS(ISE).GT.500) C=0.D0
               DECA(I) =DBLE(ISE) + B + C
C
               C   = 2000.D0
               ISA = IQUIK(M,10000   ,10   ) * (1 - IQUIK(IDF  ,2,1) *2)
               IF (IABS(ISA).GT.500) C = 0.D0
               DACA(I) =DBLE(ISA) + B + C
C
               C         = 2000.D0
               IF (J.EQ.0) GO TO 30
               ISE       = IQUIK(N,10,1)
               ISA       = ISE / 5 * 2 - 1
               ISE       = IQUIK(IABS(ISE - 5),5,1)
               DT        =DBLE((ISE * 10 + IQUIK(M,10,1)) * ISA)
      IF(N.EQ.0.OR.M.EQ.0) C=0.D0
               DOPC(I-1) = DT + B + C
   30          J         = IABS(J - 1)
  300      N = M
C
       IF (L.GE.6) GO TO 60
       WRITE (IPT,45) L
       RETURN
C
   40  WRITE (IPT,25) I , M
       RETURN
   50  WRITE (IPT,75)
       RETURN
   60  ISTB = 1
       RETURN
C
    5  FORMAT (55X, 5H*****,I2,31H DOPPLERS - PASS REJECTED *****)
   25  FORMAT (55X,24H***** FIXED PARAMETER # ,I2,4H , [,I9,
     +             36H[ OUT OF RANGE - PASS REJECTED *****)
   35  FORMAT (55X,24H***** VARBL PARAMETER # , I2,4H , [,I9,
     +            36H[ OUT OF RANGE - PARM REJECTED *****)
   45  FORMAT (55X, 5H*****,I2,37H VARIABLE PARAMETERS - PASS REJECTED ,
     +             5H*****)
   55  FORMAT (55X,46H***** WARNING - MJV/MESSAGE TIMES DIFFER *****)
   65  FORMAT (55X,37H***** WARNING - SAT #S DISAGREE *****)
   75  FORMAT (5X,41H***** INVALID SAT # - PASS REJECTED *****)
       END
       FUNCTION DEFALT (A , B)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/09/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "DEFALT    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -A       ,B       ,        ,
C
C         VARIABLES ALTERED  -A       ,DEFALT  ,        ,
C
C         VARIABLES RETURNED -DEFALT  ,        ,        ,
C
C         EXTERNAL ROUTINES  -*NONE*  ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
       IF (A.EQ.0.D0) A = B
       DEFALT = A
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
      SUBROUTINE  DVDD(X,POTT,POTX,POTY,POTZ,AE,RMU,C,T,Q, QL, MXD)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/28/83                  GEODOP 5.12 (POLISH)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   SUBROUTINES COMPUTES POTENTIALS AND  DERIVATIVES POTX, POTY, POTZ
C   W/R TO X,Y Z
C    MODIFIED FROM A SUBROUTINE WRITTEN BY H. HAUCK OF IFAG
C
      DIMENSION
     * C(1), T(1), Q(1), QL(1), X(3)     ,SINML(31), COSML(31)
C
C     DARSIN(X) = ASIN(X)
C     DARCOS(X) = ACOS(X)
C     DATAN2(X,Y) = ATAN2(X,Y)
C     DSQRT(X) = SQRT(X)
C      DBLE(X) = DBLE(X)
C     DATAN(X) = ATAN(X)
C     DTAN(X) = TAN(X)
C     DSIN(X) = SIN(X)
C     DCOS(X) = COS(X)
C KUGELFUNKTIONSREIHE MIT MAX. INDEX N = LGRENZ-1 IN DEN
C SPHERICAL HARMONIC SERIES WITH MAXIMUM INDEX N=LIMIT-1 . . .
      R=DSQRT(X(1)**2+X(2)**2+X(3)**2)
      RH=DSQRT(X(1)**2+X(2)**2)
      MX2= MXD*2
      I22= MXD +1
      THI= X(3)/R
C INITILIZE SINML AND COSML
      SINML(1)= 0.D0
      SINML(2)=  X(2)/ RH
      COSML(1)= 1.000000D 00
      COSML(2)= X(1)/ RH
      AER=AE/R
C     Q- LEGENDRE POLYN   QL- DERIVATIVES W.T. SIN(THI)
      Q(1)  =1.0000000D 00
      Q(2)  =THI
      Q(I22)=DSQRT(1.D0-THI*THI)
      TPHI=Q(2)  /Q(I22)
      DO 11 L=3,MXD
      LI=L-1
C  COMPUTE SINML AND COSML
      SINML(L)= SINML(2)*COSML(LI) + COSML(2)*SINML(LI)
      COSML(L)= COSML(2)*COSML(LI)- SINML(2)*SINML(LI)
      LU=L-2
      ZL=DBLE(L)
      ZLI=DBLE(LI)
      ZLU=DBLE(LU)
      Q(L)  =((2.D0*ZL-3.D0)*Q(2)  *Q(LI)  -Q(LU)  *ZLU)/ZLI
                 LL = (L -1)*(MX2-(L -2))/2     +1
                 LLI= (LI-1)*(MX2-(LI-2))/2     +1
      Q(LL )=Q(I22)*Q(LLI  )*(2.D0*ZLI-1.D0)
      DO 12 M=2,LI
      MI=M-1
      MM= MI*(MX2-(MI-1))/2
      LM= MM+L-MI
      LUM= MM+ LU-MI
      LIMI= (MI-1)*(MX2-(MI-2))/2 +LI-MI+1
      Q(LM )=        Q(LIMI )*Q(I22)*(2.D0*ZLI-1.000000D 00)
      IF(LU.GT.MI)   Q(LM)= Q(LM)+ Q(LUM)
   12 CONTINUE
C   DERIVATIVES OF LEGENDRE POLYN
      DO 15 M=1,L
      MO=M+1
      MI=M-1
      LM= MI*(MX2-(MI-1))/2 + L-MI
      LMO=(MO-1)*(MX2-(MO-2))/2 +L-MO+1
      ZMI=DBLE(MI)
      QL(LM )= -Q(LM )*TPHI*ZMI /Q(I22)
      IF(MO.LE.L) QL(LM)= QL(LM)+ Q(LMO)/Q(I22)
   15 CONTINUE
   11 CONTINUE
C   COMPUTE POTENTIAL AND DERIVATIVES
      ABR=0.D0
      POTT=0.D0
      ABLP=0.D0
      ABLL=0.D0
      DO 13 L=3,MXD
      LI=L-1
      SUM=0.D0
      SUM1=0.D0
      SUM2=0.D0
      DO 14 M=1,L
      MI=M-1
      ZMI=DBLE(MI)
      LM= MI*(MX2-(MI-1))/2+L-MI
      DC= COSML(M)
      DS= SINML(M)
      SUM=SUM+Q(LM )*(C(LM  )*DC+T(LM  )*DS)
C  DERIVATIVES  DV/DSIN,THI.
      SUM1=SUM1+QL(LM )*(C(LM  )*DC+T(LM  )*DS)
C   DERIVATIVES DV/DLAMDA
      SUM2=SUM2+Q(LM )*(T(LM  )*DC-C(LM  )*DS)*ZMI
   14 CONTINUE
      VOR=AER**LI/R
C POTENTIAL
      POTT=POTT+SUM*VOR
C R*DV/DR
      ABR=ABR-SUM*VOR*DBLE(L)
C ABLEITUNGEN NACH SIN(PHI) UND LAMDA
C DERIVATIVE WITH RESPECT TO SIN(PHI) AND LAMDA.
      ABLP=ABLP+SUM1*VOR
      ABLL=ABLL+SUM2*VOR
   13 CONTINUE
      POTR=(-1.D00/R+ABR) /R
      POT= RMU  *.3600000D 04
      POTX=POT*(POTR*X(1)/R-ABLP*X(3)*X(1)/R/R/R- ABLL*X(2)/RH/RH)
      POTY=POT*(POTR*X(2)/R-ABLP*X(3)*X(2)/R/R/R+ABLL*X(1)/RH/RH)
      POTZ=  POT *(POTR*X(3)/R-ABLP*(X(3)*X(3)/R/R-1.D0)/R)
      RETURN
      END
      SUBROUTINE FADJST(DF, DFT, DFO, RN, DT, VPV, REJT, SO, P)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/18/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
      DIMENSION RN(3)
C    SUBROUTINE COMPUTES FREQUENCY ADJUSTMENT UNITS HZ/MIN/400 MHZ
      DTD= DT
      D  =  RN(1)*RN(3)  - RN(2)*RN(2)
       RNN= RN(1)
      RN(1)= RN(3)/D + P
      RN(2)= -RN(2)/D + DTD*P
      RN(3)= RNN/D + DTD*DTD* P
C NEW DWTERMINANT
      D  =  RN(1)*RN(3)  - RN(2)*RN(2)
      DF1=  DFO/D*(RN(3)-RN(2)*DTD)
      DF1= DF1*P
      DF2=  DFO/D*(RN(1)*DTD- RN(2) )
      DF2= DF2*P
      DFV= DFO- DF1- DF2*DTD
      DVPV= DFO*DFV*P
      REJT= 0.D0
C    DFV RESIDUAL  7.88 IS CHI-SQRT FOR DF=1 AND ALFA= 1P.C.
      RLIM= 7.88D0*SO**2
      IF(DVPV.LE.RLIM) GO TO 1
      RN(1)= RN(1)- P
      RN(2)= RN(2)- DTD*P
      RN(3)= RN(3) - DTD*DTD*P
      REJT= 1.D0
      D= RN(1)*RN(3) - RN(2)*RN(2)
      GO TO 3
  1   CONTINUE
C  UPDATE F AND F DOT
      DF= DF + DF1/60.D0
      DFT= DFT + DF2
      VPV= VPV + DVPV
3     RNN= RN(1)
      RN(1)= RN(3)/D
      RN(2)= -RN(2)/D
      RN(3)= RNN / D
      DFO= DFV
2      RETURN
      END
      SUBROUTINE GEMINT(C,TT,O,NMAX,IF18)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 03/06/83                  GEODOP V
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C  SUBROUTINE-INITILIZES COEFFICIENTS C, TT WITH GEM9 TRANCATED 10 BY 10
C            -OR READS AN ALTERNATIVE GRAV. FIELD ON TAPE18 IN GEM FORM.
C                                                    IF AVAILABLE
C            -COMPUTES FULLY NORMALIZED COEFFICIENTS IN C, TT
       DIMENSION C(  66), TT( 66), O(25)
C--- INITIALIZE FILE NUMBERS.
      NMA2= 2*NMAX
       NMB= NMAX*2+2
C  READ GRAVITY FIELD MODEL IF AVAILABLE ON TAPE18
       IF(NMAX.LE.11)GO TO 9
       DO 8 L=12,NMAX
      LL= (L-1)*(NMA2 - (L-2))/2
       DO 8 K=L,NMAX
      KL= LL+K-L +1
       C(KL )=0.D0
8      TT(KL )=0.D0
    9 READ(IF18,5,END=6) L,M,C1,T1
C      IF(EOF(18)) 6,7
C7     M1= M+1
      IF(L.GT.NMAX) GO TO 9
      LM= M*(NMA2-(M-1))/2+ L+1-M
      C(LM  )= C1
      TT(LM  )= T1
       GO TO 9
6      CONTINUE
5      FORMAT(I2,I3,2E15.8)
C  COMPUTE FACTORIALS O(NMB)
       O(1)=1.D00
       F=1.D00
       DO 4 L=1,NMB
       J1= L+1
       F=F*DBLE(L)
4      O(J1)= F
C   COMPUTE NORMALIZED COEFICIENTS
       DO 42 L=3,NMAX
       LI= L-1
       DO 42 M=1,L
       MI= M-1
      LM= MI*(NMA2-(MI-1))/2+L-MI
       LE=2
       IF(MI.EQ.0) LE=1
       LIPMI= LI+MI+1
       LIMI=  LI-MI +1
       ZR=O(LIPMI)/O(LIMI)/DBLE(LE*(LI+LI+1))
       ZR=DSQRT(ZR)
C      PRINT 59,LI,MI,C(LM  ),TT(LM  )
C59     FORMAT(1H ,5X,2I5,2E15.6)
      C(LM  )= C(LM  )/ZR
      TT(LM  )= TT(LM  )/ ZR
42     CONTINUE
      RETURN
      END

