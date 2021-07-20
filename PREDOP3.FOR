C PREDOP3
C output modified by Bolzan - 4/3/90
C-----------------------------------------------------------------------
       FUNCTION IQUIK(I,J,K)
C
C   VERSION:  9/25/82                  GEODOP IV+V   (JUL81)
C   CHECKED FOR RUNNING USING EITHER CDC OR IBM FORTRAN.
C    (G1 OR Q COMPILER W/WO 'AUTODBL(DBLPAD),OPT(3)' OPTIONS.)
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "IQUIK     " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -I       ,J       ,K       ,
C
C         VARIABLES ALTERED  -IQUIK   ,        ,        ,
C
C         VARIABLES RETURNED -IQUIK   ,        ,        ,
C
C         EXTERNAL ROUTINES  -*NONE*  ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
       IQUIK = (I - I / J * J) / K
       RETURN
       END
      INTEGER FUNCTION IBCD3X(IDATA)
C
C   VERSION:  9/25/82                  GEODOP IV+V   (JUL81)
C   MODIFIED TO RUN USING EITHER CDC OR IBM FORTRAN.
C    (G1 OR Q COMPILER W/WO 'AUTODBL(DBLPAD),OPT(3)' OPTIONS.)
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   NOTE: CHANGES FROM CDC VERSION ARE:
C    1. USE OF CDC "SHIFT" AND "AND" FUNCTIONS ELIMINATED.
C
C   SUBPROGRAMS USED:
C    FORTRAN:       MOD
C
C
C             FUNCTION TO CONVERT SATELLITE BCD EXCESS 3 DATA
C             INTO INTEGER FORM
C
      DIMENSION IDATA(5)
C     IF      (SHIFT(IDATA(1),-4).NE.15) GO TO 20
      IF            (IDATA(1)/16 .NE.15) GO TO 20
      IPRM    =    0
      DO      10 I=1,9
      ITMP    =    IDATA(1+I/2)
C     IF      (MOD(I,2).EQ.0) ITMP=SHIFT(ITMP,-4)
      IF      (MOD(I,2).EQ.0) ITMP=ITMP/16
C     ITMP    =    AND(ITMP,15)-3
      ITMP    =    MOD(ITMP,16)-3
      IF      (ITMP.LT.0.OR.ITMP.GT.9) GO TO 20
10    IPRM    =    IPRM*10+ITMP
      IBCD3X  =    IPRM
      RETURN
20    IBCD3X  =    0
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE JMRINP(ISAT, IST, TIME, TDRY, PRES, RH, SINC, NTIM,CLK)
      IMPLICIT REAL*8 (A-H,O-Z)
        CHARACTER*4 ICHK
C
C   VERSION: 01/28/83                  GEODOP 5.11 (STDFTN)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   FOR G1 OPERATION:
C   1. "IDUM" ADDED TO "FIT" LABELED COMMON TO GIVE CORRECT ALIGNMENT.
C
C   JMRINP       WRITTEN BY  J. KOUBA   OCTOBER 1975
C                     - REFERENCES    - J.KOUBA .. PROGRAM PREDOP
C
C         VARIABLES USED     -INP     ,L       ,IPT     ,IDAT
C                             ISTA    ,ISTO
C
C         VARIABLES ALTERED  -ISTA    ,ISTN    ,ISAT    ,IDAY
C                             IHOR    ,IMIN    ,NOTE   ,IVAR
C                             IFIX    ,NDOP    ,DOP     ,KDOP
C
C         EXTERNAL ROUTINES  -EOF     ,DBLE   ,        ,
C
C         I/O DEVICES        -INP     ,IPT     ,        ,
C
C
C    READS NEXT MJV PASS FROM DEVICE [INP[
C
C   SET FLAG [ISTA[ = 1 ... END-OF-FILE
C                   = 2 ... NO MJV DATA
C                   = 3 ... PASS OK.
C                   = 4 ... LAST MJV PASS
C
       COMMON /FIT/   LL ,    M ,N,IDUM, DACA , DECA , DOPC , KDOP ,
     +               DOP ,  TIM , RESD , XNOR
       COMMON /IOS/  ICD ,  INP ,  IOT ,  IPT , ISTA , ISTB , IRJT ,
     +              LOKH , LOKR , JDOP , LDOP , MDOP , NDOP
       COMMON /MJV/ IVAR , IFIX , IDAY , IHOR , IMIN , NOTE, KSAT(17)
C--- SPECIAL LABELED COMMON TO INDICATE VAR. PARM. FORMAT.
C     OSU IBM VERSION ONLY.
      COMMON / OSU / IDAT
       DIMENSION DACA(  14) , DECA(  14) , DOPC(  14) , KDOP(  48) ,
     +  DOP(8,12), TIM(96), RESD( 48), IVAR( 14) ,
     +           IFIX(  14) , NOTE(  32)
      DIMENSION ISIG( 8), OBSIG( 8), DT( 4)
      DIMENSION ISTN(3),ICHK(3)
      DATA ICHK/ '0000','0000','00  '/
      DATA ISIG/ 3*1000, 100  ,4*10/, DT/27.D0,59.D0,87.D0,119.D0/
      XMTM= .600000000D 08
      ISTA= 1
      CLK= 0.D0
   10 READ(INP,206,END=50) ISTN,ICMNT,TD,PR,RH,IDAY,IHOR,IMIN,SEC
     1 , SECM, SINC, DOPXFR, SIGST ,IPSS
C     IF(EOF(INP).NE.0.) GO TO 50
C     IF(ISTN.NE.10H0000000000) GO TO 10
      DO 12 I=1,3
      IF(ISTN(I).NE.ICHK(I)) GO TO 10
   12 CONTINUE
      TK= IST
      IVAR(1)= 0
C    CHECK FOR LEGITIMATE MET DATA
      IF(DABS(PR-PRES).LT.40.D0) PRES= PR
      IF(DABS(TD).LE.50.D0) TDRY= TD
      IF(RH.LT.0.D0.OR.RH.GT.100.D0) RH= 50.D0
C  RECONSTRUCT JMR TIMING HISTORY IF POSSIBLE AT ALL
      CLK= (SEC+ SECM/1.000000D 06           )/.6000000D02
      IF(DABS(CLK).GT.1.D-08) CLK= CLK + .49156D-02/.600000D02
      IF(CLK.GT..9D0) CLK= CLK- 1.0000000D 00
      IF( SIGST.LT.TK) GO TO 10
      ISTA= 2
      ITWO=(IMIN/2)*2- IMIN
      IF(ITWO.NE.0)IMIN= IMIN+1
      IF(IMIN.LT.60) GO TO 600
      IHOR= IHOR +1
      IMIN= 0
      IF(IHOR.LT.24) GO TO 600
      IDAY=IDAY+1
      IHOR=0
C   READ VARIABLE PARAMETERS
  600 READ(INP,202,END=40) (IVAR(I),I=IDAT,14)
C     IF(EOF(INP).NE.0.) GO TO 40
C   READ FIXED PARAMETERS
      READ(INP,203,END=40) IFIX
C     IF(EOF(INP).NE.0.) GO TO 40
      ISAT= IFIX(11)/100
      IFIX(11)= IFIX(11) +800300000
      IF(IFIX(13).GE.9999000.AND.IFIX(13).LE.10000999)IFIX(13)= IFIX(13)
     1  +800000000
      IFIX(14)= IFIX(14) +(8        -IFIX(14)/80000000 )* 100000000
      WRITE(IPT,15) ISAT, IDAY, IHOR, IMIN ,IPSS
      IF(DOPXFR.NE.1.D0) WRITE(IPT,201)
C    READ DOPPLERS
      IL=  IST/10   +1
      IH= IST - (IL-1)*10 +1
      ISIGH= ISIG(IH)
      ISIGL= ISIG(IL)
      DO 100 NDOP=1,9
      READ(INP,204,END=40) (DOP(J,NDOP),OBSIG(J),DOP(J+1,NDOP),J=1,8,2)
C     IF(EOF(INP).NE.0.) GO TO 40
      DO 100 I=1,8,2
C  SCALE DOPPLERS BY 64/55
      DOP(I,NDOP)= DOP(I,NDOP)* .1163636364D 01
      K= I +1
      IF(OBSIG( I).GT.ISIGH ) DOP(I,NDOP)=0.D0
      IF(DOP(K  ,NDOP).LE.ISIGL )   GO TO 104
      DOP(K  ,NDOP) =0.D0
      GO TO 100
104   DOP(K  ,NDOP)= DOP(I,NDOP)
100   CONTINUE
C   READ THE TIME INFORMATION
      SINN= SINC/ XMTM
  105 READ(INP,205,END=20) (KDOP(I),TIM(I),I=1,4)
C     IF(EOF(INP).NE.0.) GO TO 20
      IF(KDOP(1).EQ.0) GO TO 105
      READ(INP,205,END=20) (KDOP(I),TIM(I),I=5,36)
C     IF(EOF(INP).NE.0.) GO TO 20
       IF(TIME.EQ.0.D0) GO TO 30
      K= 1
      TIM(63)= 0.D0
      DO 101 I=1,9
      DO 102 J=1,4
      DTT= DT(J)* 1.0000000D 06
      TNT= KDOP(K) - (KDOP(K)/100)*100
      IF(TNT.GT.TIM(K)) TIM(K)= TIM(K) +100.D0
      IF( KDOP(K).LT. 10000) DTT= DTT +1.0000000D 06
      TIMM= (TIM(K) + KDOP(K)- TNT+DTT)/ XMTM
      TIM(K)= TIMM- SINN
      SINN= TIMM
      IF(SINN.GT.1.9D 00) SINN=SINN- 2.0000000D 00
102      K= K+1
      TIM(63+I)=DBLE((I-1)*2) + (DTT+KDOP(K-1))/XMTM
      NTIM= I
101    CONTINUE
      GO TO 30
   20  ISTA = 3
   30  ISTA = ISTA + 1
       NDOP = (NDOP - 1) * 8
           DO 200 I = 1,36
               KDOP(I) = 0
  200      CONTINUE
       RETURN
   40 WRITE(IPT,15) ISAT, IDAY, IHOR, IMIN ,IPSS
      WRITE(IPT, 65)
   50  RETURN
C
   15 FORMAT(18X,I2,2X,3I3,1X,A1)
   65  FORMAT (30X,34H***** NO MJV - PASS REJECTED *****
     &/'----------------------------------------------------------------
     &------------------------')
C
  201 FORMAT(30X,44H**** WARNING - DOPPLERS NOT INITIALIZED ****)
202    FORMAT(7(I9,1X))
203    FORMAT(7(I9,1X)/3(I9,1X),5X,I4,1X,I9,2(2X,I8))
204    FORMAT(4(F7.0,2F5.0,1X))
205      FORMAT( 4(I6,F3.0,3X))
  206 FORMAT(1X,2A4,A2,A9,F4.0,F4.0,F2.0,I4,2I2,F2.0,F7.0,F3.0,F7.0,F3.0
     @,2X,A1)
      END
C----------------------------------------------------------------------- 
      SUBROUTINE LESQFT (ACOR , BASE ,    C , FUNC , MAXR , XMAX)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/19/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   FOR G1 OPERATION:
C   1. "IDUM" ADDED TO "FIT" LABELED COMMON TO GIVE CORRECT ALIGNMENT.
C
C
C        "LESQFT    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - D.E.WELLS .. SUBROUTINE LSA
C
C         VARIABLES USED     -MAXR    ,L       ,ACOR    ,NROW
C                             NCOL    ,BASE    ,FUNC    ,
C
C         VARIABLES ALTERED  -B       ,C       ,XMAX    ,XNOR
C                             RESD    ,        ,        ,
C
C         VARIABLES RETURNED -C       ,XMAX    ,        ,
C
C         EXTERNAL ROUTINES  -TRINN   ,        ,        ,
C                             ABS     ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
       COMMON /FIT/    L , NROW,NCOL,IDUM,DACA, DECA , DOPC , KDOP ,
     +               DOP ,  TIM , RESD , XNOR
       DIMENSION DACA(    14) , DECA(    14) , DOPC(    14) ,
     +           KDOP(    48) ,  DOP(    96) ,  TIM(    96) ,
     +           RESD(    48) ,    A( 10,10) ,    B(    10) ,
     +          BASE(MAXR,32) ,    C(    10) , FUNC(    32) , ACOR(   2)
C
C        CLEAR "A" ARRAY-
C
           DO 100 I = 1,MAXR
      C(I)= 0.D0
                   DO 100 J = 1,MAXR
                       A(I,J) = 0.D0
  100          CONTINUE
       IF (L.NE.0) GO TO 10
       A(2,2) = ACOR(1)
       A(3,3) = ACOR(1)
       A(2,3) = ACOR(2)
       A(3,2) = ACOR(2)
C
C        CONSTRUCT NORMAL EQ[N "A" & "B"-
C
   10      DO 200 I = 1,NROW
               XT = 0.D0
                   DO 300 J = 1,NCOL
                       YT = BASE(I,J)
                       XT = XT + FUNC(J) * YT
                           DO 300 K = 1,NROW
                               A(K,I) = A(K,I) + BASE(K,J) * YT
  300              CONTINUE
               B(I) = XT
  200      CONTINUE
C
C        INVERT "A" MATRIX-
C
       CALL TRINN (   A , NROW ,    2 , MAXR)
C
C        COMPUTE COEFFICIENT VECTOR "C"-
C
           DO 600 I = 1,NROW
               XT = 0.D0
                   DO 700 J = 1,NROW
                       XT = XT + A(I,J) * B(J)
  700              CONTINUE
               C(I) = XT
  600      CONTINUE
C
C        COMPUTE RESIDUALS VECTOR "RESD" & NORM "XNOR"-
C
       XMAX = 0.D0
       XNOR = 0.D0
           DO 800 I = 1,NCOL
               XT = FUNC(I)
                   DO 900 J = 1,NROW
                       XT = XT - BASE(J,I) * C(J)
  900              CONTINUE
               RESD(I) = XT
               IF (DABS(XT).LE.XMAX) GO TO 20
               L       =  I
               XMAX    = DABS(XT)
   20          XNOR    = XNOR + XT * XT
  800      CONTINUE
       RETURN
C
       END
       FUNCTION MINUT (I,J,K)
C
C   VERSION:  9/25/82                  GEODOP IV+V   (JUL81)
C   CHECKED FOR RUNNING USING EITHER CDC OR IBM FORTRAN.
C    (G1 OR Q COMPILER W/WO 'AUTODBL(DBLPAD),OPT(3)' OPTIONS.)
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "MINUT     " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -I       ,J       ,K       ,
C
C         VARIABLES ALTERED  -MINUT   ,        ,        ,
C
C         VARIABLES RETURNED -MINUT   ,        ,        ,
C
C         EXTERNAL ROUTINES  -*NONE*  ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
       MINUT = I * 1440 + J * 60 + K
       RETURN
       END
C----------------------------------------------------------------------- 
      SUBROUTINE METDAT (PRES , TDRY , VPAR)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C        "METDAT    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -LOKR    ,METP    ,METL    ,XMET
C
C         VARIABLES ALTERED  -METP    ,PRES    ,TDRY    ,VPAR
C
C         VARIABLES RETURNED -PRES    ,TDRY    ,VPAR    ,
C
C         EXTERNAL ROUTINES  -DBLE   ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
C        [METDAT[ RETURNS THE PRESSURE , DRY TEMPERATURE, AND
C                 PARTIAL VAPOUR PRESSURE FOR A GIVEN LOKON TIME.
C
C               THESE VALUES ARE OBTAINED BY A LINEAR
C               INTERPOLATION OF ADJACENT DATA POINTS
C               IN THE "XMET" ARRAY.
C
       COMMON /DAT/ ANOT , AZIM , CDOP , CELV , CPAP , DATM , DNOM ,
     +              DTIM , ECCS , ELEV , ELMX , FRCV , IRCV , METL ,
     +              METP , NDIR , TIME , XMET ,
     +                BT ,   C4 ,   CC ,   CM ,LB,IDUM1, PI ,   RD ,
     +                RH ,   SS ,   ST ,   WE ,   XP
       COMMON /IOS/  ICD ,  INP ,  IOT ,  IPT , ISTA , ISTB , IRJT ,
     +              LOKH , LOKR , JDOP , LDOP , MDOP , NDOP
       DIMENSION DATM(   3) , DNOM(  32) , DTIM(   4) , ELEV(  33) ,
     +           XMET(4,1000)
       VALUE(X,Y,Z) = X - (X - Y) * Z
C
       TIMM =DBLE(LOKR)
           DO 100 I = METP,METL
               FACT = XMET(1,I)
C              METP = I
      ITEM=I
               IF (FACT.GE.TIMM) GO TO 20
  100      CONTINUE
      METP=ITEM
C
C      NO DATA AVAILABLE --- RETURN DEFAULT METS -
C
   10  PRES = 1014.0D0
       TDRY = 15.D0
      VPAR=10.D0
       RETURN
C
C      FIND INTERVAL AND INTERPOLATE VALUES-
C
   20 METP=I
       IF (METP.EQ.1) GO TO 10
       J    = I - 1
       FACT = (FACT - TIMM) / (FACT - XMET(1,J))
       TDRY = VALUE (XMET(2,I) , XMET(2,J) , FACT)
       VPAR = VALUE (XMET(3,I) , XMET(3,J) , FACT)
       PRES = VALUE (XMET(4,I) , XMET(4,J) , FACT)
       RETURN
       END

