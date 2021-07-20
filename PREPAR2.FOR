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
C
       SUBROUTINE WRITE5
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 03/24/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   NOTE: CHANGES FROM CDC VERSION ARE:
C   1. DATA STATEMENT FOR UNUSED VARIABLE "J" COMMENTED OUT.
C
C   FOR G1 COMPILER:
C   2. THE POSITIONS OF "ORBP" AND "IFIT" IN UNLABLED COMMON
C      HAVE BEEN SWITCHED TO GIVE CORRECT ALIGNMENT.
C
C
C        "WRITE5    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - DECEMBER /74.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - J.KOUBA .. PROGRAM GEODOP.
C
C         VARIABLES USED     -IEPO    ,TTAU    ,TPER    ,PERD
C                             APTP    ,DATP    ,RAAN    ,DRAN
C                             RAGR    ,TDRY    ,VPAR    ,NAME
C                             ISRC    ,ISAT    ,INJC    ,LOKR
C                             LENG    ,FSAT    ,NSTN    ,ORBP
C                             IFIT    ,COEF    ,NORD    ,NDOP
C                             ISTN    ,ALOK    ,PRES    ,FREQ
C                             DOPP    ,TIME
C
C         VARIABLES ALTERED  -ORBP    ,VPAR    ,        ,
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -TIMIN   ,VPSAT   ,        ,
C                             FLOAT   ,INT     ,        ,
C
C         I/O DEVICES        -IOUT    ,        ,        ,
C
       CHARACTER*10 NAME, ISTN
       CHARACTER*4  ISRC
C
       COMMON NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT , NSTN
     +      , IFIT , ORBP , COEF , NDOP , ISTN , PRES , TDRY , VPAR
     +      , DOPP , TIME , NTIM , NSAT , NORD , ISWT , LOKG , KDOP
     +      , NPSI , NPSA , NDPI , NDPA , IRCV , INTA
       DIMENSION ORBP( 7) , COEF(30) , DOPP(50) , TIME(50) , KDOP(50)
       COMMON /DEV/ IEPH , INPT , IOUT , IRED , IPRT
       COMMON /PAR/ DAYP , TPER , XMOT , APTP , DATP , ECCT , RAAN
     +      , DRAN , RAGR , ANGL , JSAT , IEPO , PERD
       COMMON /TIM/     A ,    B , ALOK , DTAU , EXPT , GXYZ , IORD
     +      , MESS , SIGS , TTAU , VPVP , FREQ , TAUT , FIRT , FIRT2
       DIMENSION GXYZ( 3) , EXPT(99) ,  A(3,2) , B(3,3,3) , MESS( 3)
     +         , TAUT(10) , INTA(30)
       DATA IZER / 0 / , WEAR / .0043752695D0/, PITW / 6.28318531D0/
C    +    ,    J / 0 /
C
       I       = NPSA
       CALL TIMIN
       IF (I.EQ.NPSA) RETURN
       GO TO (20 , 10) , IEPO
C
   10  DTIM     =DBLE(LOKG) - TPER
       DTIN    =DBLE(IDINT(DTIM / PERD)) * PERD
       ORBP(1) = DTIM - DTIN
      IF(TPER.LE.1.D-6) ORBP(1)= 0.D0
       ORBP(4) = APTP - DATP * DTIM
       ORBP(5) = RAAN + DRAN * DTIM
       DTIM    = RAGR + WEAR * DTIN
       ORBP(6) = DTIM -DBLE(IDINT(DTIM / PITW)) * PITW
C
   20  VPAR    = VPSAT(TDRY) * VPAR * .01D0
       WRITE (IOUT)    NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT
     +               , NSTN , ORBP , IFIT , (COEF(I),I=1,NORD) , NDOP
       WRITE (IOUT)    ISTN , ALOK , PRES , TDRY , VPAR , IZER
     +               , FREQ , NDOP , (DOPP(I),I=1,NDOP)
     +                             , (TIME(I),I=1,NDOP)
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
      SUBROUTINE TRINN(A,M,IENTER,MAXP)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/19/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   SUBPROGRAMS USED:
C    FORTRAN:        SQRT
C
      DIMENSION A(MAXP,MAXP)
      IF (M-1) 1,1,2
1     IF(A(1,1).NE.0.D0)A(1,1)=DSQRT(1.D0/A(1,1))
      GO TO 14
2     DO 6 L=1,M
      FMN=1.D0/A(L,1)
      DIV=A(L,1)
      DO 3 J=2,M
3     A(L,J-1)=A(L,J)/DIV
      A(L,M)=FMN
      DO 6 J=1,M
      IF (J-L) 4,6,4
4     FMULT=A(J,1)
      DO 5 K=2,M
5     A(J,K-1)=A(J,K)-FMULT*A(L,K-1)
      A(J,M)=-FMN*FMULT
6     CONTINUE
      IF (IENTER-1) 7,7,14
7     CONTINUE
      A(1,1)=DSQRT(A(1,1))
      DO 8 I=2,M
8     A(1,I)=A(1,I)/A(1,1)
      DO 13 I=2,M
      DIV=0.D0
      N=I-1
      DO 9 L=1,N
9     DIV=DIV+A(L,I)*A(L,I)
      A(I,I)=DSQRT(A(I,I)-DIV)
      JL=I+1
      IF (JL-M) 10,10,13
10    DO 12 J=JL,M
      DIV=0.D0
      DO 11 L=1,N
11    DIV=A(L,I)*A(L,J)+DIV
      A(I,J)=(A(I,J)-DIV)/A(I,I)
12    CONTINUE
13    CONTINUE
14    CONTINUE
      RETURN
      END
       FUNCTION VPSAT (TEMP)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/09/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "VPSAT     " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - DECEMBER  /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - D.E.WELLS .. PROGRAM WEATHER
C
C         VARIABLES USED     -TEMP    ,        ,        ,
C
C         VARIABLES ALTERED  -VPSAT   ,        ,        ,
C
C         VARIABLES RETURNED -VPSAT   ,        ,        ,
C
C         EXTERNAL ROUTINES  -XTOY    ,EXP     ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
C        [VPSAT[ RETURNS THE SATURATION VAPOUR-PRESSURE FOR "TEMP".
C                "TEMP" IN DEGREES CELSIUS / "VPSAT" IN MILLIBARS.
C
C
       DATA COF1 /  1013.246D0/, COF2 /  18.19728D0/, COF3 / .0187265D0/
     +    , COF4 /  -8.03945D0/, COF5 / 3.1813D-7 / , COF6 /  26.1205D0/
     +    , POWR /   5.02808D0/, TKEL /    273.16D0/, TKL1 /   373.16D0/
C
       TEMK = TEMP + TKEL
       TEM1 = TKL1 / TEMK
       TEM2 = TEM1 -   1.D0
       TEM3 =  1.D0- TEMK / TKL1
C
       VPSAT =  COF1 *  TEM1 ** POWR /DEXP
     +         (COF2 * TEM2
     +       +  COF3 *(1.D0-DEXP(COF4 * TEM2))
     +       +  COF5 * (DEXP(COF6 * TEM3) - 1.D0))
C
       RETURN
       END
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
       SUBROUTINE READTR
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/28/83  GEODOP 5.12 (POLISH)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   FOR G1 COMPILER:
C   1. THE POSITIONS OF "ORBP" AND "IFIT" IN UNLABLED COMMON
C      HAVE BEEN SWITCHED TO GIVE CORRECT ALIGNMENT.
C
C   SUBPROGRAMS USED:
C    GEODSCI:        DECODE
C    GEODOP:         MODULO, MINUT
C    FORTRAN:        FLOAT, INT
C
C
C
C        "READTR    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - FEBRUARY /75.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - TRANET OPERATIONS MANUAL(APL)
C
C         VARIABLES USED     -NDOP    ,NPSI    ,LOKG    ,DTAU
C                             NDPI    ,DOPP    ,NTIM    ,NDPA
C                             NSAT
C
C         VARIABLES ALTERED  -ISWT    ,DTAU    ,FREQ    ,NDOP
C                             ISTN    ,ISAT    ,DOPP    ,LOKG
C                             NPSI    ,NTIM    ,TTAU    ,ALOK
C                             NDPI    ,KDOP    ,TMME    ,TAUT
C                             NDPA    ,PRES    ,TDRY    ,VPAR
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -MODULO  ,MINUT   ,        ,
C                             EOF     ,(DECODE),        ,
C
C         I/O DEVICES        -INPT    ,        ,        ,
C
C
C        [READTR[ READS AND DECODES TRANET DATA.
       CHARACTER*10 ISTN, NAME
       CHARACTER*13 LINE11
C
       CHARACTER*4 LINE,IEND,IENE,IHD,IBLK, ISRC
       COMMON NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT , NSTN
     +      , IFIT , ORBP , COEF , NDOP , ISTN , PRES , TDRY , VPAR
     +      , DOPP , TMME , NTIM , NSAT , NORD , ISWT , LOKG , KDOP
     +      , NPSI , NPSA , NDPI , NDPA , IRCV , INTA
       COMMON /DEV/  IEPH , INPT , IOUT , IRED , IPRT
       COMMON /TIM/  A    , B    , ALOK , DTAU , EXPT , GXYZ , IORD
     +      , MESS , SIGS , TTAU , VPVP , FREQ , TAUT , FIRT , FIRT2
      COMMON/DAT/ IFRC, MAXD
C
       DIMENSION ORBP( 7) , COEF(30) , DOPP(50) , TMME(50) , KDOP(50)
     +         , GXYZ( 3) , EXPT(99) , A   ( 6) , B   (27) , MESS( 3)
       DIMENSION LINE( 4) , TAUT(10) , SCAL(10) , INTA(30)
       EQUIVALENCE (LINE11,LINE)
C
      DATA IEND/'**  '/,IENE/'//  '/,STOM/.1666666666667D-01/
     +   , SECP/0.D0/, TIMD/0.D0/
     +,IHD/'    '/,IBLK/'    '/
     +  , MAXS/50/, TOFF/ .0129239D0/
       DATA SCAL / 3.49090909091D0 , 3.49090909091D0,1.16363636364D0
     +           , 1.64609053498D0,0.D0,1.16363636364D0,4*0.D0/
C
   10  ISWT = 1
       ISWU = 0
C
   20  ISWU = ISWU + 1
C
   30 READ(INPT,5,END=32) (LINE(I),I=1,3),IYER,IDAY,IHOR,IMIN,ICOD,DTAU
     +               , FREQ , NDOP
C      IF (EOF(INPT).NE.0.) RETURN
      GO TO 34
   32 RETURN
   34 CONTINUE
      LINE(4)=IBLK
       IF (LINE(1).EQ.IENE) LINE(1) = IEND
       IF (LINE(1).NE.IEND) GO TO (40 , 30 , 30 , 30) , ISWU
                            GO TO (60 , 60 , 20 , 10) , ISWU
C
   40  ISWU = 2
      READ(LINE11,15) ISTN,I,ISAT
C   CHECK FOR GEOS -3
      IF(ISAT.EQ.17) ISAT=75
      IF(I.EQ.1) I= 30
      IF(I.NE.30.OR.ICOD.GT.351) GO TO 20
      IF(NSAT.NE.0.AND.ISAT.NE.NSAT) GO TO 20
C
      IFRC= 0
       IF (ICOD.EQ.351) ICOD = 269
       IF(IHD.EQ.IBLK) IHD=LINE(1)
       ICOD = MOD(ICOD - 261 , 5) + 1
       DOP=DBLE(2**NDOP)* SCAL(ICOD)
       ICDD= ICOD
       LOKG = IDAY * 1440
       NPSI = NPSI + 1
       NDOP = 0
       NTIM = 0
       TIMO =DBLE(IDINT(DBLE(MINUT(0,IHOR , IMIN)) * 6.D-3)) / 6.D-3
       GO TO 30
C
   42  ISWT =    3
       ISWU =    1
      TIMC= TIMD*STOM
      DTAU=DBLE(IDINT(TIMC))/STOM
      LOKG= LOKG+ IDINT(TIMC)
      TTAU= SECD*STOM
      ALOK= (TIMD-DTAU)*STOM
C
   50  SECP = SECD
C
   60 READ(INPT,5,END=64) LINE
C      IF (EOF(INPT).NE.0.) RETURN
      GO TO 66
   64 RETURN
   66 CONTINUE
       IF (LINE(1).EQ.IEND)    GO TO 80
       IF(LINE(1).EQ.IHD) GO TO 80
       READ(LINE11,25) TIMA,TIMB,TIMC
       SECD= TIMC
       IF (TIMA.EQ.9.D0)    GO TO (70 , 68) , ISWU
       TIMO= TIMA*1.D 04
       TIMP = TIMD
      TIMD= TIMO + TIMB
      IF(ISWU.GT.1) GO TO 42
C
      NDOP= NDOP + 1
       IF (NDOP.GT.MAXS) GO TO 50
       NDPI = NDPI + 1
      DOPP(NDOP)= DOP
       KDOP(NDOP) = 1
       TMME(NDOP)= (SECD-SECP+TIMD-TIMP)*STOM
C   CHECK FOR REASONABLE DELTA T
      IF(TMME(NDOP).GT.5.D0) TMME(NDOP)= .1D0
       IF(ICDD.EQ.4)                DOPP(NDOP)= DOPP(NDOP)+ TMME(NDOP)
     1 *7.2000000D 05
       GO TO 50
C
   68  DTAU = TIMO + TIMB
C                  
   70  NTIM = NTIM + 1
       TAUT(NTIM)= ( TIMO+TIMB-DTAU+SECD)*STOM- TOFF
       GO TO 60
C
   80  IF (NDOP.EQ.0)       GO TO 10
       ISWT = 2
       IF (NDOP.GT.MAXD) NDOP = MAXD
      NDPA= NDPA + NDOP
       VPAR =   58.5D0
       RETURN
C
    5 FORMAT(2A4,A3,A2,1X,I3,1X,2I2,1X,I3,1X,A4,1X,F5.0,7X,I2)
   15 FORMAT(1X,A3,1X,2I2)
   25 FORMAT(F1.0,F4.0,F6.6)
       END
C
       SUBROUTINE TIMII
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 03/23/83     GEODOP 5.03   (JAN82)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C        "TIMII     " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - JANUARY /75.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -IRCV    ,EXPT
C
C         VARIABLES ALTERED  -A       ,B       ,        ,
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -*NONE*  ,        ,        ,
C
C         I/O DEVICES        -IRED
C
$INCLUDE: 'PREPAR2.INC'
C
      TMAX=DSQRT(SCHI)/XMTM
      MESS(3)= 0
C
      JRCV=IRCV
      IF(JRCV.GT.2) JRCV=2
           DO 100 I = 1,3
      A(I,1)= EXPT(I+1)
      A(I,2)= EXPT(I+5)
                   DO 200 K = 1,2
                           DO 200 J = 1,3
                               B(I,J,K) = 0.D0
  200                  CONTINUE
      B(I,I,2)=APRI(I,JRCV)
  100      CONTINUE
C
      VPVP=0.0D0
       FIRT = EXPT(1)
       FIRT2= EXPT(5)
       RETURN
       END
C
       SUBROUTINE TIMIN
       IMPLICIT REAL*8 (A-H,O-Z)
C
C
C        "TIMIN     " - AUTHOR        - P.G. LAWNIKANIS
C                     - COAUTHOR      - J. KOUBA
C                     - WRITTEN ON    - JANUARY /75.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -LOKG    ,LENG    ,LOKR    ,NPSA
C                             IRCV    ,NDOP    ,TTAU    ,COEF
C                             GXYZ    ,IORD    ,TMME    ,TAUT
C                             DTAU    ,SIGS    ,ALOK    ,NTIM
C                             A       ,B       ,VPVP
C
C         VARIABLES ALTERED  -SIGS    ,ALOK    ,A       ,TAUT
C                             TTAU    ,DTAU    ,DOPP    ,KDOP
C                             MESS    ,B       ,NPSA    ,EXPT
C                             VPVP    ,        ,        ,
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -SLAN    ,TRINN   ,MULTP   ,
C                             ABS     ,INT     ,FLOAT   ,SQRT
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
$INCLUDE: 'PREPAR2.INC'
C
       TOFF= .0129239D0
       IF(IRCV.GE.4) TOFF= .012733333D0 
       SIGS = 0.D0
       TIML = DBLE(LOKG)
       TIMM = 0.D0
       TIMN = 0.D0
      VPV= 0.D0
      MESS(1)= IBLK
      MESS(2)= IBLK
      MK=1
       TIMO = 2.D0 / (DBLE(LENG) - 1.D0)
       TIMP = TIML - DBLE(LOKR)
        IF(FIRT.LE.1.D-6.AND.NPSA.EQ.0) FIRT= TIML*DAYS
       IF(FIRT2.LE.1.D-06) GO TO 7
       IF((TIML*DAYS).LT.FIRT2) GO TO 7
C  REST TIME AS PER 2ND INPUT CURVE
C  MAX. TWO INPUT TIME CURVE ALLOWED
      FIRT= FIRT2
      DO 8 I=1,3
8      A(I,1)= A(I,2)
7      CONTINUE
      IF(IRCV.GE.2) GO TO  4
C
    2  CONTINUE
      NTIM= 0
      NTIM= 0
      ALOK= SLAN(COEF,GXYZ,IORD,TIMO,TIMP,TTAU) + TTAU
       J    =  0
       K    = NDOP + 1
      IF(IFRC.EQ.2) GO TO 12
           DO 300 I = 1,K
      IRJCT(I)= 0
               L    = IDINT(TTAU + TTAU + STOM)
               IF ((L - L / 4 * 4).NE.0) GO TO 300
               TIMQ = DBLE(L / 2)
               TIMR = SLAN (COEF , GXYZ , IORD , TIMO , TIMP , TIMQ)
               TEMP = TTAU - TIMQ + TIMR
               IF (I.EQ.1) ALOK = TEMP
   10          J    = J + 1
               LDOP(J) =    I
               TAUT(J) = TEMP
               TIMM = TIMM + TEMP
               TIMN = TIMN + 1.D0
  300          TTAU = TTAU + TMME(I)
      NTIM= IDINT(  TIMN +.1D0)
C
       GO TO 12
C
  4    ALOK = ALOK+SLAN(COEF, GXYZ, IORD, TIMO, TIMP, ALOK+TTAU) + TTAU
       NPSA = NPSA + 1
      IF(NTIM.GE.1) GO TO 6
      GO TO 12
6      CONTINUE
      J=0
           DO 400 I = 1,NTIM
      TIMQ=TAUT(I)+ TOFF
               TIMR = SLAN (COEF , GXYZ , IORD , TIMO , TIMP , TIMQ)
      TIMQ=TAUT(I)
               TEMP = TIMQ - DBLE(IDINT(TIMQ + STOM)) + TIMR
C   REJECT IF TEMP.GE. 1 SECOND
      IF(DABS(TEMP).GE.STOM) GO TO 400
               TIMM = TIMM + TEMP
      J= J +1
      TAUT(J)= TEMP
400    CONTINUE
      NTIM= J
       TIMN = DBLE(NTIM)
C
   12  CONTINUE
       ALOK = ALOK + TIML
      DTAU= (TIML +8)*DAYS- FIRT
C CHECK FOR YEARS END
      IF(DTAU.LE.-200.D0 )DTAU= DTAU +365.D0
C     DT= (A(1) +(A(2)+ A(3)*DTAU)* DTAU )/ XMTM
      DT= (A1(1) +(A1(2)+ A1(3)*DTAU)* DTAU )/ XMTM
      DTAV= DT
      IF(IFRC.EQ.2) GO TO 18
      IF(NTIM.EQ.0) GO TO 18
      TIMMR= 0
      TIMPR= 0
      VPV= 0
C  INITALIZED REJECT VECTOR
C
           DO 500 I = 1,J
      XMIS= TAUT(I) - DT
               IF (DABS(XMIS).LE.TONE) GO TO 500
      TIMPR= TIMPR +1
      TIMMR= TIMMR + TAUT(I)
      VPV= VPV+ XMIS*XMIS
      IRJCT(I)= LDOP(I)
  500          SIGS      = SIGS + XMIS * XMIS
      IF(TIMPR.EQ.0.0D0)GOTO1103
      IF(TIMPR.LE.1.0D0)GOTO1101
       IF ((TIMN - TIMPR).GT.1.D0) GO TO 1101
      REAM= TIMMR/ TIMPR
      REAN= REAM - DT
      SIGM=DSQRT(( VPV- TIMPR*REAN*REAN)/ TIMPR/(TIMPR-1))
C  TMXX MA X LIMIT  COMPATIBLE WITH CHISQR  ( NOT CHISQR LIMIT)
      TMXX= TMAX*3.D0/DSQRT(TIMPR)
      IF(SIGM.GT.TMXX) GO TO 1101
C  ALL REJECTS ARE CONSISTENT
C--- CDC VERSION USED 'A' RATHER THEN 'A1' FOR NEXT 3 STATEMENTS.
      A1(1)= REAM*XMTM
      A1(2)= 0.D0
      A1(3)= 0.D0
      MESS(3)= MESS(3)-3
      FIRT= FIRT + DTAU
      MESS(MK)= NEWFT
      MK= MK + 1
      DT= REAM
      DO 1102 I=1,3
      DO 1104 K=1,3
1104   B(I,K,2)= 0.D0
1102   B(I,I,2)=APRI(I,JRCV)
      SIGS= SIGM
      DTAU= REAM* XMTM
      DTAV= REAM
      TEMP= 0.D0
      GO TO 18
1101  CONTINUE
C   REJECTS NOT CONSISTENT
      SIGS= SIGS- VPV
      TIMM= TIMM- TIMMR
      TIMN= TIMN- TIMPR
1103  CONTINUE
C
       NTIM = IDINT(TIMN)
       IF (TIMN.GT.0.D0) GO TO 16
      MESS(MK)=IRJU
      MK= MK +1
      TEMP= 0.D0
      IF(IRCV.EQ.2.OR.IRCV.EQ.4) GO TO 18
       RETURN
C
   16  CONTINUE
       DTAV = TIMM / TIMN
      TEMP= DTAV - DT
      IF(TIMN.GT.1.D0)SIGS=DSQRT((SIGS-TEMP*TEMP*TIMN)/(TIMN-1.D0)/TIMN)
       DTAU = DTAV * XMTM
C
      TMXX= TMAX*3.D0/DSQRT(TIMN)
      IF(SIGS.LE.TMXX) GO TO 18
      MESS(MK)= IRJV
      MK= MK +1
      TEMP= 0.D0
      IF(IRCV.EQ.2.OR.IRCV.EQ.4) GO TO 18
       RETURN
C
   18  CONTINUE
      IF(IRCV.LE.2.OR.IRCV.EQ.4) ALOK= ALOK- DTAV
      IF(NTIM.GE.1) GO TO 902
      TEMP= 0.D0
      DTAU= DTAV*XMTM
C   SKIP THE REST FOR IFRC=2 I.E. FOR 324/162 FREQ. PAIR
      GO TO 901
902      CONTINUE
       SIGS = SIGS * XMTM
      TEMP= TIMN* XNIN
      TIMM= TIML* DAYS - FIRT
       TIMN = TIMM * TIMM
       TIMO = TIMM * TEMP
       TIMP = TIMM * TIMO
       TIMQ = TIMM * TIMP
       TIMR = TIMM * TIMQ
       XMIS = DTAU - (A1(1) + A1(2) * TIMM + A1(3) * TIMN)
           DO 700 I = 1,3
                   DO 700 J = 1,3
                       B1(I,J) = B2(I,J)
  700          CONTINUE
C
       B(1,1,1) = B(1,1,1) + TEMP
       B(1,2,1) = B(1,2,1) + TIMO
       B(1,3,1) = B(1,3,1) + TIMP
       B(2,1,1) = B(1,2,1)
       B(2,2,1) = B(2,2,1) + TIMP
       B(2,3,1) = B(2,3,1) + TIMQ
       B(3,1,1) = B(1,3,1)
       B(3,2,1) = B(2,3,1)
       B(3,3,1) = B(3,3,1) + TIMR
       V(    1) = TEMP * XMIS
       V(    2) = TIMO * XMIS
       V(    3) = TIMP * XMIS
           DO 800 I = 1,3
                   DO 800 J = 1,3
                       B3(I,J) = B1(I,J)
  800          CONTINUE
C
       CALL TRINN (B1 , 3 , 2 , 3)
      DO 1112 I=1,3
      DO 1112 J=I,3
1112   B1(J,I)= B1(I,J)
       CALL MULTP ( 3 , 3 , 1 , 3 , 3 , B1 , V , A2)
       TEMP = -XMIS * (A2(1) + A2(2) * TIMM + A2(3) * TIMN - XMIS)
       TONE =DSQRT (((((B1(3,3) * TIMM + B1(2,3) + B1(3,2)) * TIMM
     +               +  B1(1,3)        + B1(2,2) + B1(3,1)) * TIMM
     +                                 + B1(1,2) + B1(2,1)) * TIMM
     +               + B1(1,1)) * S0SQ + TERR) / XMTM * 3.D0
      KP=1
       IF (TEMP.LE.SCHI) GO TO 20
      KP=0
      MESS(MK)= IRJT
      TEMP=0.D0
      GO TO 901
C
   20  CONTINUE
      MESS(3)= MESS(3) +KP
           DO 900 I = 1,3
               A1(I) = A1(I) + A2(I)
                   DO 900 J = 1,3
                       B2(I,J) = B3(I,J)
  900          CONTINUE
901      CONTINUE
       IF (IRCV.EQ.1) NPSA = NPSA + 1
       IF (NPSA.LE.99) EXPT(NPSA) = DTAU
       VPVP = VPVP + TEMP
       RETURN
       END
C
       SUBROUTINE TIMIP
       IMPLICIT REAL*8 (A-H,O-Z)
C
C
C        "TIMIP     " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - JANUARY /75.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -VPVP    ,NPSA    ,B       ,A
C                             EXPT
C
C         VARIABLES ALTERED  -B       ,        ,        ,
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -PRINTP  ,        ,        ,
C                             FLOAT   ,INT     ,SQRT    ,
C
C         I/O DEVICES        -IPRT    ,        ,        ,
C
$INCLUDE: 'PREPAR2.INC'
C
      TEMP= SOSQ**2
      MPSA= MESS(3)-3
      IF(MPSA.GT.0) TEMP= VPVP/DBLE(MPSA)
           DO 1000 I = 1,3
                   DO 1000 J = 1,3
                       B1(I,J) = B1(I,J) * TEMP
 1000          CONTINUE
       TIMM =      A1(  2)  * XSEC
       TIMN =DSQRT(B1(2,2)) * XSEC
       TIMO =      A1(  3)  * XTIR
       TIMP =DSQRT(B1(3,3)) * XTIR
       TEMP =DSQRT(TEMP)
       WRITE (IPRT, 5) ((B1(I,J),J=1,3),I=1,3) , TEMP , A1(1)
     +               , A1(2) , TIMM , TIMN , A1(3) , TIMO , TIMP
C
       J    = NPSA
       IF (J.GT.99) J = 99
       TEMP = DBLE(IDINT(EXPT(J)))
       TIMM = TEMP - 1.D0
       TIMN = TEMP + 1.D0
C      IF (TEMP.GE.EXPT) TIMM = TIMM - SIZE
C      IF (TEMP.LE.EXPT) TIMN = TIMN + SIZE
       IF (TEMP.GE.EXPT(1)) TIMM = TIMM - SIZE
       IF (TEMP.LE.EXPT(1)) TIMN = TIMN + SIZE
      WRITE(IPRT,1901)
       CALL PRINTP (1 , 1 , TIMM , TIMN ,1.D0,ISTN , 1 , 1)
           DO 1100 I = 1,J
               CALL PRINTP (2 , 2,2.D0,2.D0,EXPT(I) , ISTN , 2 , 2)
 1100      CONTINUE
       CALL PRINTP (3 ,3,3.D0,3.D0,3.D0,ISTN , 3 , 3)
       RETURN
C
    5  FORMAT (1H1,//,31H   VARIANCE - COVARIANCE MATRIX
     +          ,3(//,3(5X,E13.6))
     +           ,///,23H   STD. DEVIATION(ms). ,E13.6
     +           ,///,17H   PHASE SOLUTION
     +            ,//,17H   INTERCEPT(ms) ,E13.6
     +            ,//,17H   SLOPE(ms/d).. ,E13.6,17H   FREQ.OFF(Hz)
     +                                    ,E10.3, 6H   SD ,E10.3
     +            ,//,17H   CURVAT(ms/d/d),E13.6,17H   DRFT(10**10/d)
     +                                    ,E10.3, 6H   SD ,E10.3       )
   15  FORMAT (F5.0)
 1901 FORMAT(/////1X,'===> PLOT BELOW IS OF CLOCK ERROR AND NOT OF'
     @,' FREQUENCY OFFSET]'/)
       END
       FUNCTION SLAN (COEF , CORD , IORD , SCAL , BIAS , TIME)
       IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/11/83   GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "SLAN      " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - FEBRUARY /75.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -BIAS    ,TIME    ,SCAL    ,IORD
C                             COEF    ,CORD    ,SLAN
C
C         VARIABLES ALTERED  -SLAN    ,        ,        ,
C
C         VARIABLES RETURNED -SLAN    ,        ,        ,
C
C         EXTERNAL ROUTINES  -CHEBY   ,        ,        ,
C                             SQRT    ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C         EXTERNAL ROUTINES  -*NONE*  ,        ,        ,
C
C
       DIMENSION COEF(IORD,3) , CORD(   3) , CHEB(  10)
       DATA CMIN / 1.798755003D 10 /
C
       SLAN = 0.D0
       TEMP = (BIAS + TIME) * SCAL - 1.D0
       CALL CHEBY (CHEB , CHEB ,    0 , IORD , TEMP , TEMP)
           DO 100 I = 1,3
               TEMP = COEF(1,I) - CORD(I)
                   DO 200 J = 2,IORD
                      TEMP = TEMP + CHEB(J) * COEF(J,I)
  200             CONTINUE
               SLAN = SLAN + TEMP * TEMP
  100      CONTINUE
       SLAN =-DSQRT(SLAN) / CMIN
       RETURN
       END

