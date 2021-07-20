       SUBROUTINE PRINTP (MODE,LABEL,XLOWR,XHIGH,VALUE,NAME1,NAME2,LINE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*10 NAME1
C
C   VERSION: 01/09/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "PRINTP    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - NOVEMBER /73.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -MODE    ,LABEL   ,NAME1   ,NAME2
C                             XLOWR   ,XHIGH   ,LINE    ,VALUE
C
C         VARIABLES ALTERED  -*NONE*  ,        ,        ,
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -ABS     ,FLOAT   ,INT     ,
C
C         I/O DEVICES        -OUTPUT = LINE PRINTER
C
C
C   .PRINTP. DOES THE ACTUAL PLOTTING.
C
C   PLOT OF FORM ... .LABEL.TH HEADING , STATION(S) .NAME1. TO .NAME2. ,
C                    BOUNDS .XLOWR. LE .VALUE. LE .XHIGH. , STARTING AT
C
C   .MODE. = 1 ... START NEW PLOT WITH ABOVE DATA.
C          = 2 ... PLOT CURRENT .VALUE..
C          = 3 ... TERMINATE THIS PLOT.
C
       COMMON /DEV/ IEPH , INPT , IOUT , IRED , IPRT
C
C      DIMENSION CHAR(121),HEAD(5),IAXIS(13)
      INTEGER CHAR(121),HEAD(3,5),IAXIS(13),BLANK
C      DATA HEAD/10H*FREQ.OFF* , 10H ** DX **  , 10H ** DY **  ,
C    =           10H ** DZ **  , 10H ** DR **  /
      DATA HEAD/'*FRE','Q.OF','F*  ',' ** ','DX *','*   '
     1,         ' ** ','DY *','*   ',' ** ','DZ *','*   '
     2,         ' ** ','DR *','*    ' /
       DATA BLANK/'    '/
       DATA CHAR/121*' ' /
       DATA IPLOT,JT/0,1/
      DATA MASTER/'*   '/,MX/'X   '/
       GO TO (10,20,30),MODE
C
C   START NEW PLOT .....
C
   10  IPLOT = IPLOT + 1
       WRITE(IPRT, 5) IPLOT,(HEAD(I,LABEL),I=1,3)
       IF (LABEL.EQ.1) WRITE(IPRT, 15) NAME1
       IF (LABEL.GT.1.AND.NAME2.EQ.BLANK) WRITE(IPRT, 15) NAME1
       IF (LABEL.GT.1.AND.NAME2.NE.BLANK) WRITE(IPRT, 25) NAME1,NAME2
       WRITE(IPRT, 35) XLOWR,XHIGH
       XT = (XHIGH - XLOWR) / 12.D0
       SCALE = 10.D0/XT
       DO 100 I=1,13
               YT =DABS (XLOWR + XT *DBLE (I - 1))
               IAXIS(I) =IDINT(YT -DBLE ((IDINT (YT) / 1000) * 1000))
  100  CONTINUE
       WRITE(IPRT, 45)(IAXIS(I),I=1,13)
       WRITE(IPRT, 55)
       IL = LINE - 1
       XHI = XHIGH
       XLO = XLOWR
       RETURN
C
C   PLOT CURRENT VALUE .....
C
   20  CHAR(JT) = BLANK
       IL = IL + 1
       IT = IL - (IL / 5) * 5
       JT =IDINT((VALUE - XLO) * SCALE + 1.5D0)
       XT = MASTER
       IF (JT.LT.1.OR.JT.GT.121) XT = MX
       IF (JT.LT.1) JT = 1
       IF (JT.GT.121) JT = 121
       CHAR(JT) = XT
       IF (IT.EQ.0) WRITE(IPRT, 65) IL,(CHAR(I),I=1,121),IL
       IF (IT.GT.0) WRITE(IPRT, 75) (CHAR(I),I=1,121)
       RETURN
C
C   END PLOT .....
C
   30  WRITE(IPRT, 55)
       WRITE(IPRT, 45) (IAXIS(I),I=1,13)
       RETURN
C
    5  FORMAT (1H1,/,58X,17H P L O T   S E T ,I2,/,58X,
     $                   20H--------------------,//,63X,2A4,A2/)
   15  FORMAT(60X,8HSTATION ,A10)
   25 FORMAT(54X,8HSTATION ,A10,12H TO STATION ,A10)
   35  FORMAT (/,54X,F11.2,6H  TO  ,F11.2,///)
   45  FORMAT (6X,12(I3,7X),I3)
   55  FORMAT (7X,25(5H[    ))
   65  FORMAT (3X,I3,1H>,121A1,1H@,I3)
   75  FORMAT (7X,121A1)
C
       END
       SUBROUTINE READEF
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*4 ISRC
      CHARACTER*10 NDESC ,NAME, ISTN
C
C   VERSION: 03/24/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   SUBPROGRAMS USED: NONE
C
C
C
C        "READEF    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - DECEMBER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -IEPO    ,IEPH    ,NORD    ,ISWT
C
C         VARIABLES ALTERED  -LOKR    ,LENG    ,COEF    ,INJC
C                             FSAT    ,ORBP    ,IFIT    ,ISWT
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -EOF     ,INT     ,        ,
C
C         I/O DEVICES        -IEPH    ,        ,        ,
C
C
       DIMENSION VARORB(21)
       COMMON NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT , NSTN
     +      , IFIT , ORBP , COEF , NDOP , ISTN , PRES , TDRY , VPAR
     +      , DOPP , TMME , IFLG , NSAT , NORD , ISWT , LOKG , KDOP
     +      , NPSI , NPSA , NDPI , NDPA
       DIMENSION ORBP( 7) , COEF(30) , DOPP(50) , TMME(50) , KDOP(50)
       COMMON /DEV/ IEPH , INPT , IOUT , IRED , IPRT
       COMMON /PAR/ DAYP , TPER , XMOT , APTP , DATP , ECCT , RAAN
     +      , DRAN , RAGR , ANGL , JSAT , IEPO, PERD
C
       GO TO (10 , 20, 25) , IEPO
C    SHORT ARC  EPHEM.
   10  READ  (IEPH,END=30) LOKR , LENG ,(COEF(I),I=1,NORD) , NSAT, FSAT
     +               , ORBP , IFIT
       GO TO 40
C   PRECISE EPHEM.
   20 READ(IEPH,END=30) LOKR,LENG,(COEF(I),I=1,NORD),MSAT, IYER
       GO TO 40
C   UPDATED EPHEM.
   25 READ(IEPH,END=30) LOKR,LENG,(COEF(I),I=1,NORD),NSAT, NDESC,
     +                  VARORB    
      GO TO 40
   30 ISWT=ISWT+3
   40 CONTINUE
       RETURN
C
       END
      SUBROUTINE READGE
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*4  MASS, IBLK,IHDR ,MASI,MMINUS,LINE,ISRC
      CHARACTER*10 ISTN,NAME
      CHARACTER*24 LINE24
C
C   VERSION: 03/24/83     GEODOP 5.09 (OCT82)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   FOR G1 COMPILER:
C   1. THE POSITIONS OF "ORBP" AND "IFIT" IN UNLABLED COMMON
C      HAVE BEEN SWITCHED TO GIVE CORRECT ALIGNMENT.
C
C
C
C        "READGE    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - DECEMBER /74.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - GEOCEIVER OPERATIONS MANUAL(APL)
C
C         VARIABLES USED     -NDPI    ,INPT    ,NPSI    ,IFLG
C                             NSAT    ,ISWT    ,NDPA    ,NDOP
C                             INTA
C
C         VARIABLES ALTERED  -ISWT    ,NDOP    ,ISAT    ,NPSI
C                             LOKG    ,TTAU    ,TDRY    ,PRES
C                             VPAR    ,DOPP    ,TMME    ,KDOP
C                             NDPI    ,NDPA    ,NDOP    ,
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -MINUT   ,        ,        ,
C                             EOF     ,FLOAT   ,INT     ,(DECODE)
C
C         I/O DEVICES        -INPT    ,        ,        ,
C
C
C EXTERNAL ROUTINES: (IBM VERSION)
C  SPECIAL:          BASE8, DECODE
C  GEODOP:           MINUT
C  FORTRAN:          DBLE, IDINT, MOD
C
C NOTE: COLUMNS 1-3 OF ALL HEADERS MUST BE IDENTICAL (SEE FORMAT 901).
       COMMON NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT , NSTN
     +      , IFIT , ORBP , COEF , NDOP , ISTN , PRES , TDRY , VPAR
     +      , DOPP , TMME , IFLG , NSAT , NORD , ISWT , LOKG , KDOP
     +      , NPSI , NPSA , NDPI , NDPA , IRCV , INTA
       COMMON /DEV/ IEPH , INPT , IOUT , IRED , IPRT
       COMMON /TIM/     A ,    B , ALOK , DTAU , EXPT , GXYZ , IORD
     +      , MESS , SIGS , TTAU , VPVP , FREQ , TAUT , FIRT , FIRT2
       COMMON /PAR/ DAYP , TPER , XMOT , APTP , DATP , ECCT , RAAN
     +      , DRAN , RAGR , ANGL , JSAT , IEPO, PERD
      COMMON/DAT/  IFRQ, MAXD
C--- UNIT NUMBER FOR IBM DECODE OPERATION.
       DIMENSION ORBP( 7) , COEF(30) , DOPP(50) , TMME(50) , KDOP(50)
       DIMENSION GXYZ( 3) , EXPT(99) ,  A(3,2) , B(3,3,3) , MESS( 3)
       DIMENSION INTA(30) , LINE( 6) , TAUT(10)
       EQUIVALENCE (LINE24,LINE)
       DATA IBLK /  '    '/ , MASS /'//  '/ , TIMT / 0.D0 /
     +    , DMAX / 2.48D6 / , DMIN / 1.36D6 / , SCAL / -.10909091D0 /
     +    , MINL/ 10/, RADX/4.D-6/, SECD/ 0.D0 /
     +    , MOD3 / 2048 / , MOD4 / 4096 / , STOM / .016666667D0 /
     +    , IHOR /    0 / ,IHDR/'    ' /  , IFRM/0/
      DATA MASI/ '*** '/
      DATA MMINUS/'-   '/
C
   10  I    = 1
       ISWT = 1
       NDOP = NDPI
      IF(IHDR.EQ.IBLK) GO TO 20
      READ(LINE24,901) IWORD
  901 FORMAT(A3,7X,14X)
      IF(IWORD.EQ.IHDR) GO TO 21
C
   20  READ  (INPT, 5, END=22) LINE , ISLS
C      IF (EOF(INPT).NE.0.) RETURN
      GO TO 24
   22 RETURN
   24 CONTINUE
C     IF(LINE(2).EQ.IBLK) GO TO 20
      DO 26 J=1,5
      IF(LINE(J).EQ.IBLK) GO TO 20
   26 CONTINUE
C     CALL ERRSET(IER,50)
C   SET HEADR FORMAT 1(15) OR 2(17)
      IF(IFRM.EQ.0.AND.ISLS.EQ.IBLK) IFRM=1
      IF(IFRM.EQ.0.AND.ISLS.NE.IBLK) IFRM=2
      IF(IHDR.EQ.IBLK) GO TO 21
C     IF((LINE(1).AND.MSKH).NE.IHDR) GO TO 20
      READ(LINE24,901) IWORD
      IF(IWORD.NE.IHDR) GO TO 20
21     CONTINUE
      IF(IFRM.EQ.1) READ(LINE24,15) IFIT, IY, IDAY, ISAT
      IF(IFRM.EQ.2) READ(LINE24,17) IFIT, IY, IDAY, ISAT
      DO 225 II=2,20,2
      IF(ISAT.EQ.INTA(II)) THEN
        ISAT= INTA(II-1)
        GO TO 28
      ENDIF
225    CONTINUE
28    CONTINUE    
      IF(NDOP.EQ.0.AND.IEPO.EQ.1) NSAT= ISAT
        IF(ISAT.LE.0) GO TO 20
      IF(ISAT.GE.21.AND.ISAT.NE.75) GO TO 20
C  IFIT AGENCY IFIFIT= 8 DENOTES PREDICTS SYNCRONISATION
C  FOR GEOS-3 AND CMA725
      DSCLE= 1.000000D 00
      DELTF= 0.D0
      SCAL= - .1090909D0
      IF(ISAT.NE.75)   GO TO 29
      DSCLE= 1.000000D 00/.81D0
      SCAL= -1.000000D00/.81D0/9.D0
      DELTF= 720000.D0
29      CONTINUE
      READ(LINE24,901) IWORD
      IF(IHDR.EQ.IBLK) IHDR=IWORD
       NPSI = NPSI + 1
       IF (LINE(5).NE.IBLK) GO TO 40
       IF (IFLG.EQ.1)       GO TO 10
       GO TO 60
C
   30  IMIN = IMIN + IDINT(SECD + SECD)
       LOKG = MINUT (IDAY , IHOR , IMIN)
       TTAU = SECD
      ISWT=2
       IF (TTAU.GT.(.5D0))  TTAU = TTAU - 1.D0
      IMD=MOD(IMIN,2)
      TTAU=TTAU+IMD
      LOKG=LOKG-IMD
C
   40  SECP = SECD
C      LINE(2) = IBLK
       READ  (INPT, 5, END=91) LINE
C     IF(EOF(INPT).NE.0.)GOTO91
C--- TERMINATE PASS IF UNEXPECTED BLANKS OCCUR (USE MET. DATA FROM
C     LAST PASS, ALTHOUGH UNDEFINED ON FIRST PASS).
      IF(LINE(3).EQ.IBLK.OR.LINE(4).EQ.IBLK.OR.LINE(5).EQ.IBLK)
     @ GO TO (10,80), ISWT
C
   60 ISLS=LINE(5)
      IF(ISLS.EQ.MASS.OR.ISLS.EQ.MASI) GOTO(10,79),ISWT
C
      READ(LINE24,901) IHDT
      IF(IHDT.NE.IHDR) GO TO 82
C  LINE IS A HEADR  START A NEW PASS + BACKSPACE  HEADR
      GO TO 80
82      CONTINUE
       ITIM = IHOR
      READ(LINE24,35) IHOR,IMIN,TIMM,IIFRC,IIDOP,IIREF
      CALL BASE8(IIFRC,IFRC)
      CALL BASE8(IIDOP,IDOP)
      CALL BASE8(IIREF,IREF)
       IF (IHOR.LT.ITIM.AND.ISWT.GT.1) IDAY = IDAY + 1
       SECD = (DBLE(IFRC) * RADX + TIMM) * STOM
       TIMP = TIMT
       TIMT = DBLE(MINUT (IDAY , IHOR , IMIN))
      IF(ISWT.EQ.1)GOTO30
C
   70  IF (IREF.GT.MOD3)    IREF = IREF - MOD4
      IF(IREF.GT.MOD3 ) IREF=0
       ISLS = 1
       TIMM = TIMT - TIMP + SECD - SECP
C   CHECK FOR REASONABLE DELTA T
      IF(TIMM.GT.5.D0) TIMM=.1D0
      IF(TIMM.LE.1.D-09) GO TO 40
      DOPT= DBLE(IDOP)*DSCLE + DBLE(IREF)*SCAL + DELTF*TIMM
      TDOP= DOPT/ TIMM
C
       IF (DMAX.LT.TDOP.OR.DMIN.GT.TDOP.OR.LINE(3).EQ.IBLK) ISLS = 0
       DOPP(I) = DOPT * DBLE(ISLS)
      IONOS= DBLE(IREF)*SCAL*10.00000D0*DBLE(ISLS)
      DOPP( I)= DOPP(I) + DBLE(IONOS)*1.000000D 07
       TMME(I) = TIMM
       KDOP(I) = ISLS
       I    = I    + 1
       NDPI = NDPI + 1
       NDPA = NDPA + ISLS
       GO TO 40
C--- READ TRAILER.
79      READ(LINE24,25) ISGN,TDRY,PRES,VPAR,IFRQ
           SIGN=1.
C          IF(ISGN.EQ.1H-) SIGN=-1.
      IF(ISGN.EQ.MMINUS) SIGN=-1.D0
           TDRY= TDRY*SIGN
C
   80  NDOP = NDPI - NDOP
        IF (NDOP.LT.MINL)    GO TO 10
       IF (NDOP.GT.MAXD)    NDOP = MAXD
       IF (PRES.LT.500.D0)  PRES = PRES + 1000.D0
      IF(ISAT.NE.NSAT)GOTO10
   90  RETURN
   91 ISWT=3
      RETURN
C
    5 FORMAT(7A4)
15      FORMAT(1X,2I1,I3,I2)
17      FORMAT(2X,2I1,I3,I2)
25      FORMAT(4X,A1,F2.0,F3.0,F2.0,I2,1X,A3)
35      FORMAT(2I2,F2.0,I6,I8,I4)
C
       END
      SUBROUTINE BASE8(NUM8,NUM10)
C
C   VERSION:  9/16/82 *UNTESTED*       GEODOP IV+V
C   WRITTEN TO RUN USING EITHER IBM OR CDC FORTRAN.
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C         "BASE8     " - AUTHOR        - B. ARCHINAL
C                      - WRITTEN ON    - JANUARY 5, 1981
C                      - LAST CHANGE   - JANUARY 5, 1981
C                      - REFERENCES    - D. BROWN, SAGA SUB. "BASE"
C
C          VARIABLES USED     - I,IB8,IB10,ITEMP,NUM,NUM8,NUM10
C          VARIABLES ALTERED  - *NONE*
C          VARIABLES RETURNED - NUM10
C          EXTERNAL ROUTINES  - *NONE*
C          I/O DEVICES        - *NONE*
C
C      CONVERTS BASE 8 NUMBER (NUM8) TO BASE 10 NUMBER (NUM10).
C
      IB8=8
      IB10=10
      NUM10=0
      NUM=NUM8
      DO 100 I=1,20
      ITEMP=NUM-(NUM/IB10)*IB10
      NUM10=NUM10+ITEMP*IB8**(I-1)
      NUM=NUM/IB10
      IF(NUM.EQ.0) GO TO 200
  100 CONTINUE
      STOP    1234
  200 RETURN
      END
       SUBROUTINE READT2
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      READS AND CHECK STANDARD TRANET II DOPLER DATA
C                                    J.K.      Feb. 1988
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
       CHARACTER*10   ISTN, NAME
       CHARACTER*1    IEND, IENE, KCODE
C
       CHARACTER*4   LINE , IHD  , IBLK , ISRC
       CHARACTER*44  LINEH
       CHARACTER*16  LINED
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
       DIMENSION LINE(11) , TAUT(10) , INTA(30)
C
       EQUIVALENCE (LINEH,LINE(1)),(LINED,LINE(1))
C
      DATA IEND/'"'/,IENE/'/'/,STOM/.1666666666667D-01/
     +   , SECP/0.D0/, TIMD/0.D0/
     +,IHD/'    '/,IBLK/'    '/
     +  , MAXS/50/, TOFF/ .012733333D0/
C
   10  ISWT = 1
       ISWU = 0
C
   20  ISWU = ISWU + 1
C
   30 READ(INPT,5,END=32) KCODE,(LINE(I),I=1,11)
      IF(KCODE.EQ.IENE) GOTO 60
      READ(LINEH,115) ISTNN,KSAT,IYER,IDAY,IHOR,IMIN,KNDOP,NQ,
     + MCLOSE, MFREQE, NTEM, NPRES, NHUMID
 115   FORMAT(A3,1X,I3,1X,I2,I3,1X,2I2,1X,I2,1X,I1,I4,I5,I3,1X,I3,1X,I2)
      IF(ISWU.EQ.1) GO TO 40
      GO TO 30
   40  ISWU = 2
C   COMPUTE THE BASE TIME BY CALCULATING THE 2 MOST SIGNIFICANT DIGITS * 1000
      TESTT= IHOR*3600.D0+ IMIN*60.0D0
      BASET= DMOD(TESTT,1000.D0)
      BASET= TESTT-BASET
      TIMO = BASET
C  INITIALIZATIONS
      ISAT= KSAT
      IF(KSAT.LE.50) GO TO 28
      DO 225 II=2,22,2
      IF(KSAT.EQ.INTA(II)) THEN
        ISAT= INTA(II-1)
        GO TO 28
      ENDIF
225    CONTINUE
28    CONTINUE    
       ICOD =1
       ICDD = ICOD
       IFRC = 0
       IONE= 0
       LOKG= IDAY*1440
       NPSI = NPSI + 1
       NDOP = 0
       NTIM = 0
       GO TO 30
C
   42  ISWT =    3
       ISWU =    1
      TIMC= TIMD*STOM
      DTAU=DBLE(IDINT(TIMC))*.600000000000D 02
      LOKG= LOKG+ IDINT(TIMC)
      IONE= LOKG - (LOKG/2)*2
      TTAU= SECD*STOM
      ALOK= (TIMD-DTAU)*STOM
C
   50  SECP = SECD
C                        READ IN THE TIME DATA
   60 READ(INPT,5,END=32) KCODE,(LINE(III),III=1,4)
      IF(KCODE.EQ.IENE) GO TO 80
      IF(ISAT.NE.NSAT) GO TO 60
      IF(KCODE.EQ.'0') GO TO 81
C            TWO-MINUTE MARK
      READ(LINED,125) TIMP1,TIMC
125   FORMAT(F5.0,F6.6)
      TIMA= 9
      TIMB= DMOD(TIMP1,1000.D0)
      TIMO= TIMP1- TIMB
C           RESET THE TIME BASE  (1000 S)
      BASET= TIMO
      TESTT= TIMP1
      SECD = TIMC
       GO TO (70 , 68) , ISWU
C  STANDART TIME DETECTED
81    READ(LINED,135) TIMB,TIMC,KNTDOP,KNTREF
135   FORMAT(F3.0,F6.6,I2,I5)
C           CHECK & ADD BASE TIME
        TIMP= TIMD
      TIMD= TIMB + BASET
      DT1 = (TESTT-TIMD)
        IF(DT1.LE.1.0D0) GO TO 82
        BASET= BASET + 1000.D0
        TIMD = TIMD + 1000.D0
82      CONTINUE
C                CALCULATE DOPPLER
        DOP= KNTDOP*(2.0D0**KNDOP)
C     CALCULATE & SAVE IONOS REF. COUNT (FOR 3RD IONOS REFR. ONLY)
        IONOS= KNTREF-20000
        IF(IABS(IONOS).GT.1840) IONOS=0
C   -9/55* 1/8 *10 = -2.04545D-01
        IONOS= -IDINT(DBLE(IONOS))*2.04545D-01
        DOP= DOP + 1.00000000D 07*DBLE(IONOS)
        SECD = TIMC
        IF(ISWU.GT.1) GO TO 42
        IF(KNTDOP.EQ.0.AND.NDOP.LE.10) THEN
        NDOP = 0
        LOKG = IDAY*1440
        GO TO 42
        ENDIF
C           COMPUTE THE DIFFERENCE ADN DOP COUNTS
       NDOP= NDOP + 1
       IF (NDOP.GT.MAXS) GO TO 50
       NDPI = NDPI + 1
       DOPP(NDOP)= DOP
       KDOP(NDOP) = 1
       TMME(NDOP)= (SECD-SECP+TIMD-TIMP)*STOM
C               CHECK FOR REASONABLE DELTA T
      IF(TMME(NDOP).GT.5.D0) TMME(NDOP)= .1D0
       IF(ICDD.EQ.4) DOPP(NDOP)= DOPP(NDOP)+ TMME(NDOP)*7.2000000D 05
       GO TO 50
C
   68  DTAU = TIMO + TIMB
C                CHECK FOR VALIDITY OF TWO-MINI=UTE MARK
   70  NTIM = NTIM + 1
       IF(NTIM.GT.10) GO TO 60
       TAUT(NTIM)= ( TIMO+TIMB-DTAU+SECD)*STOM- TOFF
       ITWO = IDINT(TAUT(NTIM)/2.0D0)*2+IONE
       TTEST = DABS(TAUT(NTIM)-DBLE(ITWO))
       IF(TTEST.LT.STOM) GO TO 60
       TAUT(NTIM)= 0.D0
       NTIM= NTIM -1
       GO TO 60
C                  END OF PASS
   80  IF (NDOP.EQ.0)       GO TO 10
       ISWT = 2
       IF (NDOP.GT.MAXD) NDOP = MAXD
       NDPA= NDPA + NDOP
       PRES= NPRES
       IF(PRES.LT.500.D0) PRES=PRES+1000.D0
       TDRY= DBLE(NTEM)
       VPAR= DBLE(NHUMID)
32     RETURN
C
    5 FORMAT(A1,10A4,A2)
   15 FORMAT(1X,A3,1X,2I2)
   25 FORMAT(F1.0,F4.0,F6.6)
       END
C
      SUBROUTINE RMERGE(NPRN)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 03/34/83                  GEODOP 5.04   (DEC81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C        "RMERGE    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - DECEMBER /74.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -IRCV    ,ISWT    ,LOKR    ,LOKG
C                             NDOP    ,NPSI    ,ISAT    ,DTAU
C                             SIGS    ,TDRY    ,VPAR    ,PRES
C                             NTIM    ,KDOP    ,MESS    ,IOUT
C
C         VARIABLES ALTERED  -KDOP    ,        ,        ,
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -READGE  ,READTR  ,READEF  ,WRITE5
C                             DDHHMM  ,MODULO  ,        ,
C
C         I/O DEVICES        -IPRT    ,        ,        ,
C
       CHARACTER*10  NAME , ISTN
       CHARACTER*4   ISRC
C
       COMMON NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT , NSTN
     +      , IFIT , ORBP , COEF , NDOP , ISTN , PRES , TDRY , VPAR
     +      , DOPP , TMME , NTIM , NSAT , NORD , ISWT , LOKG , KDOP
     +      , NPSI , NPSA , NDPI , NDPA , IRCV , INTA 
       COMMON /DEV/  IEPH , INPT , IOUT , IRED , IPRT
       COMMON /TIM/     A ,    B , ALOK , DTAU , EXPT , GXYZ , IORD
     +      , MESS , SIGS , TTAU , VPVP , FREQ , TAUT , FIRT , FIRT2
       DIMENSION ORBP( 7) , COEF(30) , DOPP(50) , TMME(50) , KDOP(50)
       DIMENSION GXYZ( 3) , EXPT(99) ,  A(3,2) , B(3,3,3) , MESS( 3)
     +,  TAUT(10),  INTA(30)
       DIMENSION ISPN ( 3)
C 
      DATA MAXW/8/,ILIN/1/
       ISCN = 0
C
   10  ISKP = 1
C
20     GO TO (22,32,32,33,33),IRCV
C
22    CALL READGE
      IF(ISKP.GT.1) GO TO 50
      GO TO 40
C           READS AND DECODES TRANET I DATA
   32  CALL READTR
      IF(ISKP.GT.1) GO TO 50
      GO TO 40
C           READS AND DECODES TRANET II DATA
   33  CALL READT2
      IF(ISKP.GT.1) GO TO 50 
C
   40  CALL READEF
50      CONTINUE
      IF(ISWT.EQ.2.OR.ISWT.EQ.3) GO TO 60
      GO TO 100
C
   60  ISKP = 2
      IF((LOKR-LOKG +7)/MAXW) 40,80,20
C
   80  CALL WRITE5(FIRT, FIRT2)
       CALL DDHHMM (LOKG , ISPN)
       ILIN = MOD(ILIN , 59) + 1
       IF (ILIN.EQ.2) WRITE (IPRT, 5)
       IF (ILIN.EQ.2) WRITE (ISCN, 5)
       ISKP = NDOP + 1
           DO 200 I = ISKP,32
  200      KDOP(I) = 0
       WRITE (IPRT,15) NPSI , ISAT , ISPN , DTAU , SIGS , NTIM , TDRY
     +     , VPAR, PRES, (KDOP(I),I=1,32),  MESS(1), MESS(2)
       WRITE (ISCN,15) NPSI , ISAT , ISPN , DTAU , SIGS , NTIM , TDRY
     +     , VPAR, PRES, (KDOP(I),I=1,32),  MESS(1), MESS(2)
C
      GO TO 10
100      NPRN= NPRN-1
      IF(NPRN.LE.0) GO TO 110
      IF(ISWT.GT.3) GO TO 110
      ISWT=2
      GO TO 10
110    ENDFILE IOUT
       RETURN
C
    5  FORMAT (50H1PASS  SAT    LOKON      DLAY    SIGS  NO.   DRY
     +        ,10HPAR  PRESS,10X,25HU S E D   D O P P L E R S ,18X
     +        ,13HN  O  T  E  S)
   15  FORMAT (3I5,2(1H/,I2),F9.3,F7.3,I4,F7.0,F5.0,F7.0,2X,8(1X,4I1),2X
     +        ,3A10)
       END


