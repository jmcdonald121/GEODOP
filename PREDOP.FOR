C PREDOP
C output modified by Bolzan - 4/3/90
      PROGRAM PREDOP
      IMPLICIT REAL*8 (A-H,O-Z)
        CHARACTER*1 IFET,IMET
        CHARACTER*4 ISRC, IBLNK, ITREJ, ISA, INF, INS, ISTO
         CHARACTER*2 IDIR
         CHARACTER*1 IYES
         CHARACTER*10 NAME , ISTN, DAYT , TIMME
C FILE NAME DEFFAULTS
      CHARACTER*40 FNAME,FBLNK, FNAMIN, FNAMOU
      CHARACTER*10 GRVINP,PREINP,PREOUT,PREEPH,PREMJV,GEOINP,PRESC1
      INTEGER*2 ITIMME(3), IDATE(3)
C
C   VERSION: 03/20/83                  GEODOP 5.10 (NOV82)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C    B. ARCHINAL, O.S.U.
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C        "PREDOP    " - AUTHOR        - P.G. LAWNIKANIS
C                     - COAUTHOR      - J. KOUBA
C                     - WRITTEN ON    - SUMMER /74.
C                     -LAST COMPILED JULY/1981
C                     - REFERENCES    - J.KOUBA .. PROGRAM PREDOP
C                                     - J.KOUBA .. PROGRAM GEODOP
C
C         EXTERNAL ROUTINES  -DEFALT  ,REDUCE  ,DGEOCA  ,DIST
C                             MINUT   ,VPSAT   ,CHEBY   ,INPUT
C                             DECOD1  ,SATCRD  ,SCREEN  ,METDAT
C                             PRINTP  ,        ,        ,
C                             ABS     ,ATAN    ,COS     ,DATE
C                                              ,EXP     ,DBLE
C                             INT     ,SIN     ,SQRT    ,
C
C
C        COMMON BLOCKS & DATA CONSTANTS-
C
       COMMON /DAT/ ANOT , AZIM , CDOP , CELV , CPAP , DATM , DNOM ,
     +              DTIM , ECCS , ELEV , ELMX , FRCV , IRCV , METL ,
     +              METP , NDIR , TMME , XMET ,
     +                BT ,   C4 ,   CC ,   CM ,LB,IDUM1, PI ,   RD ,
     +                RH ,   SS ,   ST ,   WE ,   XP
       COMMON /DEC/ TSPR , DMAN , APTP , DATP , ECCT , SMAX , RAAN ,
     +              DRAN , CORI , RAST , TSAT , TNJC , SORI , FSAT ,
     +              DDMN , TPER , LOKM
       COMMON /FIT/    L ,    M ,N,IDUM, DACA , DECA , DOPC , KDOP ,
     +               DOP ,  TIM , RESD , XNOR
       COMMON /IOS/  ICD ,  INP ,  IOT ,  IPT , ISTA , ISTB , IRJT ,
     +              LOKH , LOKR , JDOP , LDOP , MDOP , NDOP
       COMMON /MJV/ IVAR , IFIX , IDAY , IHOR , IMIN , NOTE, KSAT
      COMMON/MET/ METL1, METP1, XPRES, DTDRY, DVPAR, PRES, TDRY, TWET
C--- SPECIAL LABELED COMMON TO CARRY VAR. PARM. INFO. (OSU VERSION).
      COMMON / OSU / IDAT
       DIMENSION DATM(   3) , DNOM( 4,8) , DTIM(   4) , ELEV(  33) ,
     +           XMET(4,1000), DACA(  14) , DECA(  14) , DOPC(  14) ,
     +           KDOP(  48) ,  DOP(  96) ,  TIM(  96) , RESD(  48) ,
     +           IVAR(  14) , IFIX(  14) , NOTE(  32) , PSTN(   7) ,
     + STNC(3),XBAS(10,35),CXYZ(10,3),IDIR(4), HEDR(5)
     1,SATER(68)
C**WARNING** C,TT,O,Q,QL ARE DIMENSIONED FOR UP TO NMAX=11, IF HIGHER N
C            IS REQUIRED, DIMENSION O(2*NMAX+2) AND C,TT,Q,QL(NMAX**2+NM
C  NMAX= TRUNCATION (DEGREE AND ORDER) +1
       DIMENSION C(  66), TT( 66), O(25), Q( 66), QL( 66)  ,DDC(10)
     1, RN(3), KSAT(17)
C
      EQUIVALENCE (DAYT,IDAYT)
C
      DATA VPV,DF,TO,TDT,SO/4*0.D0,.10D 00/,NTPAC/0/,ITPRJ/0/
     2,RN/1.D 12,0.D0,5.D 05/
C
      DATA IFET/'F'/,ISRC/'BDCT'/,
     + LENG/29/,IORD/10/,NSTN/1/,NTIM/0/,DLT/0.200000D 01/ ,
     +      IDIR /  'SW',  'SE',  'NW',  'NE'/      , NPAS /  0 /
     1,SATER/17*0.D0,17*25.D-04,17*2.D0,17*0.D0/
C
      DATA IBLNK/ '    '/,ITREJ/'T-RJ'/,ISA/'SARC'/,INF/'NEWF'/
      DATA INS/'NEWS'/
C   TRUNCATED GEM9 10 BY 10 (TRUNCATED) GRAVITY FIELD.
      DATA C/
     10.D0,0.D0,-0.4841656D-03,0.95848D-06, 0.54154D-06, 0.68439D-07,
     1-0.15121D-06, 0.93313D-07, 0.50949D-07, 0.27331D-07, 0.52837D-07,
     1 0.D0       ,-0.21407D-09, 0.20283D-05,-0.53287D-06,-0.49104D-07,
     1-0.75431D-07, 0.26724D-06, 0.24830D-07, 0.16065D-06, 0.82382D-07,
     1 0.24340D-05, 0.89198D-06, 0.35259D-06, 0.64975D-06, 0.48125D-07,
     1 0.32786D-06, 0.73146D-07, 0.24454D-07,-0.84286D-07, 0.70256D-06,
     1 0.98849D-06,-0.47030D-06, 0.59104D-07, 0.23987D-06,-0.99218D-08,
     1-0.17360D-06,-0.20707D-07,-0.19661D-06,-0.29049D-06,-0.10458D-06,
     1-0.28553D-06,-0.24624D-06,-0.11342D-07,-0.10099D-06, 0.16123D-06,
     1-0.25597D-06, 0.16671D-07,-0.12511D-07, 0.49044D-08,-0.62158D-07,
     1 0.18798D-08,-0.36330D-06,-0.74711D-07, 0.37370D-07,-0.38882D-07,
     1-0.92368D-08, 0.67780D-07,-0.10449D-06,-0.22303D-09,-0.12123D-06,
     1 0.19418D-06, 0.39887D-07,-0.58096D-07, 0.12430D-06, 0.10563D-06/
      DATA TT/12*0.D0,-0.40556D-08, 0.25244D-06,-0.46512D-06,
     3-0.92254D-07, 0.15868D-07, 0.96067D-07, 0.63270D-07, 0.16763D-07,
     3-0.13553D-06,-0.13979D-05,-0.62241D-06, 0.66272D-06,-0.32488D-06,
     3-0.34854D-06, 0.10505D-06, 0.51207D-07,-0.24668D-07,-0.37526D-08,
     3 0.14114D-05,-0.20316D-06,-0.20788D-06, 0.13848D-08,-0.21778D-06,
     3-0.88297D-07,-0.97148D-07,-0.16867D-06, 0.29894D-06, 0.50367D-07,
     3-0.46086D-06,-0.12689D-06, 0.75392D-07, 0.10722D-07,-0.68895D-07,
     3-0.66181D-06,-0.53881D-06, 0.43674D-07, 0.82452D-07,-0.64103D-07,
     3-0.35501D-07,-0.24196D-06, 0.13170D-06, 0.32546D-06, 0.21837D-06,
     3-0.85968D-07, 0.26630D-07, 0.77116D-07,-0.74109D-07, 0.16160D-07,
     3 0.12712D-06,-0.15551D-07,-0.71589D-07, 0.91719D-07,-0.50531D-07,
     3-0.19621D-07/
C
C  DEFAULT FILE NAMES
      DATA GRVINP/'NUL.GRV   '/,
     + PREOUT/'PREDOP.TXT'/,PREEPH/'NUL.EPH   '/,GEOINP/'GEODOP.BIN'/,
     + PRESC1/'PREDOP.SC1'/,PREMJV/'PREDOP.MJV'/,
     + FBLNK/'                                        '/ 
C
C--- FAHRENHEIT TO CELSIUS DEGREES CONVERSION FUNCTION.
       CELS (XX) = (XX - 32.D0)* .55555556D0
C
C  PREDOP CONSTANTS
      IEOF=0
      WE= 4.375269513D -03
      PI= 3.14159265359D 00
      CM=1.798754748D 10
      C4= CM/.6D02/.4D09
      DTIM(1)=.536786250D 00
      DTIM(2)=.460102500D 00
      DTIM(3)=.536786250D 00
      DTIM(4)=.466328958D 00
      BT=1.966246D-02
      C4=7.4948125D-01
      CC=.2D0
      LB=6103
      SIC=0.D0
      SS=6.25D0
      ST=0.D0
C  VALID SATELLLITE NUMBERS CHANGE WHEN REQUIRED
      KSAT(1)= 13
      KSAT(2)= 20
      KSAT(3)= 11
      KSAT(5)= 48
      KSAT(4)= 30
      KSAT(6)= 50
      KSAT(7)= 12
      KSAT(8)= 14
      KSAT(9)= 19
      KSAT(10)= 24
      KSAT(11)= 23
      KSAT(12)= 25
      KSAT(13)= 27
      KSAT(14)= 29
      KSAT(15)= 31
      KSAT(16)= 32
      KSAT(17)= 49
      XP=2.576D0
C--- SET UP I/O UNIT NUMBERS.
      ICD=0
      IPT=6
       ICF=11
      INP=12
      IOT=13
      ISC=14
      IF99=1
         IF18= 18
C      IBM PC VERSION
C     LUN  FORMAT   TYPE     DEFAULT NAME   DESCRIPTION         VARIABLE
C     ---  ------   ----     ------------   ------------------  -------- 
C      IF18 ASCII   INPUT    NUL.GRV        GRAVITY FIELD INPUT GRVINP
C      IPT ASCII    OUTPUT   PREDOP.TXT     PRINTOUT            PREOUT
C     ICF  BINARY   OUTPUT   NUL.EPH        EPHEMERIS FILE      PREEPH
C     INP  ASCII    INPUT    PREDOP.MJV     MJV DATA            PREMJV
C     IOT  BINARY   OUTPUT   GEODOP.BIN     GEODOP INPUT FILE   GEOONP
C     ISC  BINARY   SCRATCH  PREDOP.SC1     PLOT SCRATCH FILE   PRESC1
C     ISCN ASCII    I/O      SCRN:(ISCN=0)  SCREEN OUTPUT       N/A
         ISCN= 0
C
C-----------------------------------------------------------------
C
      CALL GETTIM(ITIMME(1),ITIMME(2),ITIMME(3),ITSEC)
      CALL GETDAT(IDATE(3),IDATE(1),IDATE(2))
      WRITE(TIMME,'(1X,2(I2,'':''),I2)') ITIMME
      WRITE(DAYT,'(2(I2,''/''),I4)') IDATE
C
      WRITE(ISCN,1234) DAYT,TIMME
1234  FORMAT(' Program  P R E D O P - Geodopv Preprocessor (June 87)',3X
     &,A10,2X,A10/ ' for CMA722,CMA751, JMR1 & MX1502 data files'/)
      WRITE(ISCN,'(A\)') ' MJV input file (asc /bin)[PREDOP.MJV]: ' 
      FNAMIN=FBLNK
      READ(ISCN,'(A)',END=12) FNAMIN
      IF(FNAME.EQ.FBLNK) FNAMIN= PREMJV
      WRITE(ISCN,'(1X,(A))') FNAMIN
C
      WRITE(ISCN,'(A\)') ' GEODOP input file (bin.) [GEODOP.BIN]: ' 
      FNAMOU=FBLNK
      READ(ISCN,'(A)',END=12) FNAMOU
      IF(FNAME.EQ.FBLNK) FNAMOU= GEOINP
      WRITE(ISCN,'(1X,(A))') FNAMOU
      OPEN(IOT,FILE=FNAMOU,STATUS='NEW',FORM='BINARY',ERR=11,
     +   IOSTAT=IERROR)
C
      WRITE(ISCN,'(A\)') ' PREDOP output (text) file[PREDOP.TXT]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=12) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= PREOUT
      WRITE(ISCN,'(1X,(A))') FNAME
      OPEN(IPT,FILE=FNAME,STATUS='NEW',ERR=11,IOSTAT=IERROR)
C                 
      WRITE(IPT,1234) DAYT,TIMME
      WRITE(IPT,'(A\)') ' MJV input file (asci/bin)[PREDOP.MJV]: ' 
      WRITE(IPT,'(1X,(A))') FNAMIN
      WRITE(IPT,'(A\)') ' GEODOP input file (bin.) [GEODOP.BIN]: ' 
      WRITE(IPT,'(1X,(A))') FNAMOU
C
      WRITE(ISCN,'(A\)') ' Gravity field input file [  NUL.GRV ]: ' 
      WRITE(IPT,'(A\)') ' Gravity field input file [  NUL.GRV ]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=12) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= GRVINP
      WRITE(ISCN,'(1X,(A))') FNAME
      WRITE(IPT,'(1X,(A))') FNAME
         OPEN(IF18,FILE=FNAME,STATUS='OLD',ERR=11,IOSTAT=IERROR)
C
      WRITE(ISCN,'(A\)') ' PREDOP eph. output (bin) [  NUL.EPH ]: ' 
      WRITE(IPT,'(A\)') ' PREDOP eph. output (bin) [  NUL.EPH ]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=12) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= PREEPH
      WRITE(ISCN,'(1X,(A))') FNAME
      WRITE(IPT,'(1X,(A))') FNAME
      OPEN(ICF,FILE=FNAME,STATUS='NEW',FORM='BINARY',ERR=11,
     +   IOSTAT=IERROR)
C
      OPEN(ISC,FILE=PRESC1,STATUS='NEW',FORM='BINARY',ERR=11,
     +   IOSTAT=IERROR)
C
C--- ZERO OUT MET. DATA ARRAY.
      DO 3 I=1,4
      DO 3 J=1,1000
    3 XMET(I,J)=0.D0
C--- NUMBER OF METEORLOGICAL DATA POINTS AVAILABLE.
C     (MOVE TO AFTER ST. NUM 1 IF MET. DATA IS TO BE RESET
C     FOR EACH INPUT DATA SET.)
      ITEM=0
C--- SET MAXIMUM NUMBER OF DOPPLERS PER PASS.
C     CHANGED FROM 10 TO 6, JAN82.
      MDOP=6
       RMU=.39860050D15
       AE=6378137.D0
       WWE= WE*WE
      ISET= 0
C
    1 CONTINUE
      NPAS =0
      NTIM=0
      METL= 1
      CLKJMR= 0.D0
C
C        READ / PRINT OPTIONS & SET DEFAULTS IF OK-
C
      RD= PI/.180000000000D 03
      RH= .64800000000D 06/PI
      IF(IEOF.EQ.1) GO TO 12
C
       WRITE(ISCN,'(A\)') ' Figure name (A10)     :'
       READ(ISCN,'(A)') NAME
       WRITE(ISCN,'(1X,A10)') NAME
       WRITE(ISCN,'(A\)') ' Station name (A10)    :'
       READ(ISCN,'(A)') ISTN
       WRITE(ISCN,'(1X,A10)') ISTN
       WRITE(ISCN,'(A\)') ' Test station name(A10):'
       READ(ISCN,'(A)') ISTO
       WRITE(ISCN,'(1X,A4)') ISTO
C
       WRITE(ISCN,'(A\)') ' Latitude  (ddd mm ss) :'
       READ(ISCN,*) (PSTN(I),I=1,3)
       WRITE(ISCN,'(1X,F6.0,F5.0,F7.1)') (PSTN(I),I=1,3)
       WRITE(ISCN,'(A\)') ' Longitude (ddd mm ss) :'
       READ(ISCN,*) (PSTN(I),I=4,6)
       WRITE(ISCN,'(1X,F6.0,F5.0,F7.1)') (PSTN(I),I=4,6)
       WRITE(ISCN,'(A\)') ' El. height (metres)   :'
       READ(ISCN,*) PSTN(7)
       WRITE(ISCN,'(12X,F7.1)') PSTN(7)
       WRITE(ISCN,'(A\)') ' Datum shift (X,Y,Z-m) :'
       READ(ISCN,*) DATM
       WRITE(ISCN,'(1X,3F6.1)') DATM
       WRITE(ISCN,'(A\)') ' # of dig.in var.par.(rec 0,3,4)[9]:'
       READ(ISCN,*) NINE
       WRITE(ISCN,'(1X,I6)') NINE
C
       WRITE(ISCN,'(A\)') ' Rec. Type (0-6)  [0]  :'
       READ(ISCN,*) IRCV
       WRITE(ISCN,'(1X,I6)') IRCV
C
       IF(IRCV.EQ.5) THEN
         OPEN(INP,FILE=FNAMIN,STATUS='OLD',FORM='BINARY'
     +,ERR=11,IOSTAT=IERROR)
       ELSE
         OPEN(INP,FILE=FNAMIN,STATUS='OLD'
     +,ERR=11,IOSTAT=IERROR)
       ENDIF
C
       WRITE(ISCN,'(A\)') ' Time adjustment [0]   :'
       READ(ISCN,*) ITADJ
       WRITE(ISCN,'(1X,I6)') ITADJ
       WRITE(ISCN,'(A\)') ' Doppler timming [0]   :'
       READ(ISCN,*) TMME
       WRITE(ISCN,'(1X,F6.0)') TMME
       WRITE(ISCN,'(A\)') ' Freq.offset (hz)[0]   :'
       READ(ISCN,*) FRCV
       WRITE(ISCN,'(1X,F7.1)') FRCV
       WRITE(ISCN,'(A\)') ' Dop. cutoff (deg) [5] :'
       READ(ISCN,*) CDOP
       WRITE(ISCN,'(1X,F6.0)') CDOP
       WRITE(ISCN,'(A\)') ' Pass cutoff (deg) [10]:'
       READ(ISCN,*) CELV
       WRITE(ISCN,'(1X,F6.0)') CELV
       WRITE(ISCN,'(A\)') ' Sigma miscl(m)[1000]  :'
       READ(ISCN,*) SIGC
       WRITE(ISCN,'(F7.0)') SIGC
       WRITE(ISCN,'(A\)') ' Sigma freq(hz) [30]   :'
       READ(ISCN,*) SIGF
       WRITE(ISCN,'(1X,F6.0)') SIGF
       WRITE(ISCN,'(A\)') ' CPA symmetry trim (0=y,1=no) [0]  :'
       READ(ISCN,*) IDOP
       WRITE(ISCN,'(1X,I6)') IDOP
       WRITE(ISCN,'(A\)') ' Semi-major axis-6378000m [206.4]  :'
       READ(ISCN,*) ANOT
       WRITE(ISCN,'(1X,F6.1)') ANOT
       WRITE(ISCN,'(A\)') ' Semi-minor axis-6378000m [759.8]  :'
       READ(ISCN,*) BNOT
       WRITE(ISCN,'(1X,F6.1)') BNOT
       WRITE(ISCN,'(A\)') ' Var parametr format(1=14,2=12) [1]:'
       READ(ISCN,*) IDAT
       WRITE(ISCN,'(1X,I6)')IDAT
       WRITE(ISCN,'(A\)') ' Shift blanks (0 or 1 blank)   [0] :'
       READ(ISCN,*) ISHF
       WRITE(ISCN,'(1X,I6)') ISHF
       WRITE(ISCN,'(A\)') ' Signal stat.reject 400 150mhz[0 0]:'
       READ(ISCN,*) ISTATH, ISTATL
       WRITE(ISCN,'(1X,2I6)') ISTATH,ISTATL 
       ISTATS= ISTATL*10+ ISTATH
       WRITE(ISCN,'(A\)') ' Grav. field trun.(=-1 broad) [11] :'
       READ(ISCN,*) NMAX
       WRITE(ISCN,'(1X,I6)')NMAX
       WRITE(ISCN,'(A\)') ' Gravity  field weighting [5]      :'
       READ(ISCN,*) PW
       WRITE(ISCN,'(1X,F6.0)') PW
       WRITE(ISCN,'(A\)') ' Met data input (y/n)? :'
       READ(ISCN,'(A)') IYES
       WRITE(ISCN,'(6X,A1)') IYES
C 
      GO TO 16
 11   WRITE(ISCN,1122) FNAME, IERROR
 1122 FORMAT(' OPEN FILE ERROR FNAME: ',A20,' IOSTAT= ',I5)
      GO TO 12
   13 WRITE(ISCN,'(A)')' Format read error in PREDOP input file:' 
C  TERMINATE PROGRAM
 12    CLOSE(ISC,STATUS='DELETE') 
      STOP
   16 CONTINUE
      IF(NMAX.EQ.0) NMAX=11
C  IF NMAX.GT.11, CHANGE DIMENSIONS OF C,TT,O,Q,QL AS INDICATED ABOVE,
C   AND CHANGE THE IF STATEMENT BELOW.
      IF(NMAX.GT.11) NMAX=11
      IF(PW.LT.1.D-06)   PW= 5.D0
C  INITILIZE AND/OR READ GRAVITY FIELD COEFICIENTS
C   IF NMAX.GT.0.AND.ISET=0 (FIRST PASS)
      IF(NMAX.EQ.-1) ISET=1
      IF(NMAX.GT.0) ISRC=ISA
      IF(ISET.EQ.0)   CALL   GEMINT(C,TT,O,NMAX,IF18)
      ISET= 1
      WRITE(IPT,15) NAME,ISTN,PSTN,DATM,NINE,IRCV,ITADJ,TMME,FRCV,
     +                IMET , CDOP , CELV , SIGC , SIGF , IDOP , ANOT ,
     +                BNOT , IDAT , ISHF
     +, ISTO,ISTATS,NMAX, PW
C
C  INITIALIZE FREQ. AND TIME LINE FIT
      VPV= 0.D0
      DF= 0.D0
      TO= 0.D0
      TDT = 0.D0
      SO  =  .1D 00
      NTPAC= 0
      ITPRJ= 0
      RN(1)= 1.D 12
      RN(2)= 0.D0
      RN(3)= 5.D 05
C REINITIALIZE SATER
C REINITILIZE SATER
      DO 7 I= 1,17
      SATER(I   )= 0.D0
      SATER(I+17)=  .25D-02
      SATER(I+51)= KSAT(I)
7     SATER(I+34)= 2.D0
       ANOT    = DEFALT (ANOT    ,206.4D0)+ 6.378D+6
       BNOT    = DEFALT (BNOT    ,583.8D0)+ 6.356D+6
       CDOP    = DEFALT (CDOP    ,  5.D0)
       CELV    = DEFALT (CELV    , 10.D0)
       SIGC    = DEFALT (SIGC    , 1.D04) * 1.333333333D0
       SIGF    = DEFALT (SIGF    , 3.D+2)
       ECCS    = (ANOT * ANOT - BNOT * BNOT) / (ANOT * ANOT)
       IDAT    =  IDAT + 1
       ISHF    =  ISHF + 1
C     IF(IRCV.EQ.2)ITRM=1
       PHI = REDUCE (PSTN(1) , PSTN(2) , PSTN(3))
       XLM = REDUCE (PSTN(4) , PSTN(5) , PSTN(6))
       CALL DGEOCA (STNC(1) , STNC(2) , STNC(3) , ANOT , ECCS , PHI
     +                , XLM , PSTN(7))
       CCC =DCOS(PHI) *DCOS(XLM)
       SCC =DCOS(PHI) *DSIN(XLM)
       CPP =DSIN(PHI)
C
      IF(IRCV.LE.0.AND.NINE.EQ.0)NINE=8
      IF(NINE.EQ.0)NINE=9
C--- CHECK FOR VALID RECEIVER TYPE.
      IF(IRCV.GE.0.AND.IRCV.LE.7) GO TO 24
      WRITE(IPT,901) IRCV
      STOP
   24 CONTINUE
C--- TEST STATION NAME IN I FORMAT IF MX-1502 DATA.
      IF(IRCV.NE.5.AND.IRCV.NE.7) GO TO 26
      READ(ISTO,902) ITSN
   26 CONTINUE
       IF(IYES.NE.'Y'.AND.IYES.NE.'y') GO TO 10
       WRITE(ISCN,'(A\)') ' Met units(M,F) [blank]:'
       READ(ISCN,'(A)') IMET
       WRITE(ISCN,'(6X,A1)') IMET
       WRITE(ISCN,'(A\)') ' EOF (^Z) ends met input. Press RETURN to con
     +tinue: '
       READ(ISCN,'(A)') IDUMMY
       WRITE(ISCN,'(1X,A4)') IDUMMY
C        READ / PRINT MET DATA IF ANY-
C   (THIS OSU VERSION READS RELATIVE HUMIDITY IF AVAILABLE.)
C
       METP = 1
       ISTA = 2
       IF (IMET.EQ.IFET) ISTA = 1
           DO 100 I = 1,1000
       WRITE(ISCN,'(A\)')' hh mm ddd pres Tdry Twet Ihum:'
       READ(ISCN,*,END=10,ERR=13) K,M,J,PRES,TDRY,TWET
     1,IRH
C              IF (EOF(ICD).NE.0.) GO TO 10
      WRITE(ISCN,25) K, M, J, PRES, TDRY, TWET,IRH
      WRITE(IPT, 35) K, M, J, PRES, TDRY, TWET,IRH
      IF(ISTA.EQ.2)GOTO4
      PRES =1013.246D0/DEXP(PRES * 3.679215D-5)
               TWET = CELS (TDRY - TWET)
               TDRY = CELS (TDRY)
         GO TO 6
4        TWET= TDRY- TWET
C6       METL= I
C              METL      = I
    6 ITEM=I
               XMET(1,I) =DBLE(MINUT(J,K,M))
               XMET(2,I) = TDRY
         XMET(3,I)= VPSAT(TWET) -4.5D-04*(1. +.00168D0*(TWET+273.D0))*
     +              (TDRY-TWET)* PRES
      IF(IRH.GT.100) IRH= 99
C    REL HUMIDITY INPUT (IRH)IF ANY OVERRIDES TWET INPUT IF ANY
      IF(IRH.GT.0) XMET(3,I)= VPSAT(TDRY)*DBLE(IRH)*.01D0
               XMET(4,I) = PRES
  100      CONTINUE
C
C        SET TIME BASE MATRIX FOR SAT. X/Y/Z COEFFICIENTS-
C
10    IEOF=1
14    METL=ITEM
      TBAS = 2.D0/DBLE(LENG - 1)
      ILT= IDINT(DLT +.1D0)
      J=0
      NN= LENG /ILT  +1
      II= NN-1
      TKK= TBAS*TBAS
      DO 200 I=1,LENG,ILT
      J= J+1
      N= J+NN
               XT =DBLE(I - 1) * TBAS - 1.D0
      CALL CHEBY(XBAS(1,J),XBAS(1,N),1,IORD, XT, TBAS)
C   COMPUTE 2ND DERIVATIVES OF CHEB POLYN
      IF(NMAX.EQ.-1) GO TO 200
       DDC(1)=0.D0
       DDC(2)=0.D0
       DDC(3)=4.D0* TKK
       DDC(4)= 24.D0*XT*TKK
       DO 51 K=5,IORD
       NK=K-1
       FN=DBLE(NK)
 51    DDC(K)=2.D0*   FN /DBLE(NK-1)*(TBAS*XBAS( K-1,N)+XT*DDC(K-1))
     1 -FN/DBLE(NK-2)*DDC(K-2)
      DO 52 K=1,IORD
C    SET XBAS FOR -10,0, 10 MIN.
      IF(J.EQ.3) GO TO 53
      IF(J.NE.8.AND.J.NE.13) GO TO 52
53     IF(K.EQ.1) II= II+2
      XBAS(K,II)= XBAS(K,J)/10.D0
      XBAS(K,II+1)= XBAS(K,N)/10.D0
52    XBAS(K,J)= DDC(K)* PW
  200      CONTINUE
C
       YT = 1.D0
C--- "." ON "1.D0" MISSING IN CDC VERSION.
       IF (TMME.EQ.1.D0) YT = .999998021D0
           DO 300 I = 1,4
      XT= DTIM(I)
                   DO 300 J = 1,8
                       DNOM(I,J) = XT
  300      CONTINUE
C
      WRITE(IPT,75)  DAYT , NAME , ISTN , ISTO , PSTN , STNC , DATM
     +              , ANOT ,
     +                BNOT , ECCS , IRCV , FRCV , TMME , CDOP
      WRITE(IPT,85) IDOP,CELV,SIGC,SIGF,IDAT,ISHF,NINE,IMET,NMAX,PW
      WRITE(IPT,86) ITADJ
       WRITE (IPT,45)
       WRITE(ISCN,46)
   46  FORMAT(/' PASS SAT LOKR(MIN) AZIM DIR  MAXEL   FREQ',
     + 2X,21H TO(USC)  SIG GTP BTP   /)
C      WRITE (IOT   ) HEDR
      WRITE(IOT) NAME,IORD,DAYT,NSTN,ISRC
       TMME = DABS(TMME - 1.D0)
C
C        READ NEXT PASS & ACT ON STATUS FLAG-
C
C  ASSUME STANDARD ATMOSPHERE
      PREST= 1014.D0/DEXP(PSTN(7)*.1207D-03)
      PRES= PREST
      TDRY= 15.D0
C                DEGREES C
      TWET= 10.D0
C                MILLIBARS  P.V.P.
20    CONTINUE
C     READ SPECIAL FORMAT OF JMR DATA
      SIC= 0.D0
      CLK=0.D0
      IF(IRCV.EQ.1) GO TO 520
      IF(IRCV.EQ.2) CALL CMA751(ISAT,TDRY, PRES, TWET, CLK,ISTATS,
     1 2,NTIM,ISTO)
      IF(IRCV.EQ.0.OR.IRCV.EQ.3) CALL CMA722(IDAT,ISAT,ISHF,ISTO,NINE)
      IF(IRCV.EQ.4)CALL CMAMMM(IDAT,ISAT,ISHF,ISTO,NINE)
      IF(IRCV.EQ.5.OR.IRCV.EQ.7)CALL MX1502(ISAT, IST,ISTATS,JAHR,NSER
     @,NTIM,MTIM,CLK,IRCV)
      IF(IRCV.EQ.6) CALL CMA751(ISAT,TDRY, PRES, TWET, CLK,ISTATS,
     1 1,NTIM,ISTO)
      GO TO 521
520   CALL JMRINP(ISAT, ISTATS,TMME,TDRY,PRES,TWET,SIC,NTIM, CLK)
C   RECONSTRUCT JMR TIMMING  ( IF POSSIBLE AT ALL )
      CLKJMR= CLKJMR+ CLK
      CLK= CLKJMR
      IF((PRES+TDRY+TWET).NE.0.D0) GO TO 106
      PRES= PREST
      TDRY= 15.D0
      TWET= 10.D0
      GO TO 107
106    CONTINUE
      IF(TDRY.GT.100.D0)TDRY= -(TDRY-100.D0)
      TWET= VPSAT(TDRY)* TWET* .01D0
107    CONTINUE
      IF(TMME.EQ.1.D0) GO TO 523
C  SATELLITE TIMING  TMME= 0.
      DTT=(DTIM(1)+ DTIM(4)- DTIM(2) )
      DO 522 I=1,32,2
      TIM(I)= DTIM(2)
      K= I +1
      TIM(K)= DTIM(1)
      ITWO=( K/4)* 4- K
      IF(ITWO.EQ.0)TIM(K)=DTT
 522   CONTINUE
523    CONTINUE
      K=1
      DO 524 J=1,8
      DO 524 I=1,4
      DNOM( I,J)= TIM(K)
524    K= K+1
521   CONTINUE
      TIM(63)= CLK
C--- CHECK TEST STATION NAME IF MX-1502 DATA.
      IF(IRCV.NE.5.AND.IRCV.NE.7) GO TO 526
      IF(ITSN.EQ.IST) GO TO 526
      IF(ISTO.EQ.IBLNK) GO TO 526
      IF(IST.EQ.0) GO TO 526
      IF(ISTA.EQ.3) ISTA=2
      IF(ISTA.EQ.4) ISTA=1
      WRITE(IPT,903) IST,ITSN
  526 CONTINUE
C
C   ACT ON STATUS FLAG (ISTA).
C
C     1 - END OF FILE ON UNIT #INP.
C     2 - PASS REJECTED, READ NEXT PASS.
C     3 - PASS ACCEPTED, DECODE, STORE, AND READ NEXT PASS.
C     4 - PASS ACCEPTED WITH END OF FILE ON UNIT #INP.
C
       GO TO (90,20,30,30) , ISTA
C
C        DECODE PARAMETERS & GET CORRECTED DOPPLERS-
C
   30 CALL DECOD1(ISAT)
      IF(ISTB.EQ.2)GOTO20
C
C        COMPUT SAT. POS. / TEST ELEVATIONS / TEST CLOSURES-
C
      CALL SATCRD(CXYZ,XBAS,CCC,SCC,CPP,STNC,IORD,DLT,LENG,
     1 C, TT, Q ,QL, AE, RMU, NMAX, PW,NTIM)
      IF(ISTB.EQ.2)GOTO20
      CALL SCREEN(SIGC,CXYZ,SIGF,STNC,PHI,XLM,IDOP,TBAS,IORD)
      IF(ISTB.EQ.2)GOTO20
C
C        PASS OK --- GET METS & OUTPUT DATA-
C
      IF (METL.GT.1) CALL METDAT (PRES , TDRY , TWET)
       ANGL = PI / 2.D0 -DATAN(CORI / SORI)
       INJC = IDINT(TNJC)
      ALOK=DBLE(LOKR) + SIC/.60000D 08- TIM(63)
      LOKR= LOKR- 6
       XT   = TPER + 8.D0
       APTP = APTP - DATP * XT
       RAAN = RAAN + DRAN * XT
           DO 400 I = 1,NDOP
               DOP(I) = DOP(I) * DBLE(KDOP(I))
  400      CONTINUE
      IF(NPAS.NE.0) GO TO 95
      WRITE(ICF)  NSTN, LOKR,LOKR,NAME,IDAYT, ISRC, IORD
      FIRST= LOKR + 12
95      CONTINUE
C--- SET NUMBER OF TIME INTERVALS IF NOT DONE DURING INPUT.
      IF(IRCV.EQ.1) NTIM= NDOP
      IF(IRCV.EQ.2)NTIM=NDOP
      IF(IRCV.EQ.5.OR.IRCV.EQ.7) NTIM= NDOP
      IF(IRCV.EQ.6) NTIM= NDOP
      IF(( LOKR- LOKPR ).EQ.0) GO TO 525
C TIME ADJUSTMENT ANALYSIS
C     ITPRJW=6H
      ITPRJW=IBLNK
C--- "." ON "1.D0" MISSING IN CDC VERSION.
      IF(TIM(65).LT.1.D0) GO TO 96
      P=    TIM(65)/ (7.D0 +.25D0*TIM(65) )
      DT= (LOKR + 12-FIRST)/1440.D0
C  PROCCESS SAT. NO.
       DO 19 ISST=1,17
       IF(ISAT.EQ.KSAT(ISST)) GO TO 9
19     CONTINUE
 9     CONTINUE
      DTT=( TIM(67)+TIM(63)) *6.D04-TO-TDT*DT  +SATER(ISST)
      STEST= 10.D0*SO
      IF(NTPAC.LE.2) GO TO 196
      IF(DABS(TO).LE.1D-6) GO TO 196
      IF(DABS(DTT).LE.STEST) GO TO 196
C   RESTART LINE FIT
      FIRST= LOKR +12
      DT= 0.D0
      TO= 0.D0
      RN(1)= 1.D 10
      RN(2)= 0.D0
      NTPAC= NTPAC-1
C     ITPRJW=6HNEWFIT
      ITPRJW=INF
196   DF=0.D0
C--- FOLLOWING CDC STATEMENT NOT NEEDED.  "REJT" IS SET =0 IN "FADJST".
C     REJT=0.
C  AUGMENT SIGMA AND START NEW LINE SEGMENT IF DT.GT.ITADJ DAYS
      IF(ITADJ.EQ.0) GO TO 98
      IF(DT.LE.DBLE(ITADJ )   ) GO TO 98
      RN(1)= RN(1) +4.D-02 +2.D0*RN(2)*DT+ RN(3)*DT**2
      RN(2)= RN(2) + RN(3)*DT
      RN(3)= RN(3) +4.D-02
      FIRST= DBLE(LOKR +12)
      TO= TO+ TDT*DT
C--- ("." IS MISSING IN CDC VERSION OF FOLLOWING STATEMENT.)
      DT=0.D0
C       ITPRJW=6HNEWSEG
      ITPRJW=INS
98    CONTINUE
      CALL FADJST(DF,TDT,DTT,RN,DT,VPV,REJT,SO,P)
      IF(REJT.NE.0.D0) GO TO 96
      TO= TO+ DF*60.D0
      NTPAC= NTPAC +1
C     SATELLITE TIME ADJUSTMENT
      RNP = SATER(ISST+34)
      RNP1= SATER(ISST+34)+1.D0
      SATER(ISST)=  SATER(ISST)    -DTT /RNP1
C  MEAN ERROR(MSEC)
C SIGMA(MSEC)
      SATER(ISST+17)=( SATER(ISST+17)* RNP     +DTT**2)/RNP1
      SATER(ISST+34)= SATER(ISST+34) +1.D0
C   CORRECT ALOK FOR RESI TIME DELAY DTT IF SPECIFIED SO
      DTT= DTT*(1.D0-1.D0/RNP1)
      IF(ITADJ.GT.0.AND.ITADJ.LT.10)ALOK= ALOK- DTT/6.000D 04
C   NO TIME CORRECTION IF ITADJ =10
      IF(ITADJ.EQ.10)ALOK= DBLE(LOKR)+SIC/.60000D 08
C   CESIUM CLOCK ASSUMED IF ITADJ.GT.10
      IF(ITADJ.GT.10)ALOK= DBLE(LOKR)+SIC/.60000D 08   +TIM(67)
C  REJECTION
  96   ITPRJ= ITPRJ+IDINT(REJT+.05D0)
C     IF(REJT.EQ.1.)ITPRJW= 6HT-RJCT
      IF(REJT.EQ.1.D0)ITPRJW= ITREJ
      TPER=TPER-6.D0
       WRITE (ICF) LOKR , LENG , ((CXYZ(J,I),J=1,IORD),I=1,3)     , ISAT
     +           , FSAT , TPER , DMAN , ECCT , APTP , RAAN , RAST , ANGL
     +           , NOTE(32)
       WRITE (IOT) NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT , NSTN
     +           , TPER , DMAN , ECCT , APTP , RAAN , RAST , ANGL
     +       , NOTE(32) , ((CXYZ(J,I),J=1,IORD),I=1,3) , NDOP
       WRITE (IOT) ISTN , ALOK , PRES , TDRY , TWET , NDIR , FRCV
     +           , NTIM , (DOP(I),I=1,NDOP) , (TIM(I),I=1,NTIM)
       WRITE (ISC) ISAT , LOKR , FRCV
       NPAS = NPAS + 1
C        CONTINUE PROCESSING IF MORE DATA-
C
      NCLK= IDINT((TIM(63)+TIM(67))*6.D 07)
      NSGM= IDINT(TIM( 64)*6.D 07)
      ITM= IDINT( TIM( 65) +.05D0)
      ITMRJ= IDINT((TIM( 66)+ .05D0))
      WRITE(IPT,65) NPAS, AZIM, IDIR(NDIR), ELMX, FRCV, NCLK, NSGM,
     1 ITM, ITMRJ,ITPRJW, (KDOP(I), I=1,32)
      WRITE(ISCN,66) NPAS,ISAT,LOKR,AZIM,IDIR(NDIR),ELMX,FRCV,NCLK,NSGM,
     1 ITM, ITMRJ,ITPRJW, (KDOP(I), I=1,32)
525      LOKPR= LOKR
       IF (ISTA.NE.4) GO TO 20
   90  ENDFILE IOT
       ENDFILE ICF
       ENDFILE ISC
C
      SOO=SO
       IF(NTPAC.GT.2) SOO=DSQRT(VPV/DBLE(NTPAC-2) )
      SGMTO=DSQRT(RN(1))*SOO
      SGMTDT=DSQRT(RN(3))*SOO
      RHO= RN(2)/SGMTO/SGMTDT*SOO*SOO
C   FREQUENCY
      DF= TDT*4.6296D0
      SGMDF= SGMTDT*4.6296D0
      WRITE(IPT,97) NTPAC,ITPRJ,TO,SGMTO,TDT,SGMTDT,RHO,DF,SGMDF,SOO
97    FORMAT(/,26H   TIME ADJUSTMENT SUMMARY   /
     &10X,'NO. OF ACCEPTED PASSES =',I5/
     &10X,'NO. OF REJECTED PASSES =',I5//
     115X,'TO    = ',F5.3,'  STD = ',F5.3,6H  MSEC/
     115X,'DRIFT = ',F8.6,'  STD = ',F8.6,' MSEC/DAY',5X, 'CORR. =',
     &F8.5/15X,'FREQ  = ',F8.6,'  STD = ',F8.6,' HZ/400MHZ',4X,'SO    ='
     &,F8.4,' MSEC' )
      DO 101 I=1,17
      SATER(I+34)= SATER(I+34)-2.D0
      IF(SATER(I+34).GT.1.D0)SATER(I+17)= SATER(I+17)/(SATER(I+34)-1.D0)
101   SATER(I+17)=DSQRT(SATER(I+17) )
      WRITE(IPT,99)(SATER(I+51),SATER(I),SATER(I+17),SATER(I+34),I=1,17)
99    FORMAT(/'  SATELLITE CLOCK ERRORS- '/5X,
     1'SAT.   ERR(MS)  SIGMA(MS)   NO.OF PASS' /(F10.0,2F10.3,F10.0))
C        PLOT RECEIVER FREQUENCY OFFSET-
C
       REWIND ISC
       XT = FRCV - 6.D0
       YT = FRCV + 6.D0
       ISTNN= ISTN
       CALL PRINTP (1 , 1 , XT , YT , FRCV , ISTNN , 1 , 1,IPT)
      KPAS= NPAS
      IF(NPAS.GT.50) KPAS= 50
      DO 500 I=1,KPAS
               READ (ISC) ISAT , LOKR , FRCV
               CALL PRINTP (2 , 2 , XT , YT , FRCV , 2 , 2 , 2,IPT)
  500      CONTINUE
       CALL PRINTP (3 , 3 , XT , YT , FRCV , 3 , 3 , 3, IPT)
       REWIND ISC
C
       GO TO 1
C
5     FORMAT(2A10,2(2F4.0,F7.3),3X,F7.1,3F5.1,I5/,I1,I2,F2.0,F5.1,4X,A1,
     +2(1X,F4.1),F5.0,2X,F3.0,4X,I1,2(1X,F4.1),2(4X,I1),A4,9X,I2,I3,F2.
     +0)
   15  FORMAT (/,50X,'******* P R E D O P *******',/,'OPTION CARDS:',//
     +2A10,2(2X,2F4.0,F7.3),F9.3,3F4.1,I4,I3,I3,F5.0,F4.1,4X,A1,
     &/2(1X,F4.1),F5.0,2X,F3.0,3X,I2,2(1X,F6.1),2(3X,I2),A4,6X,I5,I3,
     &F2.0,//,5X,'******* M E T  D A T A *******',//' HH MM',2X,'DAY', 
     &'  PRESS  TEMP  TWET  RH')
25    FORMAT(I3,1H:I2,I5,F7.0,F6.1,F6.1,I4)
35    FORMAT(I3,1H:I2,I5,F7.0,F6.1,F6.1,I4)
   45  FORMAT(/' CARD  STATION  SAT DDD HH MM   PASS  AZIM DIR  MAXEL   
     & FREQ',
     + 2X,21H TO(USC)  SIG GTP BTP   ,19X,'USED DOPPLERS')
65      FORMAT(35X,I3,F5.0,2X,A2,2X,F4.1,2X,F6.1,I10,I5,2I3,2X,A6,
     @ 8(1X,4I1)
     &/'----------------------------------------------------------------
     &------------------------')
66    FORMAT(1X,2I4,I10,F5.0,2X,A2,2X,F4.1,2X,F6.1,I10,I5,2I3,2X,A6,/,
     @ 39X, 8(1X,4I1))
   75 FORMAT(/42X,'***** OPTIONS USED IN THIS RUN ',A10,' *****',//
     +5X,'FIGURE NAME = ',A10,6X,'STATION NAME = ',A10,6X,
     &'TEST STATION NAME = ',A4,/
     &5X,'STATION COORDINATES      = ',2F5.0,F8.3,' (LAT)',3X,2F5.0,
     &F8.3,' (LONG)' ,12X,F7.2,' (HT)'/
     &40X,F11.2,4H (X),11X,F11.2,4H (Y),10X,F11.2,4H (Z),//
     &5X,'DATUM SHIFT              = ',F11.1,4H (X),F12.1,4H (Y),
     &F12.1,4H (Z), /
     &5X,'ELLIPSOID PARAMETERS     = ',F11.1,5H (A0),
     &F11.2,5H (B0),F13.11,9H (E-SQRD)/
     &5X,'RECEIVER TYPE            = '
     &,I11,'  (0=MARCONI, 1=JMR ,2=CMA751(FMT-3), 3=CM9TMJV, 4=3M CM, 5=
     &MX1502, 6=CMA751(FMT-1)'/46X,'7=MX1502 (EBCDIC HEX))', /
     &5X,'FREQUENCY OFFSET         = ',F5.1,/
     &5X,'DOPPLER TIMING           = ',F4.0,' (0=INTERNAL, 1=SATELLITE)
     &',/
     &5X,'DOPPLER CUTOFF ELEVATION = ',F5.1)
   85  FORMAT(/5X,
     &   'DOPPLER SYMMETRY FLAG    = ',I4,' (0=CPA TRIM, 1=NO TRIM)',/
     &5X,'PASS CUTOFF ELEVATION    = ',F5.1,/
     &5X,'SIGMA (MISCLOSURES)      = ',F5.0,/
     &5X,'SIGMA (FREQUENCY OFFSET) = ',F4.0,/
     &5X,'RAW DATA FORMAT (PARMS)  = ',I4,' (1=14 V.PARMS, 2=12 V.PARMS,
     & # OF FIRST V. PARM. FOR JMR)'/
     &5X,'                (SHIFT)  = ',I4,' (1=NO BLANK, 2=ONE BLANK)'
     &/5X,'                (DIGITS) = ',I4,/
     &5X,'PRESSURE FORMAT          = '2X,A2,' (F=FEET-1000, M=MILLIBARS)
     &'/5X,'GRAVITY FIELD TRUNCATION = ',I4,'  PW =',F5.1,1X,' (G.F. TR 
     &= -1 BROADCAST ORBITS (WELLS SMOOTHING))')
  901 FORMAT(1X,'*** EXECUTION TERMINATING *** RECEIVER TYPE OUT OF'
     @,' RANGE:',I12)
  902 FORMAT(I4)
  903 FORMAT(32X,'WRONG STATION NUMBER -- PASS IGNORED. DATA='
     @,I5,'  COORD. CARD=',I5/)
C
86       FORMAT(/
     &5X,'TIME ADJUSTMENT (ITADJ)  = ',I4,' (=0 (DEFAULT) - PASS CORREC
     &TION, = 1-9  L.FIT CORR, = 10  NO TIME CORRECTION)')
       END
C----------------------------------------------------------------------- 
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
C         EXTERNAL ROUTINES  -DBLE   ,        ,        ,
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

