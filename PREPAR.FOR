Cv3.31  INTERFACE TO SUBROUTINE TIME (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
Cv3.31  INTERFACE TO SUBROUTINE DATE (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]	
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
      PROGRAM PREPAR
      IMPLICIT REAL*8 (A-H,O-Z)
        CHARACTER*4 ISRC, IBLNK, IEPN, TYPE
        CHARACTER*10 NAME , ISTN, DAYT, TIMME
C FILE NAME DEFFAULTS
      CHARACTER*40 FNAME,FBLNK, PRMJV4 ,PREPH4
      CHARACTER*10 PREEPH,PREMJV,GEOINP,PREOUT
      INTEGER DAIT
C
C   VERSION: 03/24/83                GEODOP 5.03   (NOV81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C        "PREPAR    " - AUTHOR        - P.G. LAWNIKANIS
C                     - COAUTHOR      - J. KOUBA
C                     - WRITTEN ON    - DECEMBER /74.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - *NONE*
C                     - IBM PC version- February 1988 BY J.K. 
C         EXTERNAL ROUTINES  -TIMII   ,RMERGE  ,TIMIP   ,
C                             DATE    ,ERRSET  ,EOF     ,EXIT
C
C        [PREPAR[ READS AND MERGES GEOCEIVER OR TRANET DOPPLER
C                 DATA WITH NWL-FITTED (PRECISE) OR PREDOP EPHEMERIS.
C
       COMMON NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT , NSTN
     +      , IFIT , ORBP , COEF , NDOP , ISTN , PRES , TDRY , VPAR
     +      , DOPP , TMME , IFLG , NSAT , NORD , ISWT , LOKG , KDOP
     +      , NPSI , NPSA , NDPI , NDPA , IRCV , INTA
       COMMON /DEV/ IEPH , INPT , IOUT , IRED , IPRT
       COMMON /PAR/ DAYP , TPER , XMOT , APTP , DATP , ECCT , RAAN
     +      , DRAN , RAGR , ANGL , JSAT , IEPO , PERD
       COMMON /TIM/     A ,    B , ALOK , DTAU , EXPT , GXYZ , IORD
     +      , MESS , SIGS , TTAU , VPVP , FREQ , TAUT , FIRT , FIRT2
      COMMON/DAT/  IFRQ, MAXD
C--- UNIT NUMBER FOR IBM DECODE OPERATION.
       DIMENSION ORBP( 7) , COEF(30) , DOPP(50) , TMME(50) , KDOP(50)
     +         , INTA(30)
       DIMENSION GXYZ( 3) , EXPT(99) ,  A(3,2) , B(3,3,3) , MESS( 3)
     +         , TAUT(10)
      DIMENSION IEPN(2),TYPE(3,5), INTB(2,10)
      INTEGER*2 ITIMME(3), IDATE(3)
C
      DATA TYPE/'  GE','OCEI','VER ',
     1          '  TR','ANET',' I  ',
     3          ' Cs ','TRAN','ET I', 
     4          '  TR','ANET',' II ', 
     5          'Cs T','RANE','T II' /
       DATA IEPN/'SARC','PREC'/,PITW/6.28318531D0/,RADN/.0174532925D0/
     +    ,INTB / 13,59,  20,77,  11,93,  30,119,  48,105,  50,115,
     +            12,58,  14,60,  19,68,  75,75/       ,IBLNK/'    '/
C
C  DEFAULT FILE NAMES
      DATA GRVINP/'NUL.GRV   '/, PREEPH/'EPHM.BIN  '/,
     + PREOUT/'PREPAR.TXT'/,PREMJV/'PREPAR.MJV'/,GEOINP/'GEODOP.BIN'/,
     + FBLNK/'                                        '/ 
C
C--- I/O UNITS.
      ISCN= 0
      IEPH=3
      INPT=4
      IOUT=13
      IRED=0
      IPRT=6
C      IBM PC VERSION
C     LUN   FORMAT   TYPE     DEFAULT NAME   DESCRIPTION         VARIABLE
C     ---   ------   ----     ------------   ------------------  -------- 
C     INPT  ASCII    INPUT    PREPAR.MJV     DOPPLER DATA        PREMJV
C     IEPH  BINARY   INPUT    EPHM.BIN       FIT. EPHEM. FILE    PREEPH
C     IPRT  ASCII    OUTPUT   PREPAR.TXT     PRINTOUT            PREOUT
C     IOUT  BINARY   OUTPUT   GEODOP.BIN     GEODOP INPUT FILE   GEOINP
C     ISCN ASCII     I/O      SCRN:(ISCN=0)  SCREEN OUTPUT       N/A
C
C-----------------------------------------------------------------
C
C  SYSTEM DEPENDENT CALL FOR TIME AND DATE, CHANGE IF REQUIRED SO
Cv3.31        CALL DATE(10,DAYT)
Cv3.31        CALL TIME(10,TIMME)
C VERSION 4.1 ONLY
      CALL GETTIM(ITIMME(1),ITIMME(2),ITIMME(3),ITSEC)
      CALL GETDAT(IDATE(3),IDATE(2),IDATE(1))
      WRITE(TIMME,'(1X,2(I2,'':''),I2)') ITIMME
      WRITE(DAYT,'(2(I2,''/''),I4)') IDATE
      WRITE(ISCN,1234) DAYT,TIMME
1234  FORMAT(' Program  P R E P A R - Geodopv Preprocessor (June 87)',3X
     +,2A10,/ ' for TRANET and GEOCEIVER data files'/)
C
      WRITE(ISCN,'(A\)') ' Doppler data input(ascii)[PREPAR.MJV]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=12) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= PREMJV
      PRMJV4=FNAME
      WRITE(ISCN,'(1X,(A))') FNAME
         OPEN(INPT,FILE=FNAME,STATUS='OLD',ERR=11,IOSTAT=IERROR)
C
      WRITE(ISCN,'(A\)') ' Fitted Ephem input(bin.) [EPHM.BIN  ]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=12) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= PREEPH
      PREPH4=FNAME
      WRITE(ISCN,'(1X,(A))') FNAME
         OPEN(IEPH,FILE=FNAME,STATUS='OLD',FORM='BINARY',ERR=11,
     +   IOSTAT=IERROR)
C
      WRITE(ISCN,'(A\)') ' PREPAR output (text) file[PREPAR.TXT]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=12) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= PREOUT
      WRITE(ISCN,'(1X,(A))') FNAME
         OPEN(IPRT,FILE=FNAME,STATUS='UNKNOWN',ERR=11,IOSTAT=IERROR)
C
      WRITE(IPRT,1234) DAYT,TIMME
      WRITE(IPRT,'(A\)') ' Doppler data input(ascii)[PREPAR.MJV]: ' 
      WRITE(IPRT,'(1X,(A))') PRMJV4
      WRITE(IPRT,'(A\)') ' Fitted Ephem input(bin.) [EPHM.BIN  ]: ' 
      WRITE(IPRT,'(1X,(A))') PREPH4
      WRITE(IPRT,'(A\)') ' PREPAR output (text) file[PREPAR.TXT]: ' 
      WRITE(IPRT,'(1X,(A))') FNAME
      WRITE(ISCN,'(A\)') ' PREPAR output file (bin) [GEODOP.BIN]: ' 
      WRITE(IPRT,'(A\)') ' PREPAR output file (bin) [GEODOP.BIN]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=12) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= GEOINP
      WRITE(ISCN,'(1X,(A))') FNAME
      WRITE(IPRT,'(1X,(A))') FNAME
         OPEN(IOUT,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY',ERR=11,
     + IOSTAT=IERROR)
C
C--- INITIALIZATION OF VARIABLES.
C
       DO 1 I=1,7
1      ORBP(I)= 0.D0
      INJC=0
      IFIT= 0
      PERD= 104.D0
      TPER= 0.D0
C     DATA  GXYZ,EXPT,A,B,TAUT/145*0.0/
C     DATA  MESS/3*1H /
C     DATA ALOK,DTAU,SIGS,TTAU,VPVP,FREQ/6*0.0/
      DO 2 I=1,3
    2 GXYZ(I)=0.D0
      DO 3 I=1,99
    3 EXPT(I)=0.D0
      DO 4 I=1,3
      DO 4 J=1,2
    4 A(I,J)=0.D0
      DO 6 I=1,3
      DO 6 J=1,3
      DO 6 K=1,3
    6 B(I,J,K)=0.D0
      DO 7 I=1,3
    7 MESS(I)=IBLNK
      ALOK=0.D0
      DTAU=0.D0
      SIGS=0.D0
      TTAU=0.D0
      VPVP=0.D0
      FREQ=0.D0
           DO 100 I = 1,20,2
               II=I/2 +1
               INTA(I)   = INTB(1,II)
               INTA(I+1) = INTB(2,II)
  100      CONTINUE
      DO  110 I=21,30
  110 INTA(I)=0
      INTA(21)= 117
      INTA(22)= 117
      IEOF= 0
      INTA(26)=75
   10 CONTINUE
      IF(IEOF.NE.0) GO TO 16
C
       WRITE(ISCN,'(A\)') ' Rec:0-GEOC;1-TR1;2-TR1Cs;3-TR2;4-TR2Cs :'
       READ(ISCN,*) IRCV
       WRITE(ISCN,'(1X,I6)') IRCV
       WRITE(ISCN,'(A\)') ' Maximum number of Dopplers        [32] :'
       READ(ISCN,*) MAXD
       WRITE(ISCN,'(1X,I6)') MAXD
       WRITE(ISCN,'(A\)') ' Satellite (APL) number (0-all)     [0] :'
       READ(ISCN,*) KSAT
       WRITE(ISCN,'(1X,I6)') KSAT
       WRITE(ISCN,'(A\)') ' Station name (A10)             [blank] :'
       READ(ISCN,'(A)') ISTN
       WRITE(ISCN,'(1X,A10)') ISTN
C
       WRITE(ISCN,'(A\)') ' Station coordinates (X,Y,Z in metres)  :'
       READ(ISCN,*) (GXYZ(I),I=1,3)
       WRITE(ISCN,'(1X,3F12.2)') (GXYZ(I),I=1,3)
       WRITE(ISCN,'(A\)') ' Clock error polynom. #1 EPOCH(day) [0] :'
       READ(ISCN,*) EXPT(1)
       WRITE(ISCN,'(1X,F6.0)') EXPT(1)
       IF(EXPT(1).LE.0.D0) GO TO 24
       WRITE(ISCN,'(A\)') ' Err(ms) Drft(ms/d) Rate(ms/d**2)[0 0 0]:'
       READ(ISCN,*) (EXPT(I),I=2,4)
       WRITE(ISCN,'(1X,3F6.1)') (EXPT(I),I=2,4)
       WRITE(ISCN,'(A\)') ' Clock error polynom. #2 EPOCH(day)  [0]:'
       READ(ISCN,*) EXPT(5)
       WRITE(ISCN,'(1X,F6.0)') EXPT(5)
       IF(EXPT(5).LE.0.D0) GO TO 24
       WRITE(ISCN,'(A\)') ' Err(ms) Drft(ms/d) Rate(ms/d**2)[0 0 0]:'
       READ(ISCN,*) (EXPT(I),I=6,8)
       WRITE(ISCN,'(1X,3F6.1)') (EXPT(I),I=6,8)
C   IF IRCV.EQ.2(I.E. INTERNALY 3),TRANET(CESIUM) TIME HELD FIXED
24     IEOF=0
      GO TO 18
C--- IF EOF REACHED, STOP IBM VERSION.
 11   WRITE(ISCN,1122) FNAME, IERROR
 1122 FORMAT(' OPEN FILE ERROR FNAME: ',A20,' IOSTAT= ',I5)
      GO TO 16
   13 WRITE(ISCN,'(A)')' Format read error in PREDOP input file:' 
C  TERMINATE PROGRAM
   16 IEOF=1
      CLOSE(INPT) 
      CLOSE(IOUT) 
      CLOSE(IPRT) 
      CLOSE(IEPH) 
      STOP
   18 CONTINUE
C--- CHECK FOR VALID RECEIVER TYPE.
      IF(IRCV.GE.0.AND.IRCV.LE.4) GO TO 20
      WRITE(IPRT,901) IRCV
      STOP
   20 CONTINUE
      IF(KSAT.GT.50) THEN
       DO 224 I=1,10
       IF(KSAT.EQ.INTB(2,I)) THEN
        KSAT= INTB(1,I)
        GOTO 26
       ENDIF 
 224    CONTINUE     
 26    CONTINUE
      ENDIF
      IPRN= 1
      IF(MAXD.EQ.0) MAXD= 32
       READ(IEPH,END=16) NSTN,NSAT,LOKR,NAME,DAIT,ISRC,IORD
C             CORRECT SAT NO
      IF(NSAT.EQ.5.OR.NSAT.EQ.15) NSAT= NSAT+ 100
      IF(IRCV.LT.1.OR.IRCV.GT.2) GO TO 14
       A1= 6.378138D 06
      ESQ=  6.6943D-03
      CALL CARGEO(GXYZ(1),GXYZ(2),GXYZ(3),ALAT,ALOG,XH,RN,A1,ESQ)
C    SET STANDARD WEATHER PARAMETERS
      TDRY=15.D0
      VPAR=58.5D0
      PRES=1014.D0/DEXP(XH*.1207D-03)
14    CONTINUE
       IEPO = 1
       NSTN = 1
       IFLG = IFLG + 1
       IRCV = IRCV + 1
       NORD = IORD * 3
      IF(ISRC.EQ.IEPN(2)) DAIT=IBLNK
       WRITE (IPRT,15) DAYT,(TYPE(I,IRCV),I=1,3),DAYT,NAME,NSTN,DAIT
     +               , IORD , ISRC        , ISTN , GXYZ
C--- ARRAY ELEMENT OF IEPN WASN'T SPECIFIED IN CDC VERSION OF NEXT
C     STATEMENT.
       IEPO=3
       IF (ISRC.EQ.IEPN(1)) THEN
        IEPO = 1
        GO TO 12
       ENDIF
C
       IF(ISRC.EQ.IEPN(2)) IEPO = 2
      IF(NSAT.GT.50) THEN
       DO 22 I=1,10
       IF(NSAT.EQ.INTB(2,I)) THEN
        NSAT= INTB(1,I)
        GOTO 23
       ENDIF 
 22    CONTINUE     
 23    CONTINUE
      ENDIF
      IF((KSAT*NSAT).NE.0.AND.KSAT.NE.NSAT) STOP
C
   12  WRITE (IOUT)    NAME , IORD , DAYT , NSTN , ISRC
       LOKG = 0
       NPSI = 0
       NPSA = 0
       NDPI = 0
       NDPA = 0
C--- USE EPHEMERIS SATELLITE NUMBER (NOT INPUT NUMBER) FOR GEOCEIVER.
C      NSAT = KSAT
       IF(IRCV.NE.1.AND.KSAT.NE.0) NSAT=KSAT
       CALL TIMII
      CALL RMERGE( IPRN,FIRT,FIRT2)
       WRITE (IPRT,25) NPSI , NPSA , NDPI , NDPA
       CALL TIMIP
       GO TO 16
C
5     FORMAT(I2,I3,3(3X,I2),A10,3F10.1,4F5.1/4F5.1)
   15 FORMAT(1H1/ 5X,A10,20X,25H** PREPARE GEODOP INPUT (,3A4,4H) **
     +  ,20X,A10,//,24H   SOURCE EPHEMERIS ... ,2(A10,I3)
     +             ,10H ....... (,A10,1H)
     +          ,//,13H   STATION - ,A10
     +          ,//,15H   COORDINATES ,3F15.1)
   25  FORMAT (////,21H   # PASSES READ   = ,I4
     +           ,/,21H   # PASSES USED   = ,I4
     +           ,/,21H   # DOPPLERS READ = ,I4
     +           ,/,21H   # DOPPLERS USED = ,I4
     +)
   35  FORMAT(F3.0,F9.4,F9.7,F8.4,F6.0,F7.6,F8.4,F5.0,F8.4,F8.5,F6.0,I3)
   45  FORMAT (   /,24H   KEPLERIAN PARAMETERS
     + ,F5.0,F11.4,F11.7,F10.4,F8.0,F9.6,F10.4,F7.0,F10.4,F10.5,F8.0,I5)
  901 FORMAT(1X,50H*** EXECUTION TERMINATING *** RECEIVER TYPE OUT OF
     @,7H RANGE:,I12)
C
       END
      SUBROUTINE CARGEO (X,Y,Z,ALAT,ALOG,H,RAN,A,ESQ)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
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
      SUBROUTINE INITIA (XC,YC,ZC,XDD,YDD,ZDD,ZMUE,E,V,CLEINO,ZIEIN
     1 , OMEIN, AEIN, ECA)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C     RADIUS  VECTOR
      R=DSQRT(XC*XC+YC*YC+ZC*ZC)
C     R DOT
      RD=(XC*XDD+YC*YDD+ZC*ZDD)/R
      D1=YC*ZDD-ZC*YDD
      D2=XC*ZDD-ZC*XDD
C     LONGITUDE OF NODE
      OMG=DATAN2(D1,D2)
      OMEIN=OMG
      C1=DSQRT(D1*D1+D2*D2)
      C2=XC*YDD-XDD*YC
C     INCLINATION
      CLI=DATAN2(C1,C2)
      ZIEIN=CLI
      V2=XDD*XDD+YDD*YDD+ZDD*ZDD
C     SEMI-MAJOR AXIS
      SM=(ZMUE*R)/(2*ZMUE-R*V2)
      AEIN=SM
      H2=(C1*C1+C2*C2)
C     ECCENTRICITY
      E = DSQRT(1.D0-H2/(ZMUE*SM))
      E1=R*RD*DSQRT(SM)
      E2=(SM-R)*DSQRT(ZMUE)
C     ECCENTRIC ANOMALY
      ECA=DATAN2(E1,E2)
      F1=DSIN(ECA)*DSQRT(1-E*E)
      F2 = DCOS(ECA) - E
C     TRUE ANOMALY
      V=DATAN2(F1,F2)
      O1=-XC*DCOS(CLI)*DSIN(OMG)+YC*DCOS(CLI)*DCOS(OMG)+ZC*DSIN(CLI)
      O2=XC*DCOS(OMG)+YC*DSIN(OMG)
      OF=DATAN2(O1,O2)
C     ARGUMENT OF PERIGEE
      CLEINO= OF - V
      RETURN
      END

