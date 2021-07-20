C PREDOP4
C output modified by Bolzan - 4/3/90
C-----------------------------------------------------------------------
       SUBROUTINE PRINTP (MODE,LABEL,XLOWR,XHIGH,VALUE,NAME1,NAME2,LINE,
     + IPT)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*4 HEAD, BLANK, MASTER, MX
      CHARACTER*1 CHAR
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
C         EXTERNAL ROUTINES  -ABS     ,DBLE   ,INT     ,
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
C
C      DIMENSION CHAR(121),HEAD(5),IAXIS(13)
      DIMENSION CHAR(121), HEAD(3,5), IAXIS(13)
C      DATA HEAD/10H*FREQ.OFF* , 10H ** DX **  , 10H ** DY **  ,
C    =           10H ** DZ **  , 10H ** DR **  /
      DATA HEAD/'*FRE','Q.OF','F*   ',' ** ','DX *','*   '
     1,          ' ** ','DY *','*   ',' ** ','DZ *','*   '
     2,          ' ** ','DR *','*   '  /
       DATA BLANK/'   '/
       DATA CHAR/121*' '/
       DATA IPLOT,JT/0,1/
      DATA MASTER/ '*   '/,MX/'X   '/
       GO TO (10,20,30),MODE
C
C   START NEW PLOT .....
C
   10  IPLOT = IPLOT + 1
       WRITE(IPT,5) IPLOT,(HEAD(I,LABEL),I=1,3)
       IF (LABEL.EQ.1) WRITE(IPT,15) NAME1
       IF (LABEL.GT.1.AND.NAME2.EQ.BLANK) WRITE(IPT,15) NAME1
       IF (LABEL.GT.1.AND.NAME2.NE.BLANK) WRITE(IPT,25)NAME1,NAME2
       WRITE(IPT,35) XLOWR,XHIGH
       XT = (XHIGH - XLOWR) / 12.D0
       SCALE = 10.D0/XT
       DO 100 I=1,13
               YT =DABS (XLOWR + XT *DBLE (I - 1))
               IAXIS(I) =IDINT(YT -DBLE ((IDINT (YT) / 1000) * 1000))
  100  CONTINUE
       WRITE(IPT,45) (IAXIS(I),I=1,13)
       WRITE(IPT,55)
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
       IF (IT.EQ.0) WRITE(IPT,65) IL,(CHAR(I),I=1,121),IL
       IF (IT.GT.0) WRITE(IPT,75) (CHAR(I),I=1,121)
       RETURN
C
C   END PLOT .....
C
   30  WRITE(IPT,55)
       WRITE(IPT,45) (IAXIS(I),I=1,13)
       RETURN
C
    5  FORMAT (/58X,17H P L O T   S E T ,I2,/,58X,
     $                   20H--------------------,//,63X,2A4,A2/)
   15  FORMAT(60X,8HSTATION ,A10)
   25 FORMAT(54X,8HSTATION ,A10,12H TO STATION ,A10)
   35  FORMAT (/,54X,F11.2,' TO',F7.2,/)
   45  FORMAT (6X,12(I3,7X),I3)
   55  FORMAT (7X,25(5H|    ))
   65  FORMAT (3X,I3,1H-,121A1,1H-,I2)
   75  FORMAT (7X,121A1)
C
       END
C-----------------------------------------------------------------------
      SUBROUTINE REJECT(V, KDPSI,NG,DOP,MXDOP,IRJ,SGMT,NGR,VV,ISWT,
     1 IZO,NM,NBI,IR, MXROW,A,C,W,IS,ITRM)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/10/83                  GEODOP V
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   SUBPROGRAMS USED:
C    FORTRAN:        ABS
C
C
C  DATA SCREENING SUBROUTINE
C
C   V    RESIDUALS
C   KDPSI  INDEX VECTOR OF USED DOPPLERS
C   NG     INDEX VECTOR OF USED DOPPLERS PER INTERVAL  MULT STA SOLUT
C   DOP    OBSERVATIONS
C   MXDP   DIMENSION OF NG, DOP, KDPSI, V PER SATION
C   IVRJ   NO OF REJECTIONS
C   SGMT   UPPER STAT LIMLIT
C   NGR    NO OF OBSERVATIONS PER STAION
C   ISWT   SWITCH NE 0 REJECT MAXIMUM RESID ONLY  =0 REJECT ALL OUTLAYER
C   IZO  =1  NO REJECTION OCCURED    =0  REJECTION ENCOUNTERED
C
       COMMON /IOS/  ICD ,  INP ,  IOT ,  IPT , ISTA , ISTB , IRJT ,
     +              LOKH , LOKR , JDOP , LDOP , MDOP , NDOP
      DIMENSION V(1), KDPSI(1), NG(1), DOP(1)
      DIMENSION W(1), A(MXROW,3), C(MXROW,NBI)
      RMX=0.D0
      VV=0.
      RMX2=0.D0
      J=1
      DO 1 I=1,MXDOP
      II= IR +I - 1
      IF(DOP(I).EQ.0.D0) GO TO 2
      RR=DABS(V(J))
      IF(RR.GT.SGMT.AND.ISWT.EQ.0) GO TO 3
      IF(NG(IS+J).LT.ITRM) GO TO 13
      IF(RR.LE.RMX) GO TO 11
      RMX2= RMX
      K2= K
      K=I
      RMX=RR
      GO TO 2
11      CONTINUE
      IF(RR.LE.RMX2) GO TO 2
      K2= I
      RMX2=RR
      GO TO 2
3     IRJ= IRJ +1
      WRITE(IPT,5) V(J), NM,I,IRJ
13    DOP(I)= 0.D0
      IZO=0
      KDPSI(I)=0
      NG(I+IS)= NG(IS+I)-1
      NGR= NGR- 1
      W(J)= 0.D0
      DO 8 L=1,3
8      A(II,L)= 0.D0
      DO 9 L=1, NBI
9      C(II,L)= 0.D0
2     VV= VV + V(J)*V(J)
      J= J+1
1     CONTINUE
      KD= (K2- K)**2
      IF(ISWT.EQ.0) GO TO 4
C   MAXIMUM RESIDUALS ONLY
      IF(RMX.LE.SGMT) GO TO 4
10      DOP(K)= 0.D0
      IZO=0
      KDPSI(K)=0
      NG(K+IS)= NG(K+IS) -1
      IRJ= IRJ+1
      NGR= NGR-1
      II= K+IR -1
      W(K)= 0.D0
      DO 6 L=1,3
6      A(II,L)= 0.D0
      DO 7 L=1, NBI
7      C(II,L)= 0.D0
      WRITE(IPT,5) V(K), NM, K, IRJ
5     FORMAT(30X,'***** REJECTED AT 99%  V =',F10.1, 8H  ST. # ,A8,'  DO
     &PP # ',I4,' - REJECTION #',I4
     &/'----------------------------------------------------------------
     &------------------------')
      IF(KD.LT.4) GO TO 4
      IF(RMX2.LE.SGMT) GO TO 4
      K= K2
      KD= 0
      GO TO 10
4     RETURN
      END
C----------------------------------------------------------------------- 
      SUBROUTINE SATCRD(CXYZ,XBAS,CCC,SCC,CPP,STNC,IORD,DLT,LENG,
     1 CT, TT, Q, QL, AE, RMU, NMAX, PW,NTIM)
       IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 03/07/83                  GEODOP 5.13 (JAN83-IBM)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPARTMENT OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   FOR G1 OPERATION:
C   1. "IDUM1" AND "IDUM" ADDED TO "DAT" AND "FIT" LABELED COMMON
C      BLOCKS RESPECTIVELY TO GIVE CORRECT ALIGNMENT.
C
C   SUBPROGRAMS USED:
C    GEODOP:         DIST, DROTAT, DVDD, LESQFT, VARFIT
C    FORTRAN:        ABS, ASIN, ATAN, COS, DBLE, INT, SIN, SQRT
C
C
C        "SATCRD    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - J.KOUBA .. PROGRAM PREDOP
C                                     - J.KOUBA .. PROGRAM GEODOP
C                                     - D.E.WELLS .. SUBROUTINE LSA
C
C         VARIABLES USED     -ISTB    ,CORI    ,SORI    ,TPER
C                             DDMN    ,DMAN    ,DATP    ,APTP
C                             RAST    ,RAAN    ,DRAN    ,WE
C                             FACA    ,FECA    ,ECCT    ,SMAX
C                             FOPC    ,DATM    ,CCC     ,SCC
C                             CPP     ,ELMX    ,CELV    ,CDOP
C                             DNOM    ,KDOP    ,JDOP    ,MDOP
C                             XBAS
C
C         VARIABLES ALTERED  -FPAR    ,ISTB    ,ELMX    ,JDOP
C                             KDOP    ,M       ,CXYZ    ,
C
C         VARIABLES RETURNED -CXYZ    ,        ,        ,
C
C         EXTERNAL ROUTINES  -VARFIT  ,DROTAT  ,DIST    ,LESQFT
C                             ASIN    ,ATAN    ,COS     ,SIN
C
C         I/O DEVICES        -IPT     ,        ,        ,
C
C
C      COMPUTES VECTORS OF SAT. COORD.S [X,Y,ZS[
C         AND MATRIX OF XYZ COEFFICIENTS [CXYZ[.
C
C        SET FLAG [ISTB[ = 1 ... PASS OK
C                        = 2 ... PASS REJECTED.
C
       COMMON /DAT/ ANOT , AZIM , CDOP , CELV , CPAP , DATM , DNOM ,
     +              DTIM , ECCS , ELEV , ELMX , FRCV , IRCV , METL ,
     +              METP , NDIR , TIME , XMET ,
     +                BT ,   C4 ,   CC ,   CM ,LB,IDUM1, PI ,   RD ,
     +                RH ,   SS ,   ST ,   WE ,   XP
       COMMON /DEC/ TSPR , DMAN , APTP , DATP , ECCT , SMAX , RAAN ,
     +              DRAN , CORI , RAST , TSAT , TNJC , SORI , FSAT ,
     +              DDMN , TPER , LOKM
       COMMON /FIT/   M2 ,    M ,N,IDUM, DACA , DECA , DOPC , KDOP ,
     +               DOP ,  TIM , RESD , XNOR
       COMMON /IOS/  ICD ,  INP ,  IOT ,  IPT , ISTA , ISTB , IRJT ,
     +              LOKH , LOKR , JDOP , LDOP , MDOP , NDOP
       DIMENSION DATM(   3) , DACA(  14) , DECA(  14) , DOPC(  14) ,
     +           DNOM( 4,8) , DTIM(   4) , ELEV(  33) , XMET(4,1000),
     +            DOP(  96) , KDOP( 4,12) ,  TIM(  96) , CXYZ(10,3) ,
     +           FPAR(10,3) , FACA(  10) , FECA(  10) , FOPC(  10) ,
     +           ROTM( 3,3) ,    C(   3) ,    U(   3) ,
     + XS(35),YS(50),ZS(35),XBAS(10,35),UT(3),RESD(48),LL(3),STNC(3)
     +,ACR(2),CT(1), TT(1), Q(1), QL(1)
      EQUIVALENCE (FACA(1),FPAR(1,1)),(FECA(1),FPAR(1,2))
     @,(FOPC(1),FPAR(1,3))
       DATA LL / 3 , 1 , 3 /
     +,ACR/0.D0,0.D0/
C
C        FIT THREE VARIABLE ORBITAL PARAMETERS-
C
       CALL VARFIT (FPAR)
       IF (ISTB.EQ.2) RETURN
C
       C(2) =DATAN(CORI / SORI) - PI / 2.D0
       ISTB =  2
       M1   =  0
      WWE= WE*WE
       N=LENG /IDINT(DLT+ .1D0) + 1
          DTMK=TPER-.8000000D 01
       ANGL = DDMN * DTMK
       ELV1 = 0.D0
C      INITILIZE TIMMING ADJ.
      SUMTO= 0.D0
       SUMTT=0.D0
      ITME=0
      ITRJ=0
       FTMK = DMAN * DTMK
      TIMS= -.80000000D 01
           DO 400 I = 1,N
               ANGL = ANGL + DDMN + DDMN
               COSA =DCOS(ANGL)
               SINA =DSIN(ANGL)
       DTMK= DTMK+  DLT
               FTMK = FTMK + DDMN
       TIMS= TIMS+ DLT
C
               C(1) = DATP * DTMK - APTP
               C(3) = RAST - RAAN - (DRAN - WE) * DTMK
               DAC1 = FACA(1) + FACA(2) * TIMS + FACA(3) * SINA
     +                        + FACA(4) * COSA
               DEC1 = FECA(1) + FECA(2) * TIMS + FECA(3) * SINA
     +                        + FECA(4) * COSA
      SINF=DSIN(FTMK)
      ECC1= DEC1 + FTMK+ ECCT*SINF
      SINE=DSIN(ECC1)
      COSE=DCOS(ECC1)
      U(1)= (DAC1+ SMAX)*(COSE- ECCT)
      U(2)= (DAC1+SMAX)*SINE
               U(3) = FOPC(1) + FOPC(2) * TIMS + FOPC(3) * SINA
     +                        + FOPC(4) * COSA
C    COMPUTE DERIVATIVES OF A, E, O.P.
      ET= FECA(2)+((FECA(3)*COSA-FECA(4)*SINA)*.200000D 01
     1  +.1000000D 01+ECCT*DCOS(FTMK))*DMAN
      AT= FACA(2)+ (FACA(3)*COSA-FACA(4)*SINA)*.200000D 01*DMAN
      UT(3)=FOPC(2)+ (FOPC(3)*COSA-FOPC(4)*SINA)*.200000D 01*DMAN
C  COMPUTE DERV. OF U- COORDINATES
      UT(1)= AT*( COSE-ECCT)- ET*U(2)
      UT(2)= AT* SINE       +ET*(DAC1+SMAX)* COSE
               CALL DROTAT (   C ,   LL ,    3 , ROTM)
               XT = 0.D0
               YT = 0.D0
               ZT = 0.D0
       XST   =-(ROTM(1,2)*U(1)-ROTM(1,1)*U(2))*DATP
       YST   =-(ROTM(2,2)*U(1)-ROTM(2,1)*U(2))*DATP
       ZST   =-(ROTM(3,2)*U(1)-ROTM(3,1)*U(2))*DATP
                   DO 500 J = 1,3
                       XT = XT + ROTM(1,J) * U(J)
                       YT = YT + ROTM(2,J) * U(J)
                       ZT = ZT + ROTM(3,J) * U(J)
       XST   = XST    +ROTM(1,J)*UT(J)+ ROTM(2,J)*U(J)*(WE-DRAN)
       YST   = YST    +ROTM(2,J)*UT(J)- ROTM(1,J)*U(J)*(WE-DRAN)
       ZST   = ZST    +ROTM(3,J)*UT(J)
  500              CONTINUE
               XS(I) = XT
               YS(I) = YT
               ZS(I) = ZT
      XS(I+N)= XST
      YS(I+N)= YST
      ZS(I+N)= ZST
C
C        TEST SAT. ELVTNS --- TRIM DOPPLERS --- REJECT IF @ "CELV"-
C
               XT = XT - DATM(1) - STNC(1)
               YT = YT - DATM(2) - STNC(2)
               ZT = ZT - DATM(3) - STNC(3)
      IF(TIMS.LT.0.D0.OR.TIMS.GT.16.D0) GO TO 1
      IF(TIMS.NE.0.D0) GO TO 3
      TIMO= DIST(XT, YT, ZT)/CM
      TIMI= TIMO
      IF(DABS(TIM(63)).GT.1.D-08.AND.IRCV.NE.5.AND.IRCV.NE.7)
     1TIM(63)= TIM(63)- TIMO
3       CONTINUE
      IF(NTIM.EQ.0) GO TO 1
C   COMPUTE CORRECTION AT EVEN MINUTES ONLY
      K= IDINT(TIMS+.05D0)/2
      TST= TIMO -DBLE(K)*2.D0
      TST= DABS(TST)
      IF(TST.GT..05D0) GO TO 1
      RSD= TIMO- DIST(XT,YT, ZT)/CM  - DBLE(K)*2.D0
      IF(DABS(RSD).GT.5.D-06) GO TO 2
      SUMTO= SUMTO + RSD
      SUMTT= SUMTT + RSD*RSD
      ITME= ITME + 1
            TIMO= TIMI + TIM(64+K)
      GO TO 1
2     ITRJ= ITRJ + 1
1      CONTINUE
               ELV0=DASIN((XT * CCC + YT * SCC + ZT * CPP)
     +              / DIST(XT , YT , ZT)) * 57.295779513D0
C
       IF(I.LT.4.OR.I.GT.12) GO TO 380
               XT = ELV0 - ELV1
               IF (M1.EQ.1.OR.XT.GT.0.D0) GO TO 360
C
               ELMX=(ELV0-ELV2)*(ELV0-ELV2)/(8.D0*(ELV1+ELV1-ELV0-ELV2))
     +              +  ELV1
               IF (ELMX.LT.CELV) GO TO 10
      IF(I.EQ.4) GO TO 11
               M1   = 1
C
360    IF((ELV0.GT.CDOP.AND.ELV1.GT.CDOP).OR.I.EQ.4) GO TO 380
       M2= I-4
               YT =      - DNOM(1,M2)
                   DO 450 J = 1,4
                       YT = YT + DNOM(J,M2)
                     IF ((ELV1+XT*YT/2.000003958D0).GE.CDOP) GO TO 450
                       IF (KDOP(J,M2).NE.0) JDOP = JDOP - 1
                       KDOP(J,M2) = 0
  450              CONTINUE
  380          ELV2 = ELV1
               ELV1 = ELV0
      IF(NMAX.EQ.-1) GO TO 400
      IF(I.EQ.3) GO TO 501
C   USE 3 POINTS AT -10,0,AND 10 MIN. ONLY
      IF(I.NE.8.AND.I.NE.13) GO TO 505
      GO TO 503
 501     II= N-1
 503    II= II+2
      XS(II)= XS(I)/10.D0
      YS(II)= YS(I)/10.D0
      ZS(II)= ZS(I)/10.D0
      XS(II+1)= XST/10.D0
      YS(II+1)= YST/10.D0
      ZS(II+1)= ZST/10.D0
 505   CONTINUE
      U(1)= XS(I)
      U(2)= YS(I)
      U(3)= ZS(I)
C  COMPUTE SECOND DERIVATIVES  AND STORE AT XS, YS, ZS
      CALL DVDD(U,V,XS(I),YS(I),ZS(I),AE,RMU,CT,TT, Q, QL, NMAX)
      XS(I)=(XS(I) +.2000000D01*WE*YST+ WWE*U(1) )*PW
      YS(I)=(YS(I) -.2000000D01*WE*XST+ WWE*U(2) )*PW
      ZS(I)= ZS(I)* PW
  400      CONTINUE
       IF (JDOP.LT.MDOP) GO TO 20
C     COMPUTE MEAN AND SIGMA
      TIM(67)= TIM(63)
      TIM(63)= 0.D0
      IF(ITME.GT.1) TIM( 63)= SUMTO/ITME
      TIM( 64)= 0.D0
      IF(ITME.GT.2) TIM(64)=DSQRT(SUMTT-ITME*TIM( 63)**2)/DBLE(ITME-1)
      TIM(65)= ITME
      TIM(66)= ITRJ
      M= IORD
       N= N +6
      IF(NMAX.LT.0) N=(N-6)*2
       CALL LESQFT(ACR,XBAS, CXYZ(1,1), XS, 10, XMX)
       XNRM=DSQRT(XNOR/11.D0)
       CALL LESQFT(ACR,XBAS, CXYZ(1,2), YS, 10, YMX)
       YNRM=DSQRT(XNOR/11.D0)
       CALL LESQFT(ACR,XBAS, CXYZ(1,3), ZS, 10, ZMX)
      ZNRM=DSQRT(XNOR/11.D0)
       ISTB =  1
       RETURN
C
   10  WRITE (IPT, 5) ELMX , CELV
       RETURN
   11  WRITE (IPT,25) ELMX
       RETURN
C
   20  WRITE (IPT,15) JDOP , MDOP
       RETURN
C
    5  FORMAT (30X,'***** MAX ELVN ',F5.1,' @ ',F5.1,' CUTOFF - PASS REJ
     &ECTED *****'
     &/'----------------------------------------------------------------
     &------------------------')
   15  FORMAT (30X,'***** ',I2,' DOPPLERS @ ',I2,
     +            ' DOPPLER MINIMUM - PASS REJECTED *****'
     &/'----------------------------------------------------------------
     &------------------------')
   25  FORMAT (30X,'***** MAX ELVN ',F5.1,' BEFORE T0 - PASS REJECTED',
     +' *****'
     &/'----------------------------------------------------------------
     &------------------------')
       END
C-----------------------------------------------------------------------
      SUBROUTINE SCREEN(SIGC,CXYZ,SIGF,SPOS,ALAT,ALNG,IDOP,XINT,IORD)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*4 IMIS
C
C   VERSION: 01/18/83                  GEODOP V
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C        "SCREEN    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - J.KOUBA .. PROGRAM GEODOP
C
C         VARIABLES USED     -FRCV    ,TIME    ,NDOP    ,IORD
C                             CXYZ    ,SPOS    ,CM      ,IDOP
C                             CPAP    ,L       ,M       ,NDOP
C                             N       ,KDOP    ,JDOP    ,MDOP
C                             DNOM    ,FSAT    ,DOP     ,SIGF
C                             SIGC    ,ALAT    ,ALNG    ,AZIM
C
C         VARIABLES ALTERED  -CPAP    ,L       ,M       ,N
C                             JDOP    ,KDOP    ,NDOP    ,ISTB
C                             FRCV    ,AZIM    ,NDIR    ,
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -COORD   ,DIST    ,REJEKT  ,DROTAT
C                             MULTP   ,DEFALT  ,        ,
C                             ABS     ,ATAN2   ,INT     ,
C
C         I/O DEVICES        -IPT     ,        ,        ,
C
C
C        [SCREEN[ COMPUTES THE SAT. VECTOR "XS" AND TESTS THE DOPPLERS
C                 FOR MISCLOSURES > "CMIS" , AS WELL AS COMPUTE THE AZI-
C                 MUTH "AZIM" AND DIRECTION "NDIR" OF THE SAT.
C
C        SET FLAG "ISTB" = 1 ... PASS OK,
C                        = 2 ... PASS REJECTED.
C
       COMMON /DAT/ ANOT , AZIM , CDOP , CELV , CPAP , DATM , DNOM ,
     +              DTIM , ECCS , ELEV , ELMX , FRCV , IRCV , METL ,
     +              METP , NDIR , TIME , XMET ,
     +                BT ,   C4 ,  CCC ,   CM ,LB,IDUM1, PI ,   RD ,
     +                RH ,  SSS ,   ST ,   WE ,   XP
       COMMON /DEC/ TSPR , DMAN , APTP , DATP , ECCT , SMAX , RAAN ,
     +              DRAN , CORI , RAST , TSAT , TNJC , SORI , FSAT ,
     +              DDMN , TPER , LOKM
       COMMON /FIT/    L ,    M ,N,IDUM, DACA , DECA , DOPC , KDOP ,
     +               DOP ,  TIM , RESD , XNOR
       COMMON /IOS/  ICD ,  INP ,  IOT ,  IPT , ISTA , ISTB , IRJT ,
     +              LOKH , LOKR , JDOP , LDOP , MDOP , NDOP
       DIMENSION    C(   3) , DACA(  14) , DECA(  14) , DOPC(  14) ,
     +           RESD(  48) , XMET(1000,4),  TIM(  96) ,
     +           CLOS(  48) , CXYZ(10,3) ,  XLA(   6) ,
     +           DATM(   3) , DNOM(  32) ,  DOP(  96) ,
     +           DTIM(   4) , ELEV(  33) , KDOP(  48) ,
     +             LL(   3) , ROTM( 3,3) , SPOS(   3) ,   SK(  33) ,
     +      TIMI( 48),   XS( 6)
      DATA IMIS/  'CLOS' / ,DG/57.295779513D0/
     +    ,   LL /  2 ,  3 ,  0 /
     +    ,  PI2 / 1.570796327D0/
C      DCOS (XX) = COS (XX)
C      DSIN (XX) = SIN (XX)
C      DSQRT(XX) = SQRT(XX)
C
C        COMPUTE SAT. STATE VECTOR "XS" OVER PASS INTERVALS-
C
      TSCAL= .10000000D 01/ CM
       SCAL = 1.D+0 - FRCV / 4.D+8
      IF(TIME.NE.0.D0)GO TO 801
      SCAL= .999998021D 00
      TSCAL= 0.D0
801   CONTINUE
       DELT = 0.D0
       MAXD = NDOP + 1
      XTIM= .6000000D01
C          DO 200 J = 1,MAXD
      J=0
  100 J=J+1
               XTIM = XTIM + DELT
               ZT   = XINT * XTIM - 1.D0
               CALL COORD (  ZT , XINT , IORD , CXYZ ,   XS , SPOS)
               SKJ = DIST (XS(1) , XS(2) , XS(3))
               IF (J.EQ.1.OR.TIME.EQ.0.D0) GO TO 10
      ZT= ( SK(1)- SKJ)* TSCAL
                   DO 500 K = 1,3
                       XS(K) = XS(K) + XS(K+3) * ZT
  500              CONTINUE
               SKJ = DIST (XS(1) , XS(2) , XS(3))
C
   10  CSC = (XS(1) * XS(4) + XS(2) * XS(5) + XS(3) * XS(6))
     +     / (DIST (XS(4) , XS(5) , XS(6)) * SKJ)
       IF (J.EQ.1)          GO TO 30
       IF ((CSC*CSP).GT.0.D0) GO TO 30
       CPAP = XTIM - CSC / (CSC - CSP) * (DELT - (SKJ - SK(J-1)) / CM)
C
C        IF FLAG SET - BALANCE DOPPLERS ABOUT CPA-
C
               IF (IDOP.NE.0) GO TO 30
               K = J + IDINT((CPAP - XTIM) / DELT * 2.D0)
               L = 0
               M = 0
                   DO 600 I = 1,NDOP
                       N = NDOP - I + 1
                       IF (KDOP(I).NE.0.AND.L.EQ.0) L = I
                       IF (KDOP(N).NE.0.AND.M.EQ.0) M = N
                       IF ((L*M).NE.0) GO TO 20
  600              CONTINUE
   20          CONTINUE
               L    = K - L
               M    = M - K
               IF (L.GT.M) L = M
               JDOP = L * 2 + 1
               IF (JDOP.LT.MDOP) GO TO 50
               M    = K - L - 1
               IF (M.LT.1) GO TO 21
                   DO 650 I = 1,M
  650              KDOP(I) = 0
   21          N    = K + L + 1
               IF (N.GT.NDOP) GO TO 30
                   DO 660 I = N,NDOP
  660              KDOP(I) = 0
               MAXD = N
               NDOP = N - 1
C
   30          DELT = DNOM(J) * SCAL
               CSP = CSC
               SK(J) = SKJ
C 200      CONTINUE
      IF(J.LT.MAXD) GO TO 100
C
       I    = 1
       N    = 0
       FDIF = (FRCV + FSAT) * 60.D0
       XT   = 2.997925D 8 / (4.D 8 + FRCV)
       YT   = 0.D0
       ZT   = 0.D0
       ISTB = 1
C
C        GENERATE VECTOR OF CLOSURES-
C
           DO 700 J = 1,NDOP
               IF (KDOP(J).EQ.0) GO TO 700
               TDIF    = SK(J) - SK(J+1)
      TINT= DNOM(J) + TDIF* TSCAL
C   DECODE DOPPLERS
      IIDOP= DOP(J)/ 1.0000000D 07
      DIONS= IIDOP/ 1.000000D 01
       DOPP= DOP(J)- DIONS*.100000000000D09
      IF(DOPP.LT.0.D0) DOPP= DOPP + 1.000000000D07
      DELT= FDIF*TINT- TDIF/XT - DOPP
      IF(I.EQ.2) GO TO 42
C
C        FIND THE MEAN MISCLOSURE VALUE-
C
      IF (N.LT.4) GO TO 44
               I    = 2
      SCAL= SIGF* TINT * 3.5D0 * 60.D0
             SCLO=(CLOS(1)/TIMI(1)+CLOS(2)/TIMI(2)+CLOS(3)/TIMI(3))/3.D0
C
   42          IF(DABS(DELT-SCLO*TINT).LE.SCAL) GO TO 44
               KDOP(J) = 0
               JDOP    = JDOP - 1
               WRITE (IPT,25) J    , DELT
               GO TO 700
C
   44          N       = N + 1
               CLOS(N) = DELT
               TIMI(N) = TINT
               YT      = YT - DELT
               ZT      = ZT + TINT
  700      CONTINUE
       FDIF = YT / ZT
       FRCV = FRCV + FDIF / 60.D0
           DO 800 J = 1,N
               CLOS(J) = CLOS(J) + TIMI(J) * FDIF
  800      CONTINUE
C
C        TEST CLOSURES --- 1) REJECT LARGE MISCLOSURES > SIGC
C                          2) FIT CLOSURES AND REJEKT THOSE > 10: SIGC.
C
      SCLO= SIGC* 3.5D0
       CALL REJEKT (CLOS , SCLO , IMIS ,    2)
       IF (JDOP.LT.MDOP) GO TO 50
C
C        NOTE - CLOSURE FITTING NOT DONE CURRENTLY
C      CALL CLOFIT (CLOS , SCLO * .1 , FCLO , IMIS)
C
C
C        GET COORDS AT CLOSEST APPROACH-
C
       YT = CPAP * XINT - 1.D0
       CALL COORD (  YT , XINT , IORD , CXYZ ,   XS , SPOS)
       C(1) = PI2 - ALAT
       C(2) = PI  - ALNG
       CALL DROTAT (   C ,   LL ,    2 , ROTM)
       CALL MULTP (3 , 3 , 1 , 3 , 3 , ROTM , XS(1) , XLA(1))
       CALL MULTP (3 , 3 , 1 , 3 , 3 , ROTM , XS(4) , XLA(4))
       XLA(1) = DEFALT (XLA(1) , 1.D-8)
       XLA(5) = DEFALT (XLA(5) , 1.D-8)
       AZIM =DATAN2(-XLA(2) , XLA(1))
       DIRS =DATAN2( XLA(4) ,-XLA(5))
       NDIR = IDINT((AZIM + PI) / PI) + IDINT(DABS(DIRS) / DIRS) + 2
       AZIM = AZIM * DG
       RETURN
C
   50  WRITE (IPT,15) JDOP , MDOP
       ISTB = 2
       RETURN
   15  FORMAT (30X,'***** ',I2,' DOPPLERS @ ',I2,
     +            ' DOPPLER MINIMUM - PASS REJECTED *****'
     &/'----------------------------------------------------------------
     &------------------------')
   25  FORMAT (30X,'***** WARNING - MISCLOSURE # ',I2,', ',F8.0,
     &' DELETED *****')
       END
       FUNCTION SFIX(I)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION:  9/25/82                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C
C
C        "SFIX      " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -I       ,        ,        ,
C
C         VARIABLES ALTERED  -I       ,SFIX    ,        ,
C
C         VARIABLES RETURNED -SFIX    ,        ,        ,
C
C         EXTERNAL ROUTINES  -DBLE   ,        ,        ,
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
       IF (I.GE.100000000) I = -(I - 100000000)
       SFIX =DBLE(I)
       RETURN
       END
       FUNCTION SLAN (COEF , CORD , IORD , SCAL , BIAS , TIME)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C   VERSION: 01/11/83                  GEODOP IV+V   (JUL81)
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
C-----------------------------------------------------------------------
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
C----------------------------------------------------------------------- 
      SUBROUTINE VARFIT (FPAR)
      IMPLICIT REAL*8 (A-H,O-Z)
        CHARACTER*4 IPAR
C
C   VERSION: 01/28/83                  GEODOP 5.09 (OCT82)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   FOR G1 OPERATION:
C   1. "IDUM" ADDED TO "FIT" LABELED COMMON TO GIVE CORRECT ALIGNMENT.
C
C
C
C        "VARFIT    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SUMMER /74.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - J.KOUBA .. PROGRAM PREDOP
C                                     - J.KOUBA .. PROGRAM GEODOP
C                                     - D.E.WELLS .. SUBROUTINE LSA
C
C         VARIABLES USED     -L       ,TPER    ,FIXD    ,LOKH
C                             VPAR    ,N       ,DDMN    ,M
C                             RESD    ,IPT     ,XNOR    ,
C
C         VARIABLES ALTERED  -ISTB    ,M       ,TPER    ,IFIT
C                             N       ,L       ,FPAR    ,RESD
C
C         VARIABLES RETURNED -FPAR    ,        ,        ,
C
C         EXTERNAL ROUTINES  -IQUIK   ,LESQFT  ,        ,
C                             COS     ,DBLE   ,SIN     ,SQRT
C
C         I/O DEVICES        -IPT     ,        ,        ,
C
C
C        [VARFIT[ DOES A 4TH ORDER FIT TO THE 3 VARIABLE PARAMETERS "VPA
C                 AND THEN TESTS THE RESIDUALS AND NORM WITH A HALPERIN
C                 AND CHI-SQUARED 99: REJECTION CRITERION. THE REFERENCE
C                 SET OF COS/SIN COEFFICIENTS "COEF" ARE ALSO UPDATED IF
C                 THE 2 TESTS ARE SUCCESSFUL.
C
       COMMON /DEC/ FIXD , DDMN , TPER , LOKM
C
C NON-STANDARD FORTRAN IN CDC VERSION.  "CRAP" HAS TAKEN THE PLACE
C  OF INTEGER AND REAL VARIABLES.  THIS WILL ONLY WORK IF WORD
C  LENGTHS ARE IDENTICAL, AS CDC SINGLE PRECISION, OR IBM Q WITH
C  AUTODBL(DBLPAD) OPTION.
C
C      COMMON /FIT/    L ,    M ,N,IDUM, VPAR , CRAP , RESD , XNOR
       COMMON /FIT/    L ,    M ,N,IDUM, VPAR,                KDOP ,
     +               DOP ,  TIM , RESD , XNOR
       COMMON /IOS/  ICD ,  INP ,  IOT ,  IPT , ISTA , ISTB , IRJT ,
     +              LOKH , LOKR , JDOP , LDOP , MDOP , NDOP
       COMMON /MJV/ IFIL , IFIT, KSAT(17)
C
       DIMENSION FIXD(14), VPAR(14,3),          COEF(2,3,10) ,
     *           KDOP(48),DOP(96),TIM(96),
     +           FPAR(10,3) ,BASE(10,14) , FUNC(  14) , ACOR(   2) ,
     +           IPAR( 3,3) , SCAL(   3) , RESD(  48) , IFIL(  62)
       DIMENSION IFLG(32)
       DIMENSION CHI(15) , HAL(15)
       DIMENSION WGT (   2)
C
      DATA BASE/140*1.D0/, COEF/60*0.D0/, ACOR/.111111D0 ,
     +         .08888888889D0/, SCAL / 10.D0,1.745329252D-6 , 10.D0/
     +    , PI2  / 6.2831853072D0/, S    / .6D0/
      DATA IPAR/ ' DEL','TA A','K   ',' DEL','TA E','K   '
     @,          'DELT','A ET','A   ' /
       DATA CHI /  7.88D0, 10.6D0, 12.8D0, 14.9D0, 16.8D0, 18.6D0, 20.3D
     +0         ,  22.0D0, 23.6D0, 25.2D0, 26.8D0, 28.3D0, 29.8D0, 31.3D
     +0         ,  32.8D0/
       DATA HAL /    0.D0,   0.D0,2.396D0,2.617D0,2.763D0,2.868D0,2.951D
     +0         , 3.017D0,3.073D0,3.120D0,3.162D0,3.198D0,3.230D0,3.259D
     +0         , 3.286D0/
       DATA WGT / .5D0,.7071067812D0/
C
       I1   =  1
       I2   = 14
       I3   =  1
       ISAT =  L
       ISTB =  2
       M    =  4
       SIGN = 0.
       IF (TPER.LE.-480.D0)               SIGN =  1.D0
       IF (TPER.GE.(1440.D0 - PI2 / FIXD(2))) SIGN = -1.D0
       TPER = TPER + SIGN * 1440.D0
C
       C1  = 0.D0
       C2  = 0.D0
       ACOR(1) = 0.D0
       ACOR(2) = 0.D0
       IFIT = 0
           DO 100 I = 1,3
               FACT = SCAL(I)
               N    = 0
               IF (I.LT.3) GO TO 10
C
               J    = IQUIK(LOKH,4,2)
               I1   =  J + 1
               I2   = 13 - J
               I3   =  2
C
   10              DO 200 J = I1,I2,I3
                       IF (VPAR(J,I).LT.3000.) GO TO 200
                       N         = N + 1
                       XT        =DBLE((J - 3) * 2)
                       BASE(1,N) = 1.D0
                       BASE(2,N) = XT
                       XT        = DDMN * (TPER + XT)
                       BASE(4,N) =DCOS(XT)
                       BASE(3,N) =DSIN(XT)
                       FUNC(  N) = VPAR(J,I) - 4000.D0
                       IFLG(  N) = 1
  200              CONTINUE
                   DO 320 K = 1,2
                       II = K
                       XT = WGT(K)
                           DO 320 KK = 1,2
                                   DO 330 L = 1,M
                                       BASE(L,II) = BASE(L,II) * XT
  330                              CONTINUE
                               FUNC(II) = FUNC(II) * XT
  320                  II = N - K + 1
C
               IF (I.EQ.3.AND.N.LE.M) M = M - 1
           DO 300 II = 1,3
               J = N - M
               IF (J) 30 , 40 , 50
C
   30          WRITE (IPT, 5) (IPAR(JJ,I),JJ=1,3)
               RETURN
C
   40          WRITE (IPT,15) (IPAR(JJ,I),JJ=1,3)
C
   50          CONTINUE
               CALL LESQFT (ACOR , BASE , FPAR(1,I) , FUNC , 10 , XMAX)
               XMAX = 0.D0
                   DO 340 K = 1,N
                       XT   = RESD(K)
                       XT   = XT / BASE(1,K)
                       IF (DABS(XT).LE.XMAX) GO TO 340
                       L    = K
                       XMAX = DABS(XT)
  340              RESD(K) = XT
               IF (J.EQ.0) GO TO 80
               XT = HAL(N) *DSQRT(DBLE(J) /DBLE(N)) * S
   52          IF (XMAX.LE.XT) GO TO 60
               WRITE (IPT,35) (IPAR(JJ,I),JJ=1,3),L,XMAX
               IFLG(L) = 0
C
   53              DO 355 L = 1,N
                       IF (IFLG(L).EQ.1) GO TO 355
                       IN= N - 1
                           DO 360 J = L,IN
                               FUNC(J) = FUNC(J+1)
                                   DO 360 K = 1,M
                                       BASE(K,J) = BASE(K,J+1)
  360                          CONTINUE
  355              CONTINUE
      N=IN
  300      CONTINUE
C
   60          C1   = FPAR(4,I)
               C2   = FPAR(3,I)
               XT   = CHI(J) * S * S
               IF (XNOR.LE.XT) GO TO 70
C
               IFIT = I
               WRITE (IPT,25) (IPAR(JJ,I),JJ=1,3),XNOR
               GO TO 80
C
   70          COEF(1,I,ISAT) = C1
               COEF(2,I,ISAT) = C2
C
   80              DO 400 J = 1,M
                       FPAR(J,I) = FPAR(J,I) * FACT
  400              CONTINUE
  100      CONTINUE
       ISTB = 1
       RETURN
C
    5 FORMAT(30X,'***** NEGATIVE DEGREE FREEDOM - ',2A2,A2,
     &' - PASS REJECTED *****'
     &/'----------------------------------------------------------------
     &------------------------')
   15 FORMAT(30X,'***** WARNING - ZERO DEGREE FREEDOM - ',2A4,
     &A2,' *****')
   25 FORMAT(30X,'***** WARNING - ',2A4,A2,' NORM',F8.2,2X,
     &' - 99% CHI-SQR TEST *')
   35 FORMAT(30X,'***** WARNING - ',2A4,A2,' RESIDUAL # ',I2,F6.2
     +               ,' - 99% HALPERN TEST *')
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

