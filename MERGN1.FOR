      SUBROUTINE NFILIO (IFUN , ISWT , IDEV)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*10 NDESC,NAME, ISTN, NIDN,HEDR,IST1,IST2,NAM1,NAME1
     +            , ISSN, ISTU,DAYT
      CHARACTER*4 ISRC ,ISRCP, ISR1, IBLNK, NWLI, NOTE
C
C   VERSION: 03/25/83                  GEODOP 5.03   (NOV81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   THIS IS A SPECIAL VERSION OF "FILIO", 
C   WHICH ALLOWS OUTPUTING ALL PASSES WHEN DOING
C   EPHEMERIS MERGE-ING.
C
C
C
C        "FILIO     " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SEPTEMBER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         VARIABLES USED     -IFUN    ,KSAT    ,ISWT    ,IOUQ
C                             LOOP    ,IOUT    ,IDEV    ,INWL
C                             NFIL    ,NSAT    ,NYER    ,NIDN
C                             HEDR    ,IORD    ,DAYT    ,NSNN
C                             NORD    ,NDOP    ,TIMN    ,ISNN
C                             ALOK    ,NSTN    ,NDOP    ,NAME
C                             ISRC    ,ISAT    ,INJC    ,LOKR
C                             LENG    ,FSAT    ,ORBP    ,IFIT
C                             COEF    ,KDOP    ,NPAS    ,NTIM
C                             ISTN    ,PRES    ,TDRY    ,TWET
C                             IPAS    ,FRCV    ,DOPP    ,TME
C                             NPSS    ,ISCR    ,ISCQ    ,ILIM
C                             ISSN
C
C         VARIABLES ALTERED  -ISNN    ,LOOP    ,NFIL    ,NSNN
C                             KSAT    ,ISCR    ,ISCQ    ,ISWT
C                             NOTE    ,NPSS    ,NPAS    ,TIMN
C                             NYER    ,NSAT    ,NDAY    ,NIDN
C                             IORD    ,HEDR    ,NORD    ,NAME
C                             ISRC    ,ISAT    ,INJC    ,LOKR
C                             LENG    ,FSAT    ,ORBP    ,IFIT
C                             COEF    ,NDOP    ,IEOF    ,ISTN
C                             ALOK    ,PRES    ,TDRY    ,TWET
C                             IPAS    ,FRCV    ,DOPP    ,TME
C                             NTIM    ,NSTN    ,ISNN    ,KDOP
C                             IDEV    ,ISTU    ,        ,
C
C         VARIABLES RETURNED -ISWT    ,IDEV    ,        ,
C
C         EXTERNAL ROUTINES  -DDHHMM  ,MODULO  ,        ,
C                             EOF              ,IABS    ,INT
C
C         I/O DEVICES        -INWL    ,IPRT    ,ISCR    ,ISCQ
C                             IOUT    ,        ,        ,
C
C
C        [FILIO[ DOES THE I/O FUNCTIONS FOR MERGING.
C
C                "IFUN" SELECTS THE NINE MODES- 1) READ/WRITE HEADER REC
C                                               2) READ NEXT PASS (IDEV)
C                                               3) WRITE CURRENT PASS
C                                               4) WRITE MERGED PASS
C                                               5) CLOSE/REWIND SCRATCH
C                                               6) WRITE EDITED DATA-SET
C                                               7) READ NEXT NWL PASS
C                                               8) WRITE MERGED NWL PASS
C                                               9) INITIALIZE EVERYTHING
C
C                "ISWT" DETERMINES THE DATA-SET SOURCE- 1) SCRATCH FILE
C                                                       2) NEXT (TAPE4).
C
C                "IDEV" IS THE LOGICAL I/O UNIT #.
C
C
       COMMON HEDR , NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT
     +      , NSTN , ORBP , IFIT , COEF , ISTN , ALOK , PRES , TDRY
     +      , TWET , IPAS , FRCV , NDOP , NTIM , DOPP , TME , NORD
     +      , NYER , NSAT , NDAY , NIDN , IORD , ISRCP , MORD
       COMMON /DAT/  DAYT , LOOP , IPRN , ISNN , IEOF , ILIM , NSNN
     +             , ISSN , TIMN , NOTE , NPAS , NPSS
     +             , KSAT , ALKMX,TMINM ,  ITMN
       COMMON /DEV/  INPT , INWL , IOUQ , IOUT , IPRT , IRED , ISCQ
     +             , ISCR , ISWI , ISWJ , ISTO , NFIL , IONE , ITWO
     +             , ITTO
       DIMENSION                 NAME(      2) , ISRC(      2)
     +         , ISAT(      2) , INJC(      2) , LOKR(      2)
     +         , LENG(      2) , FSAT(      2) , NSTN(      2)
     +         , ORBP(    7,2) , IFIT(      2) , COEF(   30,2)
     +         , ISTN(   15,2) , ALOK(   15,2) , PRES(   15,2)
     +         , TDRY(   15,2) , TWET(   15,2) , IPAS(   15,2)
     +         , FRCV(   15,2) , NDOP(   15,2) , NTIM(   15,2)
     +         , DOPP( 50,15,2), TME( 50,15,2) , NPAS( 15)
     +         , KSAT(      6) , ORB(7)
       DIMENSION ILIM(      6) , TIMN(     15) , ISSN(     15)
     +         , NOTE(   4,15) , ISP1(      3) , ISP2(      3)
     +         , KDOP(     15) , KEEP(      2)
     +         , ALK2(     15) , IST2(     15) , NDP2(     15)
     +         , NSAV( 2,  17) , ISTU(   2,15) , ORB1(      7)
     +         , COF1(     30) , COF2(     30) , NDP1(     15)
     +         , NTM1(     15) , IST1(     15)
     +         , ALK1(     15) , PRS1(     15) , TDR1(     15)
     +         , TWT1(     15) , IPS1(     15) , FRQ1(     15)
     +         , DOP1( 50,15)  , TIM1( 50,15)  , VARORB(21)
       EQUIVALENCE (IST2(1),ISTN(1,2)) , (ALK2(1),ALOK(1,2))
     +                              , (ITIM,LOKR(  2))
     +           , (NDP2(1),NDOP(1,2)) , (ISR1,ISRC(  1))
     +           , (ISA1,ISAT(  1)) , (ISA2,ISAT(  2))
     +           , (ITM1,LOKR(  1)) , (LEN1,LENG(  1))
     +           , (NAM1,NAME(  1)) , (INJ1,INJC(  1))
     +           , (FSA1,FSAT(  1)) , (NST1,NSTN(  1))
     +           , (ORB1(1),ORBP(1,1)) , (IFI1,IFIT(  1))
     +           , (COF1(1),COEF(1,1)) , (COF2(1),COEF(1,2))
     +           , (NDP1(1),NDOP(1,1)) , (NTM1(1),NTIM(1,1))
     +           , (IST1(1),ISTN(1,1)) , (ALK1(1),ALOK(1,1))
     +           , (PRS1(1),PRES(1,1)) , (TDR1(1),TDRY(1,1))
     +           , (TWT1(1),TWET(1,1)) , (IPS1(1),IPAS(1,1))
     +           , (FRQ1(1),FRCV(1,1)) , (DOP1(1,1),DOPP(1,1,1))
     +           , (LEN2,LENG(  2)) , (TIM1(1,1),TME(1,1,1))
     +           , (KSA6,KSAT(  6))
C  VALID SATELLITES NO
       DATA NSAV / 13,59,  20,77,  11,93,  30,119,  48,105,  50,115,
     +             12,58,  14,60,  19,68,  24,118,  23,124,  25,129,
     +             27,126, 29,127, 31,130, 32,125,  49,128/,NWLI/'PREC'/
      DATA IBLNK/'   '/
CC
       GO TO ( 10 ,  30 ,  40 ,  50 ,  70 ,  80 ,  90 , 110 ,  1) , IFUN
CC
    1  ISNN =  0
       ISTO =  0
       LOOP =  0
       NFIL =  0
       NSNN =  0
C--- SET 'ISCR' DEFAULT TO SAME AS IN 'MERGE'.
       ISCR =  1
       IONE =  2
C--- SET 'ISCQ' DEFAULT TO SAME AS IN 'MERGE'.
       ISCQ =  2
       ITWO =  2
       ISWT =  3
       ITTO = -1
           DO 2 I = 1,15
               NOTE(1,I) = IBLNK
               NOTE(2,I) = IBLNK
               NOTE(3,I) = IBLNK
               NOTE(4,I) = IBLNK
    2      CONTINUE
           RETURN
CC
10     ITMN=0
       IEOF=0
       NPSS = 0
       ILIN = 3
           DO 12 J = 1,15
               NPAS(  J) =  0
               TIMN(  J) = -1.D0
  12       CONTINUE
       IF (ISWT.EQ.0)       ISCR = IOUQ
       IF (ISWT.NE.LOOP)    GO TO 20
       ISCR = IOUT
       REWIND IDEV
       J    = 0
           DO 3 I = 1,5
               IF (KSAT(I).EQ.0) GO TO 3
               J       = J + 1
               KSAT(J) = KSAT(I)
    3      CONTINUE
       KSA6 = J
       IF (J.EQ.0)          KSAT(1) = 0
       IF (LOOP.NE.0)       GO TO 20
C
       READ (INWL,END=32) NYER , NSAT , NDY , NIDN ,ID, ISRCP, IOOR
       IORD= IOOR
       NDAY= NDY
       NFIL = NFIL + 1
       IF(NYER.GT.1900) NYER= NYER-1900
C
C  PROCCESS SAT. NO.
      IF(NSAT.EQ.5.OR.NSAT.EQ.15) NSAT= NSAT + 100
       IF (NSAT.LT.57 ) GO TO 9
       DO 19 I=1,17
       IF(NSAT.EQ.NSAV(2,I)) NSAT= NSAV(1,I)
19     CONTINUE
 9     CONTINUE
C--- READ "PREDOP" HEADER  (6 EOF'S NECCESSARY BEFORE STOP).
20    CONTINUE
      READ(IDEV,END=32) NAME1, JORDR, HEDR   , ISTN1, ISRC1
      IF(LOOP.NE.0)IORD=JORDR
      MORD= JORDR* 3
       IF (LOOP.EQ.0)       WRITE (IPRT,35) NFIL , NYER , NSAT , NIDN
     @, NAME1, IORD, HEDR   , ISTN1
       NORD    = IORD * 3
       HEDR    = DAYT
      IF(ISWT.EQ.0.AND.LOOP.EQ.0) ISRC1=ICRCP
C      WRITE (ISCR) HEDR
      WRITE(ISCR) NAME1, IORD, HEDR   , ISTN1, ISRC1
C      IF (LOOP.GT.1)       READ  (ISCQ) HEDR
      IF(LOOP.GT.1) READ(ISCQ) NAME1, IORD, HEDR   , ISTN1, ISRC1
       ISWJ= 0 
       RETURN
C
   30 READ(IDEV,END=32) NAME(ISWT), ISRC(ISWT), ISAT(ISWT), INJC(ISWT)
     +           , LOKR(ISWT) , LENG(ISWT) , FSAT(ISWT) , NSTI
     +           , (ORBP(I,ISWT),I=1,   7) , IFIT(ISWT)
     +   ,(COEF(I,ISWT),I=1,MORD),(NDOP(I,ISWT),I=1,NSTI)
      IEOF=0
      GO TO 33
   32 IEOF=3
      RETURN
   33 CONTINUE
           DO 100 I = 1,NSTI
               NDPP = NDOP(I,ISWT)
               IF (NDPP.EQ.0) GO TO 100
               READ (IDEV) ISTN(I,ISWT) , ALOK(I,ISWT) , PRES(I,ISWT)
     +                   , TDRY(I,ISWT) , TWET(I,ISWT) , IPAS(I,ISWT)
     +                   , FRCV(I,ISWT) , NTMM
     +                   , (DOPP(J,I,ISWT),J=1,NDPP)
     +                   , (TME(J,I,ISWT),J=1,NTMM)
               NTIM(I,ISWT) = NTMM
  100      CONTINUE
       NSTN(ISWT) = NSTI
       IF (ISWT.EQ.1)    RETURN
C
       IF (ITMN.EQ.NSTI) GO TO 37
   36      DO 200 I = 1,NSTI
               IF (NDP2(I).EQ.0.OR.TIMN(I).GE.0.D0) GO TO 200
               ISNN       = ISNN + 1
               ITMN       = ITMN + 1
               TIMN(   I) = ALK2(I)
               IF(TIMN(I).LT.TMINM)  TMINM = TIMN( I) 
  200      CONTINUE
   37  IF (ISTO.EQ.ISA2.AND.((ITTO - ITIM) / 16).EQ.0) GO TO 30
       IF(ISWJ.EQ.1)GO TO  39
C
   38  ITWO = 2
       ITWO = 2
       NSNN = NSNN + NSTI
       ISWJ = 1
   39  RETURN
CC            IFUN = 3  WRITE PASS
   40  KEEP(ISWT) = 1
       KEEP(IABS(ISWT-3)) = 0
       LIM1 = ISWT
       LIM2 = ISWT
C CHECK FOR CONDITIONS FOR COMBINIG FILES INTO ONE    
C      SIMULTANEOUS CONDITIONS ARE: 
C           1- SAME NO. OF SATIONS ON ALL FILES       
C           2- STARTING TIME MUST BE .GT. ENDING OF PRECEEDIG FILE
C           3- SAME STATION NAMES IN THE SAME ORDER 
      IF(NSTN(1).NE.NSTN(2)) GO TO 251
      IF(TMINM.LT.   ALKMX)   GO TO 251 
      NSTI= NSTN(ISWT)
      DO 253 J=1,NSTI 
      IF(NDOP(J,ISWT).EQ.0) GO TO 253 
      IF(ISTN(J,1).NE.ISTN(J,2))GO TO 251 
253   CONTINUE
C  NOW COMBINIG FILES INTO ONE
      NSNN= NSTI
      ITWO= ISWT
      IONE= ISWT
251   CONTINUE
       GO TO 60
CC
   50  KEEP(1) = 1
       KEEP(2) = 1
       LIM1 = 1
       LIM2 = 2
CC
   60  K = 0
           DO 250 I = IONE,ITWO
               ISEL = KEEP(I)
               NSTI = NSTN(I)
                   DO 250 J = 1,NSTI
                       K       = K + 1
                       KDOP(K) = NDOP(J,I) * ISEL
  250          CONTINUE
       WRITE (IDEV) NAME(ISWT) , ISRC(ISWT) , ISAT(ISWT) , INJC(ISWT)
     +            , LOKR(ISWT) , LENG(ISWT) , FSAT(ISWT) , NSNN
     +            , (ORBP(I,ISWT),I=1,   7) , IFIT(ISWT)
     +            , (COEF(I,ISWT),I=1,NORD) , (KDOP(I     ),I=1,NSNN)
C
           DO 300 I = LIM1,LIM2
               NSTI = NSTN(I)
                   DO 300 J = 1,NSTI
                       NDPP = NDOP(J,I)
                       IF (NDPP.EQ.0) GO TO 300
                       IF (I   .EQ.2) NPAS(J) = NPAS(J) + 1
                       NTMM = NTIM(J,I)
                       WRITE (IDEV) ISTN(J,I) , ALOK(J,I) , PRES(J,I)
     +                            , TDRY(J,I) , TWET(J,I) , IPAS(J,I)
     +                            , FRCV(J,I) , NTMM
     +                            , (DOPP(K,J,I),K=1,NDPP)
     +                            , (TME(K,J,I) , K = 1,NTMM)
  300          CONTINUE
       NPSS = NPSS + 1
       IF (LIM1.EQ.2) RETURN
       ISTO = ISA1
       ITTO = ITM1
       RETURN
CC
   70  IF (ISWT.EQ.3) GO TO 74
       ENDFILE IDEV
       IF (ISWT.EQ.2) RETURN
       IDEU = ISCQ
       ISCQ = IDEV
       IDEV = IDEU
       IONE =     1
       ITWO =     1
       REWIND  ISCQ
   74  REWIND  IDEV
       RETURN
CC
   80  NFIL = NFIL + 1
       IF (ISWT.EQ.1)       WRITE (IPRT, 5) NFIL
   82 READ(IDEV,END=84) NAM1, ISR1, ISA1, INJ1, ITM1, LEN1, FSA1
     +            , NST1 , ORB1 , IFI1 , (COF1(I),I=1,NORD)
     +                                 , (NDP1(I),I=1,NST1)
C      IF (EOF(IDEV).NE.0.) GO TO 84
       INOT = 1
       IF (ITM1.LT.ILIM(1)) INOT = 0
       IF (ITM1.LT.ILIM(2)) GO TO 86
C
   84  WRITE (IPRT,65) NFIL , (ILIM(I),I=3,6) , NPSS
     +                      , (ISSN(I) , NPAS(I),I=1,NSNN)
       RETURN
C
   86  IF (INOT.NE.1)       GO TO 88
           DO 350 I = 1,KSA6
               IF (ISA1.NE.KSAT(I)) GO TO 350
               INOT = 0
               GO TO 88
  350      CONTINUE
       WRITE (IOUT) NAM1 , ISR1 , ISA1 , INJ1 , ITM1 , LEN1 , FSA1
     +            , NST1 , ORB1 , IFI1 , (COF1(I),I=1,NORD)
     +                                 , (NDP1(I),I=1,NST1)
       NPSS = NPSS + 1
       IF (ISWT.NE.1)       GO TO 88
       CALL DDHHMM (INJ1 , ISP1)
       CALL DDHHMM (ITM1 , ISP2)
       WRITE (IPRT,15) NPSS , ISA1 , ISP1 , ISP2 , LEN1 , FSA1 , IFI1
   88      DO 400 I = 1,NST1
               NDPP = NDP1(I)
               IF (NDPP.EQ.0) GO TO 400
               READ (IDEV) IST1(1) , ALK1(1) , PRS1(1) , TDR1(1)
     +                   , TWT1(1) , IPS1(1) , FRQ1(1) , NTMM
     +                   , (DOP1(J,1),J=1,NDPP)
     +                   , (TIM1(J,1),J=1,NTMM)
               IF (INOT.NE.1) GO TO 400
               NPAS(I) = NPAS(I) + 1
               WRITE (IOUT) IST1(1) , ALK1(1) , PRS1(1) , TDR1(1)
     +                    , TWT1(1) , IPS1(1) , FRQ1(1) , NTMM
     +                    , (DOP1(J,1),J=1,NDPP) , (TIM1(J,1),J=1,NTMM)
               IF (ISWT.NE.1) GO TO 400
C--- ELEMENT OF ALK1 WASN'T SPECIFIED IN CDC VERSION.
               CALL DDHHMM(IDINT(ALK1(1)) , ISP1)
               WRITE (IPRT,25) IST1(1) , ISP1    , PRS1(1) , TDR1(1)
     +                       , TWT1(1) , IPS1(1) , FRQ1(1) , NDPP , NTMM
               ILIN = MOD (ILIN , 60) + 1
               IF (ILIN.EQ.2) WRITE (IPRT, 5) NFIL
  400      CONTINUE
       GO TO 82
CC
   90  IF(ISRCP.EQ.'PREC')
     + READ(IDEV,END=92) ITIM, LEN2, (COF2(I),I=1,NORD),NSAT,IYER
       IF(ISRCP.EQ.'SARC')
     + READ(IDEV,END=92) ITIM, LEN2, (COF2(I),I=1,NORD),NSAT
     + ,FSAT(2), ORB, NOTE1
       IF(ISRCP.EQ.'UPDT')
     + READ(IDEV,END=92) ITIM, LEN2, (COF2(I),I=1,NORD),NSAT
     + , NDESC, VARORB
C  PROCCESS SAT. NO.
      IF(NSAT.EQ.5.OR.NSAT.EQ.15) NSAT= NSAT + 100
       IF (NSAT.LT.57 ) GO TO 109
       DO 1019 I=1,17
         IF(NSAT.EQ.NSAV(2,I)) THEN
           NSAT= NSAV(1,I)
           GOTO 109
         ENDIF
1019   CONTINUE
 109   CONTINUE
      IEOF=ISWT
      RETURN
   92 IEOF=ISWT+3
       RETURN
CC
C
110      CONTINUE
      II= ISWT
      JORD= MORD/3
      ISHT= IORD- JORD
      IF(ISHT.LE.0) GO TO 111
C   SHIFT COEFFICIENT IF JORD .GT. IORD)
      K= MORD
      DO 112 I=1,2
      ISH= ISHT * (3-I)
      DO 112 J=1,JORD
      KJ= K + ISH
      COEF(KJ,1)= COEF(K     ,1)
      COEF(K,1) = 0.D0
      K= K- 1
112      CONTINUE
      IF(KSA6.EQ.0) GO TO 111
      DO 114 I=1,KSA6
      IF(ISA1.EQ.KSAT(I) ) GO TO 113
114      CONTINUE
111   WRITE(IDEV) NAM1,ISRC(II),ISA1,INJ1,LOKR(II), LENG(II), FSA1, NST1
     1      ,ORB1, IFI1,  (COEF(I,II),I=1,NORD)
     +                                 , (NDP1(I),I=1,NST1)
       K = 0
           DO 500 I = 1,NST1
               NDPP    = NDP1(I)
               IF (NDPP.EQ.0) GO TO 500
               NTMM    = NTM1(I)
               K       = K + 1
               ISTU(1,K) = ISTN(I,1)
               WRITE (IDEV) IST1(I) , ALK1(I) , PRS1(I) , TDR1(I)
     +                    , TWT1(I) , IPS1(I) , FRQ1(I) , NTMM
     +                    , (DOP1(J,I),J=1,NDPP) , (TIM1(J,I),J=1,NTMM)
  500      CONTINUE
C
      IF(II.EQ.1) GO TO 113
       CALL DDHHMM (ITIM , ISP1)
       NPSS = NPSS + 1
       ILIN = MOD(ILIN , 59) + 1
       IF (ILIN.EQ.2) WRITE (IPRT,45)
       WRITE (IPRT,55) NPSS ,    K , ISP1 , (ISTU(1,I),I=1,K)
113      CONTINUE
115      RETURN
CC
    5  FORMAT (    50H1PASS SAT  LAST-INJC  REF-LOKON SPAN FREQSAT FIT
     +  /18X      ,50HSTATION  ACT-LOKON  PRESS T-DRY T-WET DIR FREQOFF
     +            , 9H#DOP #TIM,5X,5HFILE#,I2,/)
   15  FORMAT (1H ,/2I4,2(I5,2(1H/,I2)),I4,F9.1,I3)
   25  FORMAT (1H ,18X,A10,I4,2(1H/,I2),F7.0,2F6.1,I3,F9.2,I4,I5)
35     FORMAT (6H1FILE#,I1,7H  YEAR=,I2,6H  SAT#,I2,3H  [,A10,3H[
     +       , 7HFIGURE=,A10,9H  #COEFF=,I2,7H  DATE=,A10,8H  #STNS=,I2
     +    ,//,24H PASS  #STN  DAY HOR MIN,33X
     +       ,25HU S E D   S T A T I O N S)
   45  FORMAT (24H1PASS  #STN  DAY HOR MIN,33X
     +        ,25HU S E D   S T A T I O N S)
   55  FORMAT (2I5,2X,3I4,2X,15(1H/,A4))
   65  FORMAT ( 7H1 FILE#,I2,14H FOR TIME-SPAN,I4,1H/,I2,3H TO,I4,1H/,I2
     +        , 4H HAS,I5,9H PASSES -
     +     ,//,19H    STATION   #PASS,(/,2X,A10,I6))
C
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

