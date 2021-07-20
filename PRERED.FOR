Cv3.31  INTERFACE TO SUBROUTINE TIME (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
Cv3.31  INTERFACE TO SUBROUTINE DATE (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]	
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
      PROGRAM PRERED
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*40 FNAME, FBLNK
      CHARACTER*10 GEOBIN,NAME, ISTN, IBLK,DATTM, TIMME
      CHARACTER*4 ISRC1
      CHARACTER*1 IYES
      INTEGER*2 ITIMME(3), IDATE(3)
C
C   VERSION:  9/25/82                  GEODOP 5.03   (NOV81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   SUBPROGRAMS USED:
C    GEODOP SYSTEM: DDHHMM, MOD
C    FORTRAN:       INT
C
C   LIMITATIONS:  DIMENSIONED FOR A MAXIMUM OF 15 STATIONS AND 50
C    DOPPLER COUNTS PER PASS.  FOR MORE STATIONS, CHANGE DIMENSION
C    OF "ISTN" AND "NDOP", AND CHANGE VALUE OF "NSTN".  FOR MORE
C    COUNTS, CHANGE DIMENSION OF "DOPP" AND "TIM".
C
C
C        "PRERED    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - OCTOBER   /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         EXTERNAL ROUTINES  -DDHHMM  ,MOD  ,        ,
C                             EOF     ,INT     ,        ,
C
C        [PRERED[ READS NEW-PREDOP FILES (BINARY) FROM "TAPE5"
C
C                 IF PRINT FLAG = 0 , SUMMARIZE FILE ONLY.
C                 IF PRINT FLAG = 1 , SUMMARIZE PASSES & FILE.
C
       DIMENSION               ORBP(    7) , COFF(   30) , ISTN(   15)
     +         , INJP(    3) , LOKF(    3) , LOKN(    3) , LOKP(    3)
     +         , NDOP(   15) , DOPP(   50) , TIM(   50)
      DATA IBLK/ '          ' / , ITIM /  0 / , M /  0 / , NSTN / 15/
C  DEFAULT FILE NAMES
      DATA GEOBIN/'GEODOP.BIN'
     + /, FBLNK/'                                        '/ 
C  ISCN  CONSOLE LU change if required so
      ISCN=0
C  SYSTEM DEPENDENT CALL FOR TIME AND DATE, CHANGE IF REQUIRED SO
Cv3.31        CALL DATE(10,DATTM)
Cv3.31        CALL TIME(10,TIMME)
C VERSION 4.1 ONLY
      CALL GETTIM(ITIMME(1),ITIMME(2),ITIMME(3),ITSEC)
      CALL GETDAT(IDATE(3),IDATE(2),IDATE(1))
      WRITE(TIMME,'(1X,2(I2,'':''),I2)') ITIMME
      WRITE(DATTM,'(2(I2,''/''),I4)') IDATE
C
      WRITE(ISCN,1234) DATTM,TIMME
1234  FORMAT(' Program  P R E R E D - Geodopv utility (June 87)',4X,2A10
     +/ ' Lists (binary) Geodop files'/)
C--- I/O UNITS.
      IF98=5
      IF5=13
C--- SETUP I/O UNIT NUMBERS FOR IBM VERSION (ALSO SEE BELOW AND IN
C
C      IBM PC VERSION  by J. Kouba (June 87)
C
C     LUN  FORMAT   TYPE     DEFAULT NAME   DESCRIPTION         VARIABLE
C     ---  ------   ----     ------------   ------------------  -------- 
C      IF5  BINARY  INPUT    GEODOP.BIN     GEODOP INPUT FILE   GEOBIN
C-----------------------------------------------------------------
C
C
C
      IPRN=0
C
       WRITE(ISCN,'(A\)')' Extended printout  (Y/N) ? : ' 
       READ(ISCN,'(A)',END=2) IYES
       WRITE(ISCN,'(1H ,A1)') IYES
       IF(IYES.EQ.'Y'.OR.IYES.EQ.'y') IPRN=1
      IF(IPRN.EQ.1) THEN
       WRITE(ISCN,'(A\)')' START & END (ddd mm, ddd mm) : ' 
       READ(ISCN,*,END=2) I, J, K, L 
       WRITE(ISCN,'(1H ,2(I4,I3,2X))') I, J, K, L 
      ENDIF
C    
       LIM1 = I * 1440 + J * 60
       LIM2 = K * 1440 + L * 60
C
1       WRITE(ISCN,'(/A\)')' GEODOP input file(bin)-/E to end[GEODOP.BIN]
     +: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=2) FNAME
      IF(FNAME.EQ.'/E'.OR.FNAME.EQ.'/e') GO TO 2
      IF(FNAME.EQ.FBLNK) FNAME= GEOBIN
      WRITE(ISCN,'(1H ,(A))') FNAME
         OPEN(IF5,FILE=FNAME,STATUS='OLD',FORM='BINARY',ERR=11,
     +   IOSTAT=IERROR)
               J = 0
               L = 2
   10          M = M + 1
               READ(IF5,END=22) NAME, IORD, DATTM, ISTN1, ISRC1
      IF5EOF=0
      GO TO 24
   22 IF5EOF=1
   24 CONTINUE
               J = J + 1
               IF (J.GT.3)       STOP
               WRITE(ISCN,15)   M , NAME , DATTM , IORD
C--- MULTIPLE FILE INPUT DISABLED FOR IBM VERSION.
C              IF (EOF(5).NE.0.) GO TO 10
               IF (IPRN.EQ.1)    WRITE(ISCN,25)
               N = IORD * 3
                   DO 150 J = 1,NSTN
                       ISTN(J) = IBLK
  150              CONTINUE
C
                   DO 200 J = 1,10000
                       READ(IF5,END=40) NAME,ISRC,ISAT,INJC, LOKR, LENG
     +                        , FSAT , NSTN , ORBP , IFIT
     +                        , (COFF(K),K=1,N) , (NDOP(K),K=1,NSTN)
C                      IF (EOF(5).NE.0.) GO TO 40
                       IF (J.EQ.1)       CALL DDHHMM (LOKR , LOKF)
                       IF (IPRN.NE.1)    GO TO 30
                       IF (LOKR.LT.LIM1.OR.LOKR.GT.LIM2) GO TO 30
                       ITIM = 1
                       CALL DDHHMM (INJC , INJP)
                       CALL DDHHMM (LOKR , LOKP)
                      WRITE(ISCN,335)  J , ISRC , ISAT , INJP , LOKP
     +                             , LENG , FSAT , IFIT
C
   30                      DO 300 K = 1,NSTN
                               NDPP = NDOP(K)
                               IF (NDPP.EQ.0) GO TO 300
                               READ(IF5) ISTN(K), ALOK , PRES , TDRY
     +                                , TWET    , IPAS , FRCV , NTMM
     +                             , (DOPP(L),L=1,NDPP)
     +                             , (TIM(L),L=1,NTMM)
                               IF (ITIM.NE.1) GO TO 300
                               CALL DDHHMM (IDINT(ALOK) , LOKN)
                               L = MOD(   L , 59) + 1
                               IF (L.EQ.2)    WRITE(ISCN,25)
                              WRITE(ISCN,445)ISTN(K), LOKN , PRES
     +                                     , TDRY    , TWET , IPAS
     +                                     , FRCV    , NDPP , NTMM
  300                      CONTINUE
  200                  ITIM = 0
   40          J = J - 1
               CALL DDHHMM (LOKR , LOKP)
               WRITE(ISCN,55) ISTN, LOKF , LOKP ,    J
           CLOSE(IF5)
       GO TO 1
 11   WRITE(ISCN,1122) FNAME, IERROR
 1122 FORMAT(' OPEN FILE ERROR FNAME: ',A20,' IOSTAT= ',I5)
      GO TO 2
   13 WRITE(ISCN,'(A)')' Format read error in PREDOP input file:' 
C  TERMINATE PROGRAM
    2 STOP
C
    5  FORMAT (3X,I2,2(I3,I2),4X,I1)
   15  FORMAT (1H1,/,2X,1H@,I2,3H> [,A10,2H[ ,A10,2H (,I2,1H))
   25  FORMAT ('1PASS     SOURCE  SAT  LAST-INJC  REF-LOKON SPAN FREQ SA
     +T FIT'/14X,'STATION  ACT-LOKON  PRESS T-DRY T-WET DIR FREQOFF #DOP
     + #TIM',/)
  335  FORMAT (1H ,/,I4,2X,A10,I5,2(I5,2(1H/,I2)),I4,F9.1,A1)
  445  FORMAT (1H ,11X,A10,I4,2(1H/,I2),F7.0,2F6.1,I3,F9.2,I4,I5)
   55  FORMAT (//,17H  STATION NAME = ,10(A10,1H,),/,17X,4(A10,1H,),A10
     +         ,/,17H  FIRST LOCKON = ,I4,2(1H/,I2)
     +         ,/,17H   LAST LOCKON = ,I4,2(1H/,I2)
     +         ,/,17H    # PASSES   = ,I4)
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

