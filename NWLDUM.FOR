Cv3.31  INTERFACE TO SUBROUTINE TIME (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
Cv3.31  INTERFACE TO SUBROUTINE DATE (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]	
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
      PROGRAM NWLDUM
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*40 FNAME, FBLNK
      CHARACTER*10 EPHMB, NDESC,DATTM, TIMME
      CHARACTER*4 ISRC,SARC,UPDT,PREC
      INTEGER*2 ITIMME(3), IDATE(3)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   NOTE: CHANGES FROM CDC VERSION ARE:
C   1. CHANGED FROM MAIN PROGRAM TO SUBROUTINE.
C   2. CDC "EOF"S REPLACED BY IBM "END="S.
C   3. UNIT NUMBERS REPLACED BY VARIABLES AND CHANGED.
C
C   SUBPROGRAMS USED:
C    FORTRAN:       FLOAT, INT
C
C
C        "NWLDUM    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - MARCH      /74.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - *NONE*
C
C         EXTERNAL ROUTINES  -EOF     ,FLOAT   ,INT     ,
C
C
C
       DIMENSION  COF(10,3), ORB(8), VARORB(21)
       DATA IF / 0 /
      DATA EPHMB/'EPHM.BIN  '/,
     + FBLNK/'                                        '/ 
     + ,SARC/'SARC'/,UPDT/'UPDT'/,PREC/'PREC'/
C
C   I/O UNIT NUMBERS.
      IF3=3
      ISCN=0
C      IBM PC VERSION  by J. Kouba (June 87)
C
C     LUN  FORMAT   TYPE     DEFAULT NAME   DESCRIPTION         VARIABLE
C     ---  ------   ----     ------------   ------------------  -------- 
C      IF3  BINARY  INPUT    EPHM.BIN       FITTED EPHEMERIS    EPHMB
C-----------------------------------------------------------------
C
Cv3.31        CALL DATE(10,DATTM)
Cv3.31        CALL TIME(10,TIMME)
C VERSION 4.1 ONLY
      CALL GETTIM(ITIMME(1),ITIMME(2),ITIMME(3),ITSEC)
      CALL GETDAT(IDATE(3),IDATE(2),IDATE(1))
      WRITE(TIMME,'(1X,2(I2,'':''),I2)') ITIMME
      WRITE(DATTM,'(2(I2,''/''),I4)') IDATE
C
      WRITE(ISCN,1234) DATTM,TIMME
1234  FORMAT(' Program  N W L D U M  - Geodopv utility (Sep 87)',4X,2A10
     +/ ' Lists fitted (binary) ephemeris files'/)
C
C
C   GET NUMBER OF FILES TO BE PROCESSED (NFILE).
C
      IF=0
C
10    WRITE(ISCN,'(A\)')' Fitted ephemeris; use /E to end [EPHM.BIN]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=12) FNAME
      IF(FNAME.EQ.'/E'.OR.FNAME.EQ.'/e') GO TO 12
      IF(FNAME.EQ.FBLNK) FNAME= EPHMB
         OPEN(IF3,FILE=FNAME,STATUS='OLD',FORM='BINARY',ERR=11,
     +   IOSTAT=IERROR)
      IF= IF + 1
C
      READ(IF3,END=12) IYER , ISAT , IDAY ,NDESC, ID , ISRC, M
      WRITE(ISCN,5) IF ,NDESC, ID ,ISRC, IYER , ISAT , IDAY , M
       I4 = 0
C
           DO 100 I = 1,1000
      IF(ISRC.EQ.SARC) READ(IF3,END=20) LAST,L,((COF(K,J),K=1,M),J=1,3)
     + ,ISAT, ORB, NOTE
      IF(ISRC.EQ.UPDT) READ(IF3,END=20) LAST,L,((COF(K,J),K=1,M),J=1,3)
     + ,ISAT, NDESC, VARORB
      IF(ISRC.EQ.PREC) READ(IF3,END=20) LAST,L,((COF(K,J),K=1,M),J=1,3)
     + ,ISAT, IYER
C              IF (EOF(3).NE.0.) GO TO 20
               X  =DBLE(LAST) / 1440.D0
               I1 = IDINT(X)
               X  = (X -DBLE(I1)) * 24.D0
               I2 = IDINT(X)
               I3 = IDINT((X -DBLE(I2)) * 60.D0 + .5D0)
               I4 = I4 + L
  100          WRITE(ISCN,15) I , I1 , I2 , I3 , ISAT, L
C
  20  I=I-1
      I5= 7 + I*( 2+ M*3)
      WRITE(ISCN,25) I , I4 , I5
       CLOSE (IF3)
       GO TO 10
11    WRITE(ISCN,1122) FNAME, IERROR
 1122 FORMAT(' OPEN FILE ERROR FNAME: ',A20,' IOSTAT= ',I5)
      GO TO 12
   13 WRITE(ISCN,'(A)')' Format read error in EPHM input file:' 
C  TERMINATE PROGRAM
12    STOP  
  901 FORMAT(3I5)
C  901 FORMAT(I5)
    5  FORMAT (1H1,//,2X,1H@,I2,1H>,5X,1H[,3A10,1H[,5X,7HYEAR = ,I2
     +               ,5X,6HSAT = ,I2,5X,6HDAY = ,I3,5X,6HORD = ,I2
     +            ,//,1X,24HPASS  DAY HR MN   SAT LN,/)
   15  FORMAT (2I5,2I3,I6,I3)
   25  FORMAT (//,5X,10H# PASSES =,I6
     +         ,/,5X,10H# LINES  =,I6
     +         ,/,5X,10H# WORDS  =,I6)
       END

