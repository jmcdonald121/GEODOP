Cv3.31  INTERFACE TO SUBROUTINE TIME (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
Cv3.31  INTERFACE TO SUBROUTINE DATE (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]	
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
      PROGRAM NWLFIT
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*40 FNAME, FBLNK
      CHARACTER*10 EPHMB,EPHMA, NDESC, ID
      CHARACTER*4  ISRC
      CHARACTER*1  IYES
      INTEGER*2 ITIMME(3), IDATE(3)
C
C   SUBPROGRAMS USED:
C    GEODOP:        CHEBY, LESQF1, MINUT
C    FORTRAN:       DBLE
C
C
C        "NWLFIT    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - MARCH   /74.
C                     - LAST COMPILED - JANUARY /76.
C                     - REFERENCES    - D.E.WELLS .. SUBROUTINE LSA
C
C   [NWLFIT[ CONVERTS CODED NWL PRECISE
C            EPHEMERIS FILES ON [EPHM.ASC]
C            TO FITTED COEFFICIENTS FOR
C            [X[ [Y[ [Z[ IN BINARY ON [EPHM.BIN].
C
C
C   FOR PASS-BY-PASS LISTING ON PRINTER , USE A 1 
C   FOR MATRIX A" = A[ * SO ** 2 OUTPUT , USE A 2
C
C
C
       DIMENSION APRIME(10,10,3) , BASE(10,60) , COF(10,3) , FIRST(6)
     +         , FUNC(60,3) , IFORM(2,5) , RESD(60,3) , SO(3), ID(3)
     +         , XYZ(6, 30) , TIMM(30)
      DIMENSION FMT(4,2)
       EQUIVALENCE (FIRST(1),XYZ(1,1))
      DATA EPHMB/'EPHM.BIN  '/,EPHMA/'EPHM.ASC  '/,
     + FBLNK/'                                        '/ 
     + ,ISRC/'PREC'/,IYES/'Y'/
      DATA IPRIN/0/,M/10/,IDERV/0/,WT/1.D0/,IFMT/1/, IDD/0/
C  SYSTEM DEPENDENT CALL FOR TIME & DATE (change if requred so)
Cv3.31        CALL DATE(10,ID(2))
Cv3.31        CALL TIME(10,ID(3))
C VERSION 4.1 ONLY
      CALL GETTIM(ITIMME(1),ITIMME(2),ITIMME(3),ITSEC)
      CALL GETDAT(IDATE(3),IDATE(2),IDATE(1))
      WRITE(ID(3),'(1X,2(I2,'':''),I2)') ITIMME
      WRITE(ID(2),'(2(I2,''/''),I4)') IDATE
C
C
C      IBM PC VERSION  by J. Kouba (June 87)
C
C   I/O UNIT NUMBER SETUP.
C
C  IF99 - OUTPUT LU (SET TO SCREEN, CHANGE OR USE PIPING FOR FILE OUTPUT)
      IF99=0
      IF2=10
      IF3=11
C  ISCN - SCREEN INPUT
      ISCN=0
C
C     LUN  FORMAT   TYPE     DEFAULT NAME   DESCRIPTION         VARIABLE
C     ---  ------   ----     ------------   ------------------  -------- 
C      IF2  ASCII   INPUT    EPHM.ASC       CODED EPHEMERIS     EPHMA
C      IF3  BINARY  OUTPUT   EPHM.BIN       FITTED EPHEMERIS    EPHMB
C-----------------------------------------------------------------
C
      WRITE(ISCN,1234) ID(2), ID(3)
1234  FORMAT(' Program  N W L F I T  - Geodopv utility (Sep 87)',4X,2A10
     +/ ' Fits coded ephemeris in 9 or 6 digit (NWL) format'/)
C
      WRITE(ISCN,'(A\)')' PRINTOUT ? (0-short; 1-pass by pass; 2-var-cov
     +): ' 
      READ(ISCN,*,END=1211) IPRIN
      WRITE(ISCN,'(I3)') IPRIN
      WRITE(ISCN,'(A\)')' DERIVATIVE fitting? (Y/N): ' 
      READ(ISCN,'(A)',END=1211) IYES
      IF(IYES.EQ.'N'.OR.IYES.EQ.'n') THEN
      IDERV=1
      ELSE
      IYES='Y'
      ENDIF
      WRITE(ISCN,'(1X,A1)') IYES
C
1112  WRITE(ISCN,'(A\)')' INPUT coded eph.file(/E to end)[  EPHM.ASC]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=1211) FNAME
      IF(FNAME.EQ.'/E'.OR.FNAME.EQ.'/e') GO TO 1211
      IF(FNAME.EQ.FBLNK) FNAME= EPHMA 
      WRITE(ISCN,'(1X,(A))') FNAME
         OPEN(IF2,FILE=FNAME,STATUS='OLD',ERR=1111,IOSTAT=IERROR)
C
      WRITE(ISCN,'(A\)')' OUTPUT Fitted ephemeris        [  EPHM.BIN]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=1211) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= EPHMB
      WRITE(ISCN,'(1X,(A))') FNAME
         OPEN(IF3,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY',ERR=1111,
     +   IOSTAT=IERROR)
C
      ID(1)= FNAME
 10   CONTINUE
C      IF (EOF(98).NE.0.) STOP
      GO TO 8
    6 STOP
    8 CONTINUE
       IF(IFMT.EQ.0) IFMT=1
   11 GO TO (110,120), IFMT
  110 READ(IF2,901,END=14) IYER,IDAY,IHOR,IMIN,FIRST,ISAT
C      IF (EOF(2).NE.0.) STOP
      GO TO 16
  120 READ(IF2,902,END=14) IYER,IDAY,IHOR,IMIN,FIRST,ISAT
      GO TO 16
   14 STOP
   16 CONTINUE
      IF(IYER.LE.0)GOTO11
       IDERV = IDERV + 1
       IF (IDERV.NE.1) IDERV = 2
      IF(M.EQ.0) M=10
      IF(WT.EQ.0.D0) WT= 1.D0
  12  CONTINUE
      WRITE(IF3) IYER,ISAT,IDAY,NDESC,IDD,ISRC,M
      WRITE(IF99,25) IF , IYER , ISAT , IDAY , ID , M , IDERV
       I1    = IDAY
       I2    = IHOR
       I3    = IMIN
       ILINE =    2
       IPASS =    0
       K     =    1
C
   20      DO 100 I = 1,6
               FIRST(I) = XYZ(I,K)
  100      CONTINUE
       II   = IDAY
       JJ   = IHOR
       KK   = IMIN
       LAST = MINUT(II , JJ , KK)
C
           DO 200 K = 2,30
         KKK= K
1000    CONTINUE
      EOF2=0
      GO TO (210,220), IFMT
  210 READ(IF2,901,END=22)IYER,IDAY,IHOR,IMIN,(XYZ(I,K),I=1,6),NSAT
C              EOF2 = EOF(2)
      GO TO 24
  220 READ(IF2,902,END=22)IYER,IDAY,IHOR,IMIN,(XYZ(I,K),I=1,6),NSAT
      GO TO 24
   22 EOF2=1
   24 CONTINUE
      IF(IYER.EQ.0.AND.EOF2.EQ.0)  GO TO 1000
       IF(NSAT.NE.ISAT)go to 40
               IF (EOF2) 40,30,40
C
   30          JAST = MINUT(IDAY , IHOR , IMIN) - LAST
               IF (JAST.GT.30) GO TO 40
               IF ((JAST - K + 2).LE.0) GO TO 40
               TIMM(K) =DBLE(JAST)
  200      CONTINUE
C
  40   L= KKK-1
      IF(L.LT.10) GO TO 60
C--- ARRAY ELEMENT OF "TIMM" NOT SPECIFIED IN CDC VERSION.
      TIMM(1)= 0.D0
       ARG  = 2.D0 / TIMM(L)
           DO 300 I = 1,L
               N   = I + L
               PHI = ARG * TIMM(I) - 1.D0
               CALL CHEBY(BASE(1,I) , BASE(1,N) , IDERV , M , PHI , ARG)
      DO 310 J=1,M
310     BASE(J,N)= BASE(J,N)* WT
                   DO 300 J = 1,3
                       FUNC(I,J) = XYZ(J  ,I) *  1000.D0
                      FUNC(N,J)= XYZ(J+3,I)*   60000.D0 *WT
  300      CONTINUE
C
       N = N / IDERV
           DO 400 I = 1,3
               CALL LESQF1 (APRIME(1,1,I) , BASE , COF(1,I) , FUNC(1,I)
     +                    , M , 10 , N , RESD(1,I) , SO(I))
  400      CONTINUE
C
      LL= TIMM(L)     + 1
      WRITE(IF3)LAST,LL,((COF(J,I),J=1,M),I=1,3),ISAT,IYER
      WRITE(IF99,35) ILINE, LL, II, JJ, KK,ISAT, SO
       IF (IPRIN.LT.1) GO TO 50
C
           DO 500 I = 1,L
               N = I + L
             IF (IDERV.EQ.1) WRITE (IF99,45) FUNC(I,1) , RESD(I,1) ,
     +                                       FUNC(I,2) , RESD(I,2) ,
     +                                       FUNC(I,3) , RESD(I,3) ,
     +                                       FUNC(N,1) , RESD(N,1) ,
     +                                       FUNC(N,2) , RESD(N,2) ,
     +                                       FUNC(N,3) , RESD(N,3)
             IF (IDERV.EQ.2) WRITE (IF99,55) FUNC(I,1) , RESD(I,1) ,
     +                                       FUNC(I,2) , RESD(I,2) ,
     +                                       FUNC(I,3) , RESD(I,3) ,
     +                                       FUNC(N,1) , FUNC(N,2) ,
     +                                       FUNC(N,3)
  500      CONTINUE
       WRITE (IF99,85)
       IF (IPRIN.NE.2) GO TO 50
C
           DO 550 I = 1,3
               WRITE (IF99,65) (COF(J,I),J=1,M)
  550      CONTINUE
       WRITE (IF99,85)
C
           DO 600 I = 1,M
      MFMT=(M-I)/2+1
      GO TO (311,320,330,340,350), MFMT
  311 WRITE(IF99,911) ((APRIME(I,J,N),J=I,M,2),N=1,3)
      GO TO 390
  320 WRITE(IF99,912) ((APRIME(I,J,N),J=I,M,2),N=1,3)
      GO TO 390
  330 WRITE(IF99,913) ((APRIME(I,J,N),J=I,M,2),N=1,3)
      GO TO 390
  340 WRITE(IF99,914) ((APRIME(I,J,N),J=I,M,2),N=1,3)
      GO TO 390
  350 WRITE(IF99,915) ((APRIME(I,J,N),J=I,M,2),N=1,3)
  390 CONTINUE
  600      CONTINUE
C
   50  IPASS = IPASS + 1
C
   60  ILINE = ILINE + L
       ISAT=NSAT
       IF (EOF2) 70 , 20 , 70
C
   70  ENDFILE IF3
       ILINE = ILINE - 2
       IWORD = IPASS * (M * 3 + 2) + 7
      WRITE(IF99,75) ILINE,IPASS,IWORD,I1,I2,I3,II,JJ,KK
C
       IF = IF + 1
C      IF(NSAT.EQ.ISAT)GOTO10
C      IF(NSAT.EQ.0) GO TO 10
C      ISAT=NSAT
      GOTO 1112
1111  WRITE(ISCN,1122) FNAME, IERROR
 1122 FORMAT(' OPEN FILE ERROR FNAME: ',A20,' IOSTAT= ',I5)
      GO TO 1211
1311  WRITE(ISCN,'(A)')' Format read error in EPHM input file:' 
C  TERMINATE PROGRAM
1211  STOP  
C
5     FORMAT(3A10,9X,I1,8X,I2,2(9X,I1),8X,F2.0)
C  15  FORMAT (I2,1X,I3,2(1X,I2),3F10.3,3F10.6,5X,I3)
   25  FORMAT (1H1,/,1X,1H@,I2,1H>,3X,7HYEAR = ,I2,3X,6HSAT = ,I2,3X,
     1 6HDAY = ,I3,3X,1H(,3A10,1H),/2X,8HORDER = ,I2,3X,
     $        13HDERIVATIVE = ,I1,/)
   35  FORMAT (/,2X,5HCARD=,I5,8H  LINES=,I2,6H  DAY=,I3,7H  TIME=,I2,
     $              1H/,I2,6H  SAT=,I3,6H   S0=,3F7.2,/)
   45  FORMAT (3(F12.0,F5.1),3X,3(F12.1,F5.1))
   55  FORMAT (3(F12.0,F5.1),3X,3(F12.1,5X))
   65  FORMAT (5X,10F13.2)
   75  FORMAT (////,1X,26H**  TOTAL LINES  INPUT  = ,I6,
     $            /,1X,26H**  TOTAL PASSES INPUT  = ,I6,
     $            /,1X,26H**  TOTAL WORDS  OUTPUT = ,I6,
     $           //,1X,22H**    STARTING TIME =  ,2I3,1H/,I2,
     $            /,1X,22H**    ENDING   TIME =  ,2I3,1H/,I2,//)
   85  FORMAT (1H )
C
C   IBM FORMATS (FROM ARRAYS OF CDC VERSION).
C
C   "FMT":
  901 FORMAT(I2,1X,I3,2(1X,I2),3F13.6,3F13.9,5X,I3)
  902 FORMAT(I2,1X,I3,2(1X,I2),3F10.3,3F10.6,5X,I3)
C   "IFORM":
  911 FORMAT(3(35X,F8.5))
  912 FORMAT(3(27X,2F8.5))
  913 FORMAT(3(19X,3F8.5))
  914 FORMAT(3(11X,4F8.5))
  915 FORMAT(3(3X,5F8.5))
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
       SUBROUTINE LESQF1 (A , BASE , C , F , M , MAX , N , R , S)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C        "LESQF1    " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - FEBRUARY  /74.
C                     - LAST COMPILED - NOVEMBER /75.
C                     - REFERENCES    - D.E.WELLS .. SUBROUTINE LSA
C
C         VARIABLES USED     -MAX     ,        ,        ,
C
C         VARIABLES ALTERED  -A       ,        ,        ,
C
C         VARIABLES RETURNED -*NONE*  ,        ,        ,
C
C         EXTERNAL ROUTINES  -*NONE*
C
C         I/O DEVICES        -*NONE*  ,        ,        ,
C
C
C        [LESQF1[ DOES A LEAST-SQUARES FIT TO THE "N" POINTS OF FUNCTION
C                 FOR A GIVEN DESIGN MATRIX "BASE" . THE "M"TH ORDER COE
C                 "C" , THE "N" RESIDUALS "R" , THE NORMALIZED [A[ MATRI
C                 AND THE STANDARD DEVIATION "S" ARE RETURNED TO THE CAL
C
      DIMENSION A(10,10), B(10), BASE(MAX,60), C(M), F(N), R(N)
C
           DO 100 I = 1,MAX
                   DO 100 J = 1,MAX
                       A(I,J) = 0.D0
  100          CONTINUE
C
           DO 200 I = 1,M
               Y = 0.D0
                   DO 300 J = 1,N
                       X = BASE(I,J)
                       Y = Y + F(J) * X
                           DO 300 K = 1,M
                               A(K,I) = A(K,I) + BASE(K,J) * X
  300                  CONTINUE
               B(I) = Y
  200      CONTINUE
C
       CALL TRINN (A , M , 2 , MAX)
C
           DO 400 I = 1,M
               X = 0.D0
                   DO 500 J = 1,M
                       X = X + A(I,J) * B(J)
  500              CONTINUE
               C(I) = X
  400      CONTINUE
C
       X = 0.D0
           DO 600 I = 1,N
               Y = F(I)
                   DO 700 J = 1,M
                       Y = Y - BASE(J,I) * C(J)
  700              CONTINUE
               R(I) = Y
               X = X + Y * Y
  600      CONTINUE
C
       X = X /DBLE(N - M)
           DO 800 I = 1,M
                   DO 800 K = 1,M
                       A(I,K) = A(I,K) * X
  800          CONTINUE
C
       S =DSQRT(X)
       RETURN
       END

