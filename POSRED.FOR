Cv3.31  INTERFACE TO SUBROUTINE TIME (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
Cv3.31  INTERFACE TO SUBROUTINE DATE (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]	
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
      PROGRAM POSRED
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*40 FNAME, FBLNK
      CHARACTER*10 CONOUA,CONINP,NAME, ISTN, IBLK,DATTM, TIMME
      CHARACTER*4 ISRC,HEADER
      CHARACTER*1 IYES
      INTEGER*2 ITIMME(3), IDATE(3)
C
C   VERSION: 01/10/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   PURPOSE:
C
C    POSRED DOES THE FOLLOWING:
C     1. INPUTS GEODOP (OUTPUT) CONTINUATION FILE FROM "TAPE9".
C     2. PRINTS A SUMMARY OF THAT "TAPE9" INFORMATION.
C     3. OUTPUTS GEODOP CONTINUATION CARD (IMAGES) ONTO "TAPE1".
C
C   SUBPROGRAMS USED:
C    GEODOP SYSTEM: DDHHMM
C    FORTRAN:       INT
C   LIMITATION:  DIMENSIONED FOR MAXIMUM OF 15 STATIONS.
C
       DIMENSION NDLY(15) , NAME(15) , GEOCO(3,15) , X(3,15) , PX(45,45)
       DIMENSION ALOK(2) , ISTR(3) , ISTO(3)
      EQUIVALENCE (ISRC,HEADER)
       DATA DEG / 57.29577951D0 /
C
C--- I/O UNITS.
      IF1=1
      IF99=0
      IF9=9
C  DEFAULT FILE NAMES
      DATA CONINP/'CONINP.BIN'/,CONOUA/'NUL.ASC   '/
     +, FBLNK/'                                        '/ 
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
1234  FORMAT(' Program  P O S R E D - Geodopv utility (Jan. 88)',4X,2A10
     +/ ' Lists Continuation (bin) Geodop files'/)
C
C      IBM PC VERSION  by J. Kouba (January 88)
C
C     LUN  FORMAT   TYPE     DEFAULT NAME   DESCRIPTION         VARIABLE
C     ---  ------   ----     ------------   ------------------  -------- 
C      IF9  BINARY  INPUT    CONINP.BIN     GEODOP CONT. FILE   CONINP
C      IF1  BINARY  ASCII    NUL.ASC        GEODOP CONT. FILE   CONOUA
C-----------------------------------------------------------------
C
1       WRITE(ISCN,'(/A\)')' GEODOP CONT. file(bin)-/E to end[CONINP.BIN]
     +: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=100) FNAME
      IF(FNAME.EQ.'/E'.OR.FNAME.EQ.'/e') GO TO 100
      IF(FNAME.EQ.FBLNK) FNAME= CONINP
      WRITE(ISCN,'(1H ,(A))') FNAME
C
         OPEN(IF9,FILE=FNAME,STATUS='OLD',FORM='BINARY',ERR=11,
     +   IOSTAT=IERROR)
C
       WRITE(ISCN,'(/A\)')' GEODOP CONT. file (ascii) OUTPUT [NUL.ASC   ]
     +: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=100) FNAME
      IF(FNAME.EQ.'/E'.OR.FNAME.EQ.'/e') GO TO 100
      IF(FNAME.EQ.FBLNK) FNAME= CONOUA
      WRITE(ISCN,'(1H ,(A))') FNAME
C
         OPEN(IF1,FILE=FNAME,STATUS='NEW',ERR=11,
     +   IOSTAT=IERROR)
C
      READ(IF9,END=100) NPTS , MACOL , (NDLY(I),I=1,NPTS)
     +                , (NAME(NDLY(J)) , (GEOCO(I,NDLY(J)) ,X(I,NDLY(J))
     +                , I=1,3) , J=1,NPTS) , ((PX(I,J),I=J,MACOL)
     +                , J=1,MACOL) , MDFM , VPV , IPASS , IELEV , NDFRJ
     +                , NREJT , IVRJ , HEADER , ALOK
      GO TO 20
 11   WRITE(ISCN,1122) FNAME, IERROR
 1122 FORMAT(' OPEN FILE ERROR FNAME: ',A20,' IOSTAT= ',I5)
      GO TO 100
   13 WRITE(ISCN,'(A)')' Format read error in PREDOP input file:' 
C  TERMINATE PROGRAM
 100  CLOSE (IF1)
      STOP
C
   20 N=N+1
       CALL DDHHMM (IDINT(ALOK(1)) , ISTR)
       CALL DDHHMM (IDINT(ALOK(2)) , ISTO)
       WRITE(IF99,5) N , ISRC   , ISTR , ISTO
       WRITE(IF99,15) NPTS, MDFM , VPV , IPASS , IELEV , NDFRJ , NREJT
     +             , IVRJ
       WRITE(IF1,17) NPTS , IPASS , NREJT , NDFRJ , IELEV , IVRJ , MDFM
     +             , VPV
           DO 200 K = 1,MACOL
              WRITE(IF99,25) (PX(I,K),I=K,MACOL)
  200      CONTINUE
       WRITE(IF99,35)
           DO 300 K = 1,NPTS
               J = NDLY(K)
               Y = GEOCO(1,J) * DEG
               Z = GEOCO(2,J) * DEG
               WRITE(IF99,45) NAME(J) , J , Y , Z , GEOCO(3,J)
     +                     , (X(I,J),I=1,3)
               IX = IDINT(X(1,J) * 10.D0 + .5D0)
               IY = IDINT(X(2,J) * 10.D0 + .5D0)
               IZ = IDINT(X(3,J) * 10.D0 + .5D0)
               WRITE(IF1,47) NAME(J) , IX , IY , IZ
               WRITE(IF1,49) NAME(J) , (X(I,J),I=1,3)
  300      CONTINUE
       WRITE(IF1,57) (PX(I,I),I=1,MACOL)
       K = MACOL - 1
       WRITE(IF1,57) ((PX(I+1,J),I=J,K),J=1,K)
       CLOSE (IF9)
       GO TO 1
C
    5  FORMAT ('1',//,'   FILE # ',I2,'   EPHEMERIS = ',A10,'   START ='
     +        ,I4,2('/',I2),'   STOP =',I4,2('/',I2))
   15  FORMAT (/,'   # ACTIVE STNS = ',I2,'   DF = ',I6,'   VPV = ',F10.
     +2       ,'   # PASSES = ',I4,'   REJECTIONS... ',I4,'(ELV)',I4
     +        ,'(DF)',I4,'(99%)',I5,'(DOP)',//,'    WGT. COEF. MATRIX-')
   17  FORMAT (7I10,F10.3)
C  25  FORMAT (15E9.3,/,15E9.3,/,15E9.3)
   25 FORMAT(1X,12E11.3/1X,12E11.3/1X,12E11.3)
   35  FORMAT (/,'  STATION     #    LATITUDE    LONGITUDE   HEIGHT'
     +        ,'        X            Y            Z',/,1X,92('-'))
   45  FORMAT (2X,A10,3X,I2,3X,2(F10.6,2X),F7.2,1X,3(2X,F11.2))
   47  FORMAT (A10,43X3I9)
   49  FORMAT (' 61 ',A7,8X3F15.4)
   57  FORMAT (6(2X,E11.5))
  901 FORMAT(I5)
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

