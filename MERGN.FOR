Cv3.31  INTERFACE TO SUBROUTINE TIME (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
Cv3.31  INTERFACE TO SUBROUTINE DATE (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]	
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
      PROGRAM MERGN
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*40 FNAME, FBLNK
      CHARACTER*10 MERGB,GEODB, NDESC,DATTM, TIMME ,NAME ,DAYT
     + ,EPHMB, MERQB, MERS1, MERS2, ISTN, HEDR,NIDN, ISTP, ISSN
      CHARACTER*4 ISRC, ISRP, NOTE, INWLP
      CHARACTER*1 IYES
      INTEGER*2 ITIMME(3), IDATE(3)
C
C   VERSION: 03/20/83                  GEODOP 5.09 (OCT82)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C        "MERGE     " - AUTHOR        - P.G. LAWNIKANIS
C                     - WRITTEN ON    - SEPTEMBER /74.
C                     - LAST COMPILED - APRIL   1975.
C                     - REFERENCES    - *NONE*
C
C         EXTERNAL ROUTINES  -FILIO   ,DDHHMM  ,MINUT   ,
C                             DATE    ,EOF              ,IABS
C                             INT     ,        ,        ,
C
C         I/O DEVICES        -INPT = SOURCE PREDOP-LIKE DATA
C                             INWL = BINARY FITTED-NWL-EPHEMERIS
C                             IOUQ = SCRATCH MERGED DATA
C                             IOUT = MERGED OUTPUT DATA
C                             IPRT = LINE PRINTER
C                             IRED = CARD READER
C                             ISCQ = INTERMEDIATE SCRATCH MERGED DATA
C                             ISCR = INTERMEDIATE SCRATCH MERGED DATA
C
C
C        [MERGE[  COMBINES PREDOP OUTPUT FILES ON "TAPE4"
C                INTO 1 MULTI-STATION INPUT FILE ON "TAPE5" ,
C                OR MERGES NWL FITTED COEFFICIENTS ON "TAPE3"
C                WITH 1 GEODOP INPUT FILE ON "TAPE4" GIVING "TAPE5".
C
C                 -ONLY DATA COMMON TO THE INPUT TIME SPAN WILL BE PROCE
C                 -THE COEFFICIENTS OF A DATA SET WITH FITTING TROUBLE
C                  WILL BE REPLACED WHEREVER POSSIBLE.
C                 -A PASS-BY-PASS LISTING OF "TAPE5" IS SELECTED BY
C                  SETTING THE PRINT FLAG (COL.20) TO "1".
C
C                 THE # OF INPUT FILES , TIME-SPAN , AND PRINT FLAG
C                  ARE INPUT ON A SINGLE DATA CARD.
C                 THE NWL FLAG (COL.25) IS SET TO "1" FOR NWL-MERGING.
C
C        NOTE ... [MERGE[ MAY BE USED TO EDIT A DATA-SET INTO "BLOCK"S
C                 (N FILES) BY SUPPLYING N OPTION CARDS WITH THE DESIRED
C                 TIME SPANS.
C
C
       COMMON HEDR , NAME , ISRC , ISAT , INJC , LOKR , LENG , FSAT
     +      , NSTN , ORBP , IFIT , COEF , ISTN , ALOK , PRES , TDRY
     +      , TWET , IPAS , FRCV , NDOP , NTIM , DOPP , TME , NORD
     +      , NYER , NSAT , NDAY , NIDN , IORD , ISRP , MORD
       COMMON /DAT/  DAYT ,    I , IPRN , NSTI , IEOF , ILIM , NSNN
     +             , ISSN , TIMN , NOTE , NPAS , NPSS , KSAT , ALKMX
     +             , ITMN ,TMINM
       COMMON /DEV/  INPT , INWL , IOUQ , IOUT , IPRT , IRED , ISCQ
     +             , ISCR , ISWI , ISWJ , ISTO , NFIL , IONE , ITWO
     +             , ITTO
       DIMENSION  NAME(    2)  , ISRC(      2) 
     +         , ISAT(      2) , INJC(      2) , LOKR(      2)
     +         , LENG(      2) , FSAT(      2) , NSTN(      2)
     +         , ORBP(    7,2) , IFIT(      2) , COEF(   30,2)
     +         , ISTN(   15,2) , ALOK(   15,2) , PRES(   15,2)
     +         , TDRY(   15,2) , TWET(   15,2) , IPAS(   15,2)
     +         , FRCV(   15,2) , NDOP(   15,2) , NTIM(   15,2)
     +         , DOPP(50,15,2) , TME( 50,15,2) , KSAT(      6)
       DIMENSION ILIM(      6) , TIMN(     15) , ISSN(     15)
     +         , NOTE(   4,15) , ITIM(      4) , ISPN(      6)
     +         , ALPR(     15) , ISTP(     15) , NPAS(     15)
       EQUIVALENCE (ITIM(1),LOKR(1)) , (ITPR,LOKR(2)) , (ITSC,LOKR(1))
     +           , (ISPR,ISAT(2)) , (ISSC,ISAT(1)) , (IFPR,IFIT(2))
     +           , (IFSC,IFIT(1)) , (INJP,INJC(2)) , (INJS,INJC(1))
     +           ,(ALPR(1),ALOK(1,2)),(ISTP(1),ISTN(1,2)),(NSPR,NSTN(2))
     +           , (ILM1,ILIM(1)) , (ILM2,ILIM(2))
       DATA NSNM /  0 /
      DATA INWLP/'PREC'/, EPHMB/'EPHM.BIN  '/, MERGB/'MERGE.BIN '/,
     + GEODB/'GEODOP.BIN'/,MERQB/'MERGQ.BIN '/,MERS1/'MERGE.SC1 '/,
     + MERS2/'MERGE.SC2 '/
     + FBLNK/'                                        '/ 
C  DEFAULT VALUES
      DATA NUMF/15/, MOVE/0/ 
C
C
C   NEW DEVICE UNIT NUMBERS FOR IBM VERSION.
C
C     ISCR=1             1      TEMP. SCRATCH MERGED DATA (BINARY)
C     ISCQ=2             2      TEMP. SCRATCH MERGED DATA (BINARY)
C     IRED=5        WAS 98      CARD READER (INPUT)
C     IPRT=6            99      LINE PRINTER (OUTPUT)
C     INWL=11            3      NWL FITTED COEFFICIENTS (INPUT-BINARY)
C     INPT=12            4      PREDOP OUTPUT (INPUT-BINARY)
C     IOUT=13            5      GEODOP INPUT  (OUTPUT-BINARY)
C     IOUQ=14            6      SCRATCH MERGED DATA (BINARY)
C
C   UNIT NUMBER SETUP.
C  ISCN - SCREEN LU CHANGE IF REQUIRED SO
      ISCN=0
C
      ISCR=1
      ISCQ=2
      IRED=ISCN
      IPRT=ISCN
      INWL=11
      INPT=12
      IOUT=13
      IOUQ=14
C
C      IBM PC VERSION  by J. Kouba (June 87)
C
C     LUN  FORMAT   TYPE     DEFAULT NAME   DESCRIPTION         VARIABLE
C     ---  ------   ----     ------------   ------------------  -------- 
C      ISCN         I/O      SRN:           SCREEN
C      IOUT BINARY  OUTPUT   MERGE.BIN      MERGED OUTPUT FILE  MERGB
C      INPT BINARY  INPUT    GEODOP.BIN     GEODOP FILE         GEODB
C      INWL BINARY  INPUT    EPHM.BIN       FITTED EPHEMERIS    EPHMB
C      IOUQ BINARY  OUTPUT   MERGQ.BIN      MERGED SCRATCHED F. MERQB
C      ISCR BINARY  SCRATCH  MERGE.SC1      SCRATCH TEMP FILE   MERS1
C      ISCQ BINARY  SCRATCH  MERGE.SC2      SCRATCH TEMP FILE   MERS2
C-----------------------------------------------------------------
C
Cv3.31        CALL DATE(10,DATTM)
Cv3.31        CALL TIME(10,TIMME)
C VERSION 4.1 ONLY
      CALL GETTIM(ITIMME(1),ITIMME(2),ITIMME(3),ITSEC)
      CALL GETDAT(IDATE(3),IDATE(2),IDATE(1))
      WRITE(TIMME,'(1X,2(I2,'':''),I2)') ITIMME
      WRITE(DATTM,'(2(I2,''/''),I4)') IDATE
      DAYT= DATTM
      WRITE(ISCN,1234) DATTM,TIMME
1234  FORMAT(' Program  M E R G E  - Geodopv utility (Sep 87)',4X,2A10
     +/ ' Merges, edits binary geodop files'/)
C
      WRITE(ISCN,'(A\)') ' MERGEd scratch file    [MERGQ.BIN ]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=11) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= MERQB 
      WRITE(IPRT,'(1X,(A))') FNAME
         OPEN(IOUQ,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY',ERR=11,
     + IOSTAT=IERROR)
C
      WRITE(ISCN,'(A\)') ' SCRATCH temp. file 1   [MERGE.SC1 ]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=11) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= MERS1 
      WRITE(IPRT,'(1X,(A))') FNAME
         OPEN(ISCR,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY',ERR=11,
     + IOSTAT=IERROR)
C
      WRITE(ISCN,'(A\)') ' SCRATCH temp. file 2   [MERGE.SC2 ]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=11) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= MERS2 
      WRITE(IPRT,'(1X,(A))') FNAME
         OPEN(ISCQ,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY',ERR=11,
     + IOSTAT=IERROR)
      WRITE(ISCN,'(A\)') ' MERGEd OUTPUT bin. file[MERGE.BIN ]: ' 
      FNAME=FBLNK
      READ(IRED,'(A)',END=11) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= MERGB 
      WRITE(IPRT,'(1X,(A)/)') FNAME
       MERGB= FNAME
C
      ALKMX=0
      IPRN=0
      MOVE=0
       WRITE(ISCN,'(A\)') ' Number of input (binary) files ?  ): '
      READ(IRED,*,END=400,ERR=13) NUMF
      WRITE(IPRT,'(I3)') NUMF
      WRITE(ISCN,'(A\)') ' START & END times (ddd hh, ddd hh ): '
      READ(IRED,*,END=400,ERR=13) (ILIM(J),J=3,6)
      WRITE(IPRT,'(2(I4,I3,1X))') (ILIM(J),J=3,6)
      WRITE(ISCN,'(A\)') ' Pass by pass printout ?       (Y/N): '
      READ(IRED,'(A)',END=400,ERR=13) IYES
      WRITE(IPRT,'(1X,(A))') IYES
      IF(IYES.EQ.'Y'.OR.IYES.EQ.'y') IPRN=1
      WRITE(ISCN,'(A\)') ' EPHEMERIS (precise) merge mode(Y/N): '
      READ(IRED,'(A)',END=400,ERR=13) IYES
      WRITE(IPRT,'(1X,(A))') IYES
      IF(IYES.EQ.'Y'.OR.IYES.EQ.'y') MOVE=1
       IF(MOVE.EQ.1) THEN
       IF(IRED.EQ.0)
     + WRITE(ISCN,'(A\)') ' FITTED ephemeris input [EPHM.BIN ]: ' 
      FNAME=FBLNK
      READ(IRED,'(A)',END=11) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= EPHMB 
      WRITE(IPRT,'(1X,(A)/)') FNAME
         OPEN(INWL,FILE=FNAME,STATUS='OLD',FORM='BINARY',ERR=11,
     + IOSTAT=IERROR)
       ENDIF
      WRITE(IRED,'(A\)') ' Sat. to be excluded(Use 0s to keep): '
      READ(ISCN,*,END=400,ERR=13) KSAT
      WRITE(IPRT,'(1X,6I3/)') KSAT
C
      IF(NUMF.EQ.0) NUMF= 1
C
C  IFUN=9 INITILIZE EVERYTHING
C
       CALL NFILIO (9 , ISWT , INPT)
       IF (MOVE.EQ.1)       GO TO 114
       WRITE (IPRT,15) DATTM , TIMME
C
           DO 100 I = 1,NUMF
      WRITE(ISCN,'(A\)')' Input GEODOP file       [GEODOP.BIN]: ' 
      FNAME=FBLNK
      READ(IRED,'(A)',END=101,ERR=13) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= GEODB
      WRITE(IPRT,'(1X,(A))') FNAME
         OPEN(INPT,FILE=FNAME,STATUS='OLD',FORM='BINARY',ERR=11,
     +   IOSTAT=IERROR)
C
C   IFUN=1 READ/WRITE HEADR
C
               CALL NFILIO (1 , I-NUMF , INPT)
               IF(IEOF.EQ.3) GO TO 401
               IF (I.EQ.1) GO TO 20
C
   10          ISWT = 1
               ISCT = 1
C
C  IFUN=2  READ PASS IN 2ND LOCATION
C
   20          CALL NFILIO (2 ,    2 , INPT)
      ITEM=IEOF+ISWT
               GO TO (30 , 50 , 90 , 22 , 22 , 110) , ITEM
   22          ISWT = 3
               GO TO (40 , 70) , ISCT
C
   30          ISWT = 2
       TMINM= 530000.
C
C   IFUN=2 READ PASS IN 1ST LOCATION
C
   40          CALL NFILIO (2 ,    1 , ISCQ)
               ISCT = 2
      ITEM=IEOF+ISWT
               GO TO (50 , 50 , 70 , 42 , 42 , 110) , ITEM
   42          ISWT = 3
               GO TO 90
C
   50          IDIF = ITSC - ITPR
               IF (IDIF / 16) 70 , 60 , 90
C
   60          IF (ISSC.EQ.ISPR) GO TO 80
               IF (IDIF.GT.0)    GO TO 90
C  IFUN=3  WRITE PASS
   70          CALL NFILIO (3 ,    1 , ISCR)
               GO TO 40
C
   80          IF (IFSC.LT.IFPR.OR.IFPR.EQ.IFSC.AND.INJS.GT.INJP) ISWT=1
               IF (IDIF.NE.0) ISWT = (IABS(IDIF) / IDIF + 3) / 2
C  IFUN=4 WRITE MERGED PASS
               CALL NFILIO (4 , ISWT , ISCR)
               GO TO 10
C
C  IFUN=3 WRITE PASS
   90          CALL NFILIO (3 ,    2 , ISCR)
               GO TO 20
C
  110          WRITE (IPRT,25)    I , ISCR , NPSS
               ISWT = NSTI - NSPR
                   DO 200 J = 1,NSPR
                       ISWT       = ISWT + 1
                       IF(ALPR(J).LE.ALKMX) GO TO 221
                       ALKMX=ALPR(J)
 221                   ITMP= TIMN(J)
                       CALL DDHHMM (ITMP , ISPN(1))
                       ITMP= ALPR(J)
                       CALL DDHHMM (ITMP , ISPN(4))
                       WRITE (IPRT,35) ISWT , ISTP(J) , NPAS(J)
     +                             , (ISPN(K),K=1,6) , (NOTE(K,J),K=1,4)
                       IF (NSNM.EQ.NSNN) GO TO 200
                       NSNM       = NSNM + 1
                       ISSN(NSNM) = ISTP(J)
  200      CONTINUE
C  IFUN=5 CLOSE/REWIND FILE
               CALL NFILIO (5 ,    1 , ISCR)
C
           CLOSE(INPT)
  100      CONTINUE
  101      WRITE (IPRT,45) NSNN , (   I , ISSN(I) , I=1,NSNN)
       I = 1
C
  114  ILIM(1) = MINUT (ILIM(3) , ILIM(4) , 0)
       ILIM(2) = MINUT (ILIM(5) , ILIM(6) , 0)
         OPEN(IOUT,FILE=MERGB,STATUS='UNKNOWN',FORM='BINARY',ERR=11,
     + IOSTAT=IERROR)
       IF (MOVE.EQ.1)       GO TO 120
       CALL NFILIO (1 ,    1 , IOUQ)
       IF(IEOF.EQ.3) GOTO 400
       CALL NFILIO (6 , IPRN , IOUQ)
       CALL NFILIO (5 ,    2 , IOUT)
       CLOSE(IOUT)
       GO TO 400
C
  120  WRITE (IPRT,55) DATTM , TIMME
C           DO 300 J = 1,NUMF
      WRITE(IRED,'(A\)')' Input GEODOP (binary) file     [GEODOP.BIN]: ' 
      FNAME=FBLNK
      READ(IRED,'(A)',END=13,ERR=11) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= GEODB
         OPEN(INPT,FILE=FNAME,STATUS='OLD',FORM='BINARY',ERR=11,
     +   IOSTAT=IERROR)
               CALL NFILIO (1 ,    0 , INPT)
               IF(IEOF.EQ.3) GOTO 401
C
  130          ISWT = 0
  140          ISWT = ISWT + 1
  150          CALL NFILIO (7 , ISWT , INWL)
               GO TO (160 , 180 , 150 , 300 , 300 , 300) , IEOF
C
  160          ISWT = 2
  170          CALL NFILIO (2 ,    1 , INPT)
               IF (IEOF.NE.0) GO TO 140
               IF(NSAT.LE.10.OR.NSAT.GT.80) GO TO 175
C              IF(MOVE.EQ.1.AND.ISSC.NE.NSAT) GO TO 170
C
 175           IF (ITSC.LT.ILM1) GO TO 170
               IF (ITSC.GT.ILM2) GO TO 140
180   IWT=1
      IF((ITPR-ITSC)/15) 150,181,190
 181      CONTINUE
      IF(ISSC.NE.NSAT) GO TO 190
C
C  PRECISE EPH MATCHED
      IWT=2
      ISRC(2)=ISRP
190      CALL NFILIO(8,IWT, IOUT)
      IF(IWT.EQ.2) GO TO 130
      GO TO 170
C
  300          CALL NFILIO (5 ,    2 , IOUT)
               CLOSE(INPT)
               CLOSE(IOUT)
  301       CALL NFILIO (5 ,    3 , INWL)
            CLOSE(INWL)
            CLOSE(IOUT)
            GO TO 400
11    WRITE(ISCN,1122) FNAME, IERROR
 1122 FORMAT(' OPEN FILE ERROR FNAME: ',A20,' IOSTAT= ',I5)
      GO TO 400
   13  WRITE(ISCN,'(A)')' Format read error in input file:' 
C  TERMINATE PROGRAM
C--- SUCCESSFUL COMPLETION OF MERGE.
  400 CLOSE(ISCR)
      CLOSE(ISCQ)
      CLOSE(IOUQ)
  401 STOP
C
C   FORMATS.
C
    5  FORMAT (I5,2(I3,I2),2(4X,I1),6(3X,I2))
   15  FORMAT (1H1,/, 5X,A10,5X,24H** MERGE GEODOP INPUT **,5X,A10
     +          ,///,48H  STEP  OUTPUT  TOTAL#  INPUT    STATION   #PASS
     +              ,21H    T I M E   S P A N,16X,13HN  O  T  E  S,/)
   25  FORMAT (I5,8H    TAPE,I2,I6)
   35  FORMAT (1H ,I26,4X,A10,2I6,2(1H/,I2),2H -,I4,2(1H/,I2),1X,4A10,/)
   45  FORMAT (///,I4,23H STATIONS WERE MERGED -,/
     +        ,(/,I5,13H ........... ,A10))
   55  FORMAT (1H1,/,5X,A10,5X,29H** MERGE PRECISE EPHEMERIS **5X,A10)
C
       END

