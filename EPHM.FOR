Cv3.31  INTERFACE TO SUBROUTINE TIME (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
Cv3.31  INTERFACE TO SUBROUTINE DATE (N,STR)
Cv3.31  CHARACTER*10 STR [NEAR,REFERENCE]	
Cv3.31  INTEGER*2 N [VALUE]
Cv3.31  END
      PROGRAM EPHM
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*40 FNAME, FBLNK
      CHARACTER*10 EPHMB,EPHMA, NDESC, DATTM, TIMME
      CHARACTER*4  ISRC
      INTEGER*2 ITIMME(3), IDATE(3)
C
C   VERSION: 03/23/83                  GEODOP IV+V   (JUL81)
C   MODIFIED FOR RUNNING ON IBM FORTRAN (G1 OR Q COMPILERS).
C   B. ARCHINAL, DEPT. OF GEODETIC SCIENCE AND SURVEYING,
C   THE OHIO STATE UNIVERSITY, COLUMBUS, OHIO.
C
C   NOTE: CHANGES FROM CDC VERSION ARE:
C   1. CDC "EOF"S CHANGED TO IBM "END="S.
C   2. VARIABLES USED FOR UNIT NUMBERS.
C   3. UNUSED ST. 30 COMMENTED OUT TO INCREASE EFFICIENCY.
C   4. CONTROL INPUT OPTIONS (DESCRIBED BELOW) ADDED.
C
C   SUBPROGRAMS USED:
C    GEODOP SYSTEM: CHEBY
C
C   I/O UNITS USED:
C    IF2      2    EPHEMERIS (OUTPUT - CARD IMAGE)
C    IF4      4    FITTED EPHEMERIS (INPUT - BINARY)
C
C   SUBPROGRAM READS FITTED EPHEMERIS AND OUTPUTS STANDART CODED FORMAT(
C
       DIMENSION CXYZ(10,3), XS(6), CHEB(10),DCHEB(10), VARORB(21)
     + ,ORB(8)
      DATA EPHMB/'EPHM.BIN  '/,EPHMA/'EPHM.ASC  '/,
     + FBLNK/'                                        '/ 
C
C   UNIT NUMBERS.
C
      IF2=2
      IF4=4
C  ISCN - SCREEN LU CHANGE IF REQUIRED SO
      ISCN=0
C      IBM PC VERSION  by J. Kouba (June 87)
C
C     LUN  FORMAT   TYPE     DEFAULT NAME   DESCRIPTION         VARIABLE
C     ---  ------   ----     ------------   ------------------  -------- 
C      IF4  BINARY  INPUT    EPHM.BIN       FITTED EPHEMERIS    EPHMB
C      IF2  ASCII   OUTPUT   EPHM.ASC       CODED EPHEMERIS     EPHMA
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
1234  FORMAT(' Program  E P H M  - Geodopv utility (Sep 87)',4X,2A10
     +/ ' Converts fitted (binary) ephemeris into coded files'/)
C
C
1     WRITE(ISCN,'(A\)')' Fitted ephemeris; use /E to end [EPHM.BIN]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=12) FNAME
      IF(FNAME.EQ.'/E'.OR.FNAME.EQ.'/e') GO TO 12
      IF(FNAME.EQ.FBLNK) FNAME= EPHMB
         OPEN(IF4,FILE=FNAME,STATUS='OLD',FORM='BINARY',ERR=11,
     +   IOSTAT=IERROR)
C
      WRITE(ISCN,'(A\)')' OUTPUT coded ephemeris(9 dig. f)[EPHM.ASC]: ' 
      FNAME=FBLNK
      READ(ISCN,'(A)',END=12) FNAME
      IF(FNAME.EQ.FBLNK) FNAME= EPHMA 
         OPEN(IF2,FILE=FNAME,STATUS='UNKNOWN',ERR=11,IOSTAT=IERROR)
C
      READ(IF4,END=12) IYR, IST, IDAY,NDESC, ID, ISRC, IORD
20      M = IORD
      IF(ISRC.EQ.'SARC')READ(IF4,END=50)LOKR,IMIN,((CXYZ(K,J),K=1,M),J=
     + 1,3) ,IST, ORB, NOTE
      IF(ISRC.EQ.'UPDT')READ(IF4,END=50)LOKR,IMIN,((CXYZ(K,J),K=1,M),J=
     + 1,3),IST, NDESC, VARORB
      IF(ISRC.EQ.'PREC')READ(IF4,END=50)LOKR,IMIN,((CXYZ(K,J),K=1,M),J=
     + 1,3), IST, IYR
      TK= 2.D0/DBLE(IMIN-1)
      T=0.D0
       DO 2 I=1,IMIN
      Y= T* TK-1.D0
       CALL CHEBY(CHEB,DCHEB, 1, IORD, Y, TK)
      T= T+ 1.D0
      DO 3 J=1,3
      N=J+ 3
      XS(J)= CXYZ(1,J)/ 1000.D0
      XS(N)= 0.D0
      DO 3 K=2,IORD
      XS(J)= XS(J) + CHEB(K)*CXYZ(K,J)/1000.D0
3      XS(N)= XS(N)+ DCHEB(K)*CXYZ(K,J)/60000.D0
C
      IDAY=(LOKR+I-1)/1440
      IH= ((LOKR+I-1-IDAY*1440))/60
       IM  = (LOKR+I-1)- IDAY*1440- IH*60
   31 WRITE(IF2,40) IYR, IDAY, IH, IM  , XS, IST, ID, NDESC
40      FORMAT(I2,1X,I3,2(1X,I2),3F13.6,3F13.9,1X,I7,2X,I10,A10 )
2     CONTINUE
C   WRITE ORBIT VAR COV MATRX
C      IF(ISRC.EQ.'UPDT')  WRITE(IF2,101) VARORB
101      FORMAT(/6F10.4,2X,5F10.4/4F10.4,2X,3F10.4,2X,2F10.4,2X,F10.4)
        GO TO 20
50      ENDFILE IF2
       CLOSE (IF2)
       CLOSE (IF4)
       GO TO 1
11    WRITE(ISCN,1122) FNAME, IERROR
 1122 FORMAT(' OPEN FILE ERROR FNAME: ',A20,' IOSTAT= ',I5)
      GO TO 12
   13 WRITE(ISCN,'(A)')' Format read error in EPHM input file:' 
C  TERMINATE PROGRAM
12    STOP  
  901 FORMAT(3I5)
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
C                     - LOKR COMPILED - APRIL   1975.
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

