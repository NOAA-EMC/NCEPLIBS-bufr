      SUBROUTINE UFBTAB(LUNIN,TAB,I1,I2,IRET,STR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    UFBTAB
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE EITHER OPENS A BUFR FILE CONNECTED TO
C   ABS(LUNIN) FOR INPUT OPERATIONS (IF IT IS NOT ALREADY OPENED AS
C   SUCH), OR SAVES ITS POSITION AND REWINDS IT TO THE FIRST DATA
C   MESSAGE (IF BUFR FILE ALREADY OPENED), THE EXTENT OF ITS PROCESSING
C   IS DETERMINED BY THE SIGN OF LUNIN.  IF LUNIN IS GREATER THAN ZERO,
C   THIS SUBROUTINE READS SPECIFIED VALUES FROM ALL DATA SUBSETS IN THE
C   BUFR FILE INTO INTERNAL ARRAYS AND RETURNS THESE VALUES ALONG WITH
C   A COUNT OF THE SUBSETS.  IF LUNIN IS LESS THAN ZERO, THIS
C   SUBROUTINE RETURNS THE BUFR ARCHIVE LIBRARY'S GLOBAL VALUE FOR
C   MISSING (REGARDLESS OF THE MNEMONICS SPECIFIED IN STR)
C   ALONG WITH A COUNT OF THE SUBSETS (SEE REMARKS 2).  FINALLY, THIS
C   SUBROUTINE EITHER CLOSES THE BUFR FILE IN ABS(LUNIN) (IF IT WAS
C   OPENED HERE) OR RESTORES IT TO ITS PREVIOUS READ/WRITE STATUS AND
C   POSITION (IF IT WAS NOT OPENED HERE).  WHEN LUNIN IS GREATER THAN
C   ZERO, THE DATA VALUES CORRESPOND TO MNEMONICS, NORMALLY WHERE THERE
C   IS NO REPLICATION (THERE CAN BE REGULAR OR DELAYED REPLICATION, BUT
C   THIS SUBROUTINE WILL ONLY READ THE FIRST OCCURRENCE OF THE MNEMONIC
C   IN EACH SUBSET).  UFBTAB PROVIDES A MECHANISM WHEREBY A USER CAN
C   EITHER DO A QUICK SCAN OF THE RANGE OF VALUES CORRESPONDING TO ONE
C   OR MORE MNEMNONICS AMONGST ALL DATA SUBSETS FOR AN ENTIRE BUFR FILE
C   (WHEN LUNIN IS GREATER THAN ZERO), OR SIMPLY OBTAIN A COUNT OF
C   SUBSETS IN THE BUFR FILE (WHEN LUNIN IS LESS THAN ZERO); NO OTHER
C   BUFR ARCHIVE LIBRARY ROUTINES HAVE TO BE CALLED.  THIS SUBROUTINE
C   IS SIMILAR TO BUFR ARCHIVE LIBRARY SUBROUTINE UFBTAM EXCEPT UFBTAM
C   READS SUBSETS FROM MESSAGES STORED IN INTERNAL MEMORY AND IT HAS NO
C   OPTION FOR RETURNING ONLY A COUNT OF THE SUBSETS.  IN ADDITION,
C   UFBTAM CURRENTLY CANNOT READ DATA FROM COMPRESSED BUFR MESSAGES.
C   UFBTAB CAN READ DATA FROM BOTH UNCOMPRESSED AND COMPRESSED BUFR
C   MESSAGES.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- IMPROVED MACHINE PORTABILITY
C 1998-10-27  J. WOOLLEN -- MODIFIED TO CORRECT PROBLEMS CAUSED BY IN-
C                           LINING CODE WITH FPP DIRECTIVES
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI)
C 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           10,000 TO 20,000 BYTES
C 2002-05-14  J. WOOLLEN -- REMOVED OLD CRAY COMPILER DIRECTIVES
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MODIFIED TO NOT ABORT WHEN THERE ARE TOO
C                           MANY SUBSETS COMING IN (I.E., .GT. "I2"),
C                           BUT RATHER JUST PROCESS "I2" REPORTS AND
C                           PRINT A DIAGNOSTIC; MAXJL (MAXIMUM NUMBER
C                           OF JUMP/LINK ENTRIES) INCREASED FROM 15000
C                           TO 16000 (WAS IN VERIFICATION VERSION);
C                           MODIFIED TO CALL ROUTINE REWNBF WHEN THE
C                           BUFR FILE IS ALREADY OPENED, ALLOWS
C                           SPECIFIC SUBSET INFORMATION TO BE READ FROM
C                           A FILE IN THE MIDST OF ITS BEING READ FROM
C                           OR WRITTEN TO), BEFORE OPENBF WAS ALWAYS
C                           CALLED AND THIS WOULD HAVE LED TO AN ABORT
C                           OF THE APPLICATION PROGRAM (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY)
C 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2005-09-16  J. WOOLLEN -- WORKS FOR COMPRESSED BUFR MESSAGES; ADDED
C                           OPTION TO RETURN ONLY SUBSET COUNT (WHEN
C                           INPUT UNIT NUMBER IS LESS THAN ZERO)
C 2006-04-14  J. ATOR    -- ADD DECLARATION FOR CREF
C 2007-01-19  J. ATOR    -- REPLACED CALL TO PARSEQ WITH CALL TO PARSTR
C 2009-04-21  J. ATOR    -- USE ERRWRT
C 2009-12-01  J. ATOR    -- FIX BUG FOR COMPRESSED CHARACTER STRINGS
C                           WHICH ARE IDENTICAL ACROSS ALL SUBSETS IN
C                           A SINGLE MESSAGE
C 2010-05-07  J. ATOR    -- WHEN CALLING IREADMG, TREAT READ ERROR AS
C                           END-OF-FILE CONDITION
C 2012-03-02  J. ATOR    -- USE FUNCTION UPS
C 2012-09-15  J. WOOLLEN -- MODIFIED FOR C/I/O/BUFR INTERFACE;
C                           USE NEW OPENBF TYPE 'INX' TO OPEN AND CLOSE
C                           THE C FILE WITHOUT CLOSING THE FORTRAN FILE
C 2014-11-20  J. ATOR    -- ENSURE OPENBF HAS BEEN CALLED AT LEAST ONCE
C                           BEFORE CALLING STATUS
C 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C
C USAGE:    CALL UFBTAB (LUNIN, TAB, I1, I2, IRET, STR)
C   INPUT ARGUMENT LIST:
C     LUNIN    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER
C                FOR BUFR FILE
C     I1       - INTEGER:
C                  - IF LUNIN IS GREATER THAN ZERO: LENGTH OF FIRST
C                    DIMENSION OF TAB OR THE NUMBER OF BLANK-SEPARATED
C                    MNEMONICS IN STR, (FORMER MUST BE AT LEAST AS
C                    LARGE AS LATTER)
C                  - IF LUNIN IS LESS THAN ZERO: LENGTH OF FIRST
C                    DIMENSION OF TAB (RECOMMEND PASSING IN WITH VALUE
C                    OF 1 - SEE REMARKS 2)
C     I2       - INTEGER: LENGTH OF SECOND DIMENSION OF TAB
C                  - IF LUNIN IS GREATER THAN ZERO: MUST BE AT LEAST AS
C                    LARGE AS VALUE RETURNED IN IRET, OTHERWISE ONLY
C                    FIRST I2 SUBSETS ARE RETURNED IN TAB
C                  - IF LUNIN IS LESS THAN ZERO: RECOMMEND PASSING IN
C                    WITH VALUE OF 1 - SEE REMARKS 2
C     STR      - CHARACTER*(*):
C                  - IF LUNIN IS GREATER THAN ZERO: STRING OF BLANK-
C                    SEPARATED TABLE B MNEMONICS IN ONE-TO-ONE
C                    CORRESPONDENCE WITH FIRST DIMENSION OF TAB, I1
C                    (THE NUMBER OF MNEMONICS IN THE STRING MUST BE NO
C                    LARGER THAN I1)
C                      - THERE ARE THREE "GENERIC" MNEMONICS NOT
C                        RELATED TO TABLE B, THESE RETURN THE FOLLOWING
C                        INFORMATION IN CORRESPONDING TAB LOCATION:
C                         'NUL'  WHICH ALWAYS RETURNS BMISS ("MISSING")
C                         'IREC' WHICH ALWAYS RETURNS THE CURRENT BUFR
C                                MESSAGE (RECORD) NUMBER IN WHICH THIS
C                                SUBSET RESIDES
C                         'ISUB' WHICH ALWAYS RETURNS THE CURRENT
C                                SUBSET NUMBER OF THIS SUBSET WITHIN
C                                THE BUFR MESSAGE (RECORD) NUMBER
C                                'IREC'
C                  - IF LUNIN IS LESS THAN ZERO: DUMMY {RECOMMEND
C                    PASSING IN STRING AS A 1-CHARACTER BLANK (i.e.,
C                    ' ') - SEE REMARKS 2}
C
C   OUTPUT ARGUMENT LIST:
C     TAB      - REAL*8: (I1,I2):
C                  - IF LUNIN IS GREATER THAN ZERO: STARTING ADDRESS OF
C                    DATA VALUES READ FROM BUFR FILE
C                  - IF LUNIN IS LESS THAN ZERO: STARTING ADDRESS OF
C                    ARRAY OF VALUES ALL RETURNED WITH THE BUFRLIB'S
C                    GLOBAL VALUE FOR MISSING (BMISS)
C     IRET     - INTEGER: NUMBER OF DATA SUBSETS IN BUFR FILE
C                  - IF LUNIN IS GREATER THAN ZERO: MUST BE NO LARGER
C                    THAN I2, OTHERWISE ONLY FIRST I2 SUBSETS ARE
C                    RETURNED IN TAB
C
C REMARKS:
C    1) NOTE THAT UFBMEM CAN BE CALLED PRIOR TO THIS TO STORE THE BUFR
C       MESSAGES INTO INTERNAL MEMORY.
C
C    2) BELOW ARE TWO EXAMPLES WHERE THE USER CALLS UFBTAB WITH LUNIN
C       LESS THAN ZERO SO AS TO ONLY OBTAIN A COUNT OF SUBSETS IN A
C       BUFR FILE (ALONG WITH THE BUFRLIB'S GLOBAL VALUE FOR
C       "MISSING").
C
C       EXAMPLE 1) I1 AND I2 ARE SET TO 1 SUCH THAT TAB IS A SCALAR AND
C          STR IS SET TO A 1-CHARACTER BLANK.  THESE ARE THE
C          RECOMMENDED VALUES FOR I1, I2 AND STR SINCE THEY USE THE
C          LEAST AMOUNT OF MEMORY):
C
C            REAL(8) TAB
C                 ....
C                 ....
C            CALL UFBTAB(-LUNIN,TAB,1,1,IRET,' ')
C                 ....
C                 ....
C
C          HERE IRET WILL RETURN THE COUNT OF SUBSETS IN THE BUFR FILE
C          AND TAB WILL RETURN THE BUFRLIB'S GLOBAL VALUE FOR "MISSING"
C          (BMISS).
C
C       EXAMPLE 2) I1 IS SET TO 4 AND I2 IS SET TO 8 SUCH THAT TAB IS A
C          32-WORD ARRAY, AND STR IS SET TO A NONSENSICAL STRING.
C          THESE VALUES FOR I1, I2 AND STR WASTE MEMORY BUT GIVE THE
C          SAME ANSWERS FOR TAB AND IRET AS IN EXAMPLE 1 (FOR THE SAME
C          INPUT BUFR FILE!):
C
C            REAL(8) TAB(4,8)
C                 ....
C                 ....
C            CALL UFBTAB(-LUNIN,TAB,4,8,IRET,'BUFR IS A WONDERFUL FMT')
C                 ....
C                 ....
C
C          HERE IRET WILL AGAIN RETURN THE COUNT OF SUBSETS IN THE BUFR
C          FILE AND ALL 32 VALUES OF ARRAY TAB WILL RETURN THE
C          BUFRLIB'S GLOBAL VALUE FOR "MISSING" (BMISS).
C
C       THE SIXTH ARGUMENT STR IS A DUMMY VALUE AND CAN BE SET TO
C       ANY CHARACTER STRING (AGAIN, A 1-CHARACTER BLANK ' ' IS
C       RECOMMENDED).  THE THIRD ARGUMENT I1 HAS NO RELATIONSHIP WITH
C       THE NUMBER OF BLANK-SEPARATED MNEMONICS IN STR AND CAN BE SET
C       TO ANY INTEGER VALUE (AGAIN, 1 IS RECOMMENDED).  THE FOURTH
C       ARGUMENT I2 HAS NO RELATIONSHIP WITH THE NUMBER OF DATA SUBSETS
C       IN THE BUFR FILE RETURNED IN IRET (AGAIN, 1 IS RECOMMENDED).
C
C.....................................................................
C
C    THIS ROUTINE CALLS:        BORT     CLOSBF   ERRWRT   IREADMG
C                               IREADSB  MESGBC   NMSUB    OPENBF
C                               PARSTR   REWNBF   STATUS   STRING
C                               UPB      UPBB     UPC      UPS
C                               USRTPL
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_TABLES

      INCLUDE 'bufrlib.prm'

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)
      COMMON /ACMODE/ IAC
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*40  CREF
      CHARACTER*10  TGS(100)
      CHARACTER*8   SUBSET,CVAL
      EQUIVALENCE   (CVAL,RVAL)
      LOGICAL       OPENIT,JUST_COUNT
      REAL*8        TAB(I1,I2),RVAL,UPS

      DATA MAXTG /100/

C-----------------------------------------------------------------------
      MPS(NODE) = 2**(IBT(NODE))-1
      LPS(LBIT) = MAX(2**(LBIT)-1,1)
C-----------------------------------------------------------------------

C  SET COUNTERS TO ZERO
C  --------------------

      IRET = 0
      IREC = 0
      ISUB = 0
      IACC = IAC

C  CHECK FOR COUNT SUBSET ONLY OPTION (RETURNING THE BUFRLIB'S GLOBAL
C   VALUE FOR MISSING IN OUTPUT ARRAY) INDICATED BY NEGATIVE UNIT
C  ------------------------------------------------------------------

      LUNIT = ABS(LUNIN)
      JUST_COUNT = LUNIN.LT.LUNIT

C     Make sure OPENBF has been called at least once before trying to
C     call STATUS; otherwise, STATUS might try to access array space
C     that hasn't yet been dynamically allocated.
      CALL OPENBF(0,'FIRST',0)

      CALL STATUS(LUNIT,LUN,IL,IM)
      OPENIT = IL.EQ.0

      IF(OPENIT) THEN

C  OPEN BUFR FILE CONNECTED TO UNIT LUNIT IF IT IS NOT ALREADY OPEN
C  ----------------------------------------------------------------

         CALL OPENBF(LUNIT,'INX',LUNIT)
      ELSE

C  IF BUFR FILE ALREADY OPENED, SAVE POSITION & REWIND TO FIRST DATA MSG
C  ---------------------------------------------------------------------

         CALL REWNBF(LUNIT,0)        
      ENDIF

      IAC = 1

C  SET THE OUTPUT ARRAY VALUES TO THE BUFRLIB'S GLOBAL VALUE FOR
C   MISSING (BMISS)
C  -------------------------------------------------------------

      DO J=1,I2
      DO I=1,I1
      TAB(I,J) = BMISS
      ENDDO
      ENDDO

      IF(JUST_COUNT) THEN

C  COME HERE FOR COUNT ONLY OPTION (OUTPUT ARRAY VALUES REMAIN MISSING)
C  --------------------------------------------------------------------

         DO WHILE(IREADMG(-LUNIT,SUBSET,IDATE).GE.0)
         IRET = IRET+NMSUB(LUNIT)
         ENDDO
         GOTO 25
      ENDIF

C  OTHERWISE, CHECK FOR SPECIAL TAGS IN STRING
C  -------------------------------------------

      CALL PARSTR(STR,TGS,MAXTG,NTG,' ',.TRUE.)
      DO I=1,NTG
      IF(TGS(I).EQ.'IREC') IREC = I
      IF(TGS(I).EQ.'ISUB') ISUB = I
      ENDDO

C  READ A MESSAGE AND PARSE A STRING
C  ---------------------------------

10    IF(IREADMG(-LUNIT,SUBSET,JDATE).LT.0) GOTO 25
      CALL STRING(STR,LUN,I1,0)
      IF(IREC.GT.0) NODS(IREC) = 0
      IF(ISUB.GT.0) NODS(ISUB) = 0

C  PARSE THE MESSAGE DEPENDING ON WHETHER COMPRESSED OR NOT
C  --------------------------------------------------------

      CALL MESGBC(-LUNIT,MTYP,ICMP)
      IF(ICMP.EQ.0) THEN
         GOTO 15
      ELSEIF(ICMP.EQ.1) then
         GOTO 115
      ELSE
         GOTO 900
      ENDIF

C  ---------------------------------------------
C  THIS BRANCH IS FOR UNCOMPRESSED MESSAGES
C  ---------------------------------------------
C  SEE IF THERE IS ANOTHER SUBSET IN THE MESSAGE
C  ---------------------------------------------

15    IF(NSUB(LUN).EQ.MSUB(LUN)) GOTO 10
      IF(IRET+1.GT.I2) GOTO 99
      IRET = IRET+1

      DO I=1,NNOD
      NODS(I) = ABS(NODS(I))
      ENDDO

C  PARSE THE STRING NODES FROM A SUBSET
C  ------------------------------------

      MBIT = MBYT(LUN)*8 + 16
      NBIT = 0
      N = 1
      CALL USRTPL(LUN,N,N)
20    IF(N+1.LE.NVAL(LUN)) THEN
         N = N+1
         NODE = INV(N,LUN)
         MBIT = MBIT+NBIT
         NBIT = IBT(NODE)
         IF(ITP(NODE).EQ.1) THEN
            CALL UPBB(IVAL,NBIT,MBIT,MBAY(1,LUN))
            CALL USRTPL(LUN,N,IVAL)
         ENDIF
         DO I=1,NNOD
         IF(NODS(I).EQ.NODE) THEN
            IF(ITP(NODE).EQ.1) THEN
               CALL UPBB(IVAL,NBIT,MBIT,MBAY(1,LUN))
               TAB(I,IRET) = IVAL
            ELSEIF(ITP(NODE).EQ.2) THEN
               CALL UPBB(IVAL,NBIT,MBIT,MBAY(1,LUN))
               IF(IVAL.LT.MPS(NODE)) TAB(I,IRET) = UPS(IVAL,NODE)
            ELSEIF(ITP(NODE).EQ.3) THEN
               CVAL = ' '
               KBIT = MBIT
               CALL UPC(CVAL,NBIT/8,MBAY(1,LUN),KBIT,.TRUE.)
               TAB(I,IRET) = RVAL
            ENDIF
            NODS(I) = -NODS(I)
            GOTO 20
         ENDIF
         ENDDO
         DO I=1,NNOD
         IF(NODS(I).GT.0) GOTO 20
         ENDDO
      ENDIF

C  UPDATE THE SUBSET POINTERS BEFORE NEXT READ
C  -------------------------------------------

      IBIT = MBYT(LUN)*8
      CALL UPB(NBYT,16,MBAY(1,LUN),IBIT)
      MBYT(LUN) = MBYT(LUN) + NBYT
      NSUB(LUN) = NSUB(LUN) + 1
      IF(IREC.GT.0) TAB(IREC,IRET) = NMSG(LUN)
      IF(ISUB.GT.0) TAB(ISUB,IRET) = NSUB(LUN)
      GOTO 15

C  ---------------------------------------------
C  THIS BRANCH IS FOR COMPRESSED MESSAGES
C  ---------------------------------------------
C  STORE ANY MESSAGE AND/OR SUBSET COUNTERS
C  ---------------------------------------------

C  CHECK ARRAY BOUNDS
C  ------------------

115   IF(IRET+MSUB(LUN).GT.I2) GOTO 99

C  STORE MESG/SUBS TOKENS
C  ----------------------

      IF(IREC.GT.0.OR.ISUB.GT.0) THEN
         DO NSB=1,MSUB(LUN)
         IF(IREC.GT.0) TAB(IREC,IRET+NSB) = NMSG(LUN)
         IF(ISUB.GT.0) TAB(ISUB,IRET+NSB) = NSB
         ENDDO
      ENDIF

C  SETUP A NEW SUBSET TEMPLATE, PREPARE TO SUB-SURF
C  ------------------------------------------------

      CALL USRTPL(LUN,1,1)
      IBIT = MBYT(LUN)
      N = 0

C  UNCOMPRESS CHOSEN NODES INTO THE TAB ARRAY (FIRST OCCURANCES ONLY)
C  ------------------------------------------------------------------

C  READ ELEMENTS LOOP
C  ------------------

120   DO N=N+1,NVAL(LUN)
      NODE = INV(N,LUN)
      NBIT = IBT(NODE)
      ITYP = ITP(NODE)

C  FIRST TIME IN RESET NODE INDEXES, OR CHECK FOR NODE(S) STILL NEEDED
C  -------------------------------------------------------------------

      IF(N.EQ.1) THEN
         DO I=1,NNOD
         NODS(I) = ABS(NODS(I))
         ENDDO
      ELSE
         DO I=1,NNOD
         IF(NODS(I).GT.0) GOTO 125
         ENDDO
         GOTO 135
      ENDIF

C  FIND THE EXTENT OF THE NEXT SUB-GROUP
C  -------------------------------------

125   IF(ITYP.EQ.1.OR.ITYP.EQ.2) THEN
         CALL UPB(LREF,NBIT,MBAY(1,LUN),IBIT)
         CALL UPB(LINC,   6,MBAY(1,LUN),IBIT)
         NIBIT = IBIT + LINC*MSUB(LUN)
      ELSEIF(ITYP.EQ.3) THEN
         CREF=' '
         CALL UPC(CREF,NBIT/8,MBAY(1,LUN),IBIT,.TRUE.)
         CALL UPB(LINC,   6,MBAY(1,LUN),IBIT)
         NIBIT = IBIT + 8*LINC*MSUB(LUN)
      ELSE
         GOTO 120
      ENDIF

C  LOOP OVER STRING NODES
C  ----------------------

      DO I=1,NNOD

C  CHOSEN NODES LOOP - KEEP TRACK OF NODES NEEDED AND NODES FOUND
C  --------------------------------------------------------------

      IF(NODE.NE.NODS(I)) GOTO 130
      NODS(I) = -NODS(I)
      LRET = IRET

C  PROCESS A FOUND NODE INTO TAB
C  -----------------------------

      IF(ITYP.EQ.1.OR.ITYP.EQ.2) THEN
         DO NSB=1,MSUB(LUN)
         JBIT = IBIT + LINC*(NSB-1)
         CALL UPB(NINC,LINC,MBAY(1,LUN),JBIT)
         IVAL = LREF+NINC
         LRET = LRET+1
         IF(NINC.LT.LPS(LINC)) TAB(I,LRET) = UPS(IVAL,NODE)
         ENDDO
      ELSEIF(ITYP.EQ.3) THEN
         DO NSB=1,MSUB(LUN)
         IF(LINC.EQ.0) THEN
           CVAL = CREF
         ELSE
           JBIT = IBIT + LINC*(NSB-1)*8
           CVAL = ' '
           CALL UPC(CVAL,LINC,MBAY(1,LUN),JBIT,.TRUE.)
         ENDIF
         LRET = LRET+1
         TAB(I,LRET) = RVAL
         ENDDO
      ELSE
         CALL BORT('UFBTAB - INVALID ELEMENT TYPE SPECIFIED')
      ENDIF

C  END OF LOOPS FOR COMPRESSED MESSAGE PARSING
C  -------------------------------------------

130   CONTINUE
      ENDDO
      IF(ITYP.EQ.1) CALL USRTPL(LUN,N,IVAL)
      IBIT = NIBIT

C  END OF READ ELEMENTS LOOP
C  -------------------------

      ENDDO
135   IRET = IRET+MSUB(LUN)

C  END OF MESSAGE PARSING - GO BACK FOR ANOTHER
C  --------------------------------------------

      GOTO 10

C  -------------------------------------------
C  ERROR PROCESSING AND EXIT ROUTES BELOW
C  -------------------------------------------
C  EMERGENCY ROOM TREATMENT FOR ARRAY OVERFLOW
C  -------------------------------------------

99    NREP = IRET
      DO WHILE(IREADSB(LUNIT).EQ.0)
      NREP = NREP+1
      ENDDO
      DO WHILE(IREADMG(-LUNIT,SUBSET,JDATE).GE.0)
      NREP = NREP+NMSUB(LUNIT)
      ENDDO
      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I8,A,A)' )
     . 'BUFRLIB: UFBTAB - THE NO. OF DATA SUBSETS IN THE BUFR FILE ',
     . 'IS .GT. LIMIT OF ', I2, ' IN THE 4TH ARG. (INPUT) - ',
     . 'INCOMPLETE READ'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBTAB STORED ', IRET, ' REPORTS OUT OF ', NREP, '<<<'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF


25    IF(OPENIT) THEN

C  CLOSE BUFR FILE IF IT WAS OPENED HERE
C  -------------------------------------

         CALL CLOSBF(LUNIT)
      ELSE

C  RESTORE BUFR FILE TO PREV. STATUS & POSITION IF NOT ORIG. OPENED HERE
C  ---------------------------------------------------------------------

         CALL REWNBF(LUNIT,1)
      ENDIF

      IAC = IACC

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: UFBTAB - INVALID COMPRESSION '//
     . 'INDICATOR (ICMP=",I3," RETURNED FROM BUFR ARCHIVE LIBRARY '//
     . 'ROUTINE MESGBC")') ICMP
      CALL BORT(BORT_STR)
      END
