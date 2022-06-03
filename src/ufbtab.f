C> @file
C> @brief Read one or more data values from every data subset in a
C> BUFR file

C> This subroutine reads through every data subset in a BUFR file
C> and returns one or more specified data values from each subset.
C>
C> <p>This provides a useful way to scan the ranges of one or more
C> specified data values across an entire BUFR file. 
C>
C> @author J. Woollen
C> @date 1994-01-06
C>      
C> @param[in] LUNIN   -- integer: Absolute value is Fortran logical
C>                       unit number for BUFR file
C> @param[out] TAB    -- real*8(*,*): Data values
C> @param[in] I1 -- integer: Actual first dimension of TAB as allocated
C>                  within the calling program
C> @param[in] I2 -- integer: Actual second dimension of TAB as allocated
C>                  within the calling program
C> @param[out] IRET -- integer: Number of data subsets in BUFR file
C> @param[in] STR -- character*(*): String of blank-separated
C>                   Table B mnemonics, in one-to-one correspondence
C>                   with the number of data values that will be read
C>                   from each data subset within the first dimension of
C>                   TAB (see [DX BUFR Tables](@ref dfbftab) for further
C>                   information about Table B mnemonics)
C>
C> <p>It is the user's responsibility to ensure that TAB is dimensioned
C> sufficiently large enough to accommodate the number of data values
C> that are to be read from the BUFR file.  Specifically, each row of
C> TAB will contain the data values read from a different data subset,
C> so the value I2 must be at least as large as the total number of data
C> subsets in the BUFR file.
C>
C> <p>If logical unit ABS(LUNIN) has already been opened
C> via a previous call to subroutine openbf(), then this subroutine
C> will save the current file position, rewind the file to the
C> beginning, read through the entire file, and then restore it to its
C> previous file position.  Otherwise, if logical unit ABS(LUNIN) has
C> not already been opened via a previous call to subroutine openbf(),
C> then this subroutine will open it via an internal call to
C> subroutine openbf(), read through the entire file, and then close
C> it via an internal call to subroutine closbf().
C>
C> @remarks
C> - If LUNIN < 0, the number of data subsets in the BUFR file will
C> still be returned in IRET; however, STR will be ignored,
C> and all of the values returned in TAB will contain the current
C> placeholder value for "missing" data.
C> - If any of the Table B mnemonics in STR are replicated within the
C> data subset definition for the BUFR file, then this subroutine will
C> only return the value corresponding to the first occurrence of each
C> such mnemonic (counting from the beginning of the data subset
C> definition) within the corresponding row of TAB.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort() |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32 |
C> | 2000-09-19 | J. Woollen | Maximum length increased from 10,000 to 20,000 bytes |
C> | 2002-05-14 | J. Woollen | Removed old Cray compiler directives |
C> | 2003-11-04 | D. Keyser  | Modified to not abort when there are more than I2 data subsets, but instead just process first I2 subsets and print a diagnostic |
C> | 2003-11-04 | D. Keyser  | Increased MAXJL from 15000 to 16000; modified to use rewnbf(); upgraded to allow reading from a file that has already been opened via openbf() |
C> | 2004-08-09 | J. Ator    | Maximum message length increased from 20,000 to 50,000 bytes |
C> | 2005-09-16 | J. Woollen | upgraded to work for compressed BUFR messages, and to allow for LUNIN < 0 option |
C> | 2006-04-14 | J. Ator    | Add declaration for CREF |
C> | 2007-01-19 | J. Ator    | Replaced call to parseq with call to parstr() |
C> | 2009-04-21 | J. Ator    | Use errwrt() |
C> | 2009-12-01 | J. Ator    | Fix bug for compressed character strings which are identical across all subsets in a single messagE |
C> | 2010-05-07 | J. Ator    | When calling ireadmg(), treat read error as EOF condition |
C> | 2012-03-02 | J. Ator    | Use function ups() |
C> | 2012-09-15 | J. Woollen | Modified for C/I/O/BUFR interface; added IO type 'INX' to enable open and close for C file without closing FORTRAN file |
C> | 2014-11-20 | J. Ator    | Ensure openbf() has been called at least once before calling status() |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2016-12-19 | J. Woollen | Fix bug to prevent inventory overflow |
C> | 2022-05-06 | J. Woollen | Use up8 and upb8 for 8byte integers, use nbmp for usrtpl, add msgunp=1 option |
C>
      SUBROUTINE UFBTAB(LUNIN,TAB,I1,I2,IRET,STR)

      USE MODV_BMISS
      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_UNPTYP
      USE MODA_BITBUF
      USE MODA_TABLES

      COMMON /USRSTR/ NNOD,NCON,NODS(20),NODC(10),IVLS(10),KONS(10)
      COMMON /ACMODE/ IAC
      COMMON /QUIET / IPRT

      CHARACTER*(*) STR
      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*40  CREF
      CHARACTER*10  TGS(100)
      CHARACTER*8   SUBSET,CVAL
      EQUIVALENCE   (CVAL,RVAL)
      integer*8     ival,lref,ninc,mps,lps
      LOGICAL       OPENIT,JUST_COUNT
      REAL*8        TAB(I1,I2),RVAL,UPS

      DATA MAXTG /100/

C-----------------------------------------------------------------------
      MPS(NODE) = 2_8**(IBT(NODE))-1
      LPS(LBIT) = MAX(2_8**(LBIT)-1,1)
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

      if(msgunp(lun)==2) goto 115 

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

      if(msgunp(lun)==0) MBIT = MBYT(LUN)*8 + 16
      if(msgunp(lun)==1) MBIT = MBYT(LUN)

      NBIT = 0
      N = 1
      CALL USRTPL(LUN,N,N)
20    IF(N+1.LE.NVAL(LUN)) THEN
         N = N+1
         NODE = INV(N,LUN)
         MBIT = MBIT+NBIT
         NBIT = IBT(NODE)
         IF(ITP(NODE).EQ.1) THEN
            CALL UPB8(IVAL,NBIT,MBIT,MBAY(1,LUN))
            NBMP=IVAL; CALL USRTPL(LUN,N,NBMP)
         ENDIF
         DO I=1,NNOD
         IF(NODS(I).EQ.NODE) THEN
            IF(ITP(NODE).EQ.1) THEN
               CALL UPB8(IVAL,NBIT,MBIT,MBAY(1,LUN))
               TAB(I,IRET) = IVAL
            ELSEIF(ITP(NODE).EQ.2) THEN
               CALL UPB8(IVAL,NBIT,MBIT,MBAY(1,LUN))
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

      if(msgunp(lun)==0) then
         IBIT = MBYT(LUN)*8
         CALL UPB(NBYT,16,MBAY(1,LUN),IBIT)
         MBYT(LUN) = MBYT(LUN) + NBYT
      elseif(msgunp(lun)==1) then
         mbyt(lun)=mbit
      endif

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
         CALL UP8(LREF,NBIT,MBAY(1,LUN),IBIT)
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

C  PROCESS A TYPE1 NODE INTO NVAL
C  ------------------------------

      IF(ITYP.EQ.1) THEN
         JBIT = IBIT + LINC
         CALL UP8(NINC,LINC,MBAY(1,LUN),JBIT)
         IVAL = LREF+NINC
         CALL USRTPL(LUN,N,int(IVAL))
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
         CALL UP8(NINC,LINC,MBAY(1,LUN),JBIT)
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
      END
