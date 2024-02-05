C> @file
C> @brief Parse the Table A mnemonic and date out of Section 1 of a
C> BUFR message.
C>
C> @author Woollen @date 2000-09-19

C> Return the Table A mnemonic and date
C> from Section 1 of a BUFR message that was previously read from LUN
C> using one of the [message-reading subroutines](@ref hierarchy).
C>
C> @param[in] LUN - integer: file ID.
C> @param[out] SUBSET - character*8: Table A mnemonic
C>                      - returned as a string of all blank characters
C>                        if IRET is equal to 11 (see below) and if Section 3
C>                        isn't being used for decoding
C> @param[out] JDATE    - integer: date-time stored within Section 1 of BUFR
C>                        in format of either YYMMDDHH or
C>                        YYYYMMDDHH, depending on datelen() value.
C> @param[out] IRET - integer: return code:
C>                      - 0 normal return
C>                      - -1 unrecognized Table A (message type) value
C>                      - 11 this is a BUFR table (dictionary) message
C>
C> @author Woollen @date 2000-09-19

      SUBROUTINE CKTABA(LUN,SUBSET,JDATE,IRET)

      use moda_msgcwd
      use moda_sc3bfr
      use moda_unptyp
      use moda_bitbuf

      COMMON /PADESC/ IBCT,IPD1,IPD2,IPD3,IPD4
      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*8   SUBSET
      CHARACTER*2   CPFX(3)
      CHARACTER*1   TAB
      LOGICAL       TRYBT, DIGIT

      DATA CPFX   / 'NC', 'FR', 'FN' /
      DATA NCPFX  / 3 /

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

      TRYBT = .TRUE.

      JDATE = IGETDATE(MBAY(1,LUN),IYR,IMO,IDY,IHR)

c  .... Message type
      MTYP = IUPBS01(MBAY(1,LUN),'MTYP')
c  .... Message subtype
      MSBT = IUPBS01(MBAY(1,LUN),'MSBT')

      IF(MTYP.EQ.11) THEN
c  .... This is a BUFR table (dictionary) message.
         IRET = 11
c  .... There's no need to proceed any further unless Section 3 is being
c  .... used for decoding.
         IF(ISC3(LUN).EQ.0) THEN
            SUBSET = "        "
            GOTO 100
         ENDIF
      ENDIF

C  PARSE SECTION 3
C  ---------------

      CALL GETLENS(MBAY(1,LUN),3,LEN0,LEN1,LEN2,LEN3,L4,L5)

      IAD3 = LEN0+LEN1+LEN2

c  .... First descriptor (integer)
      KSUB = IUPB(MBAY(1,LUN),IAD3+8 ,16)
c  .... Second descriptor (integer)
      ISUB = IUPB(MBAY(1,LUN),IAD3+10,16)

C  LOCATE SECTION 4
C  ----------------

      IAD4 = IAD3+LEN3

C  NOW, TRY TO GET "SUBSET" (MNEMONIC ASSOCIATED WITH TABLE A) FROM MSG
C  --------------------------------------------------------------------

C  FIRST CHECK WHETHER SECTION 3 IS BEING USED FOR DECODING
C  --------------------------------------------------------

      IF(ISC3(LUN).NE.0) THEN
        SUBSET = TAMNEM(LUN)
c  .... is SUBSET from Table A?
        CALL NEMTBAX(LUN,SUBSET,MTY1,MSB1,INOD)
        IF(INOD.GT.0) THEN
c  ....   yes it is
          MBYT(LUN) = 8*(IAD4+4)
          MSGUNP(LUN) = 1
          GOTO 10
        ENDIF
      ENDIF

C  IF ISUB FROM SECTION 3 DEFINES TABLE A THEN MSGUNP=0
C  ----------------------------------------------------

c  .... get SUBSET from ISUB
5     CALL NUMTAB(LUN,ISUB,SUBSET,TAB,ITAB)
c  .... is SUBSET from Table A?
      CALL NEMTBAX(LUN,SUBSET,MTY1,MSB1,INOD)
      IF(INOD.GT.0) THEN
c  .... yes it is
         MBYT(LUN) = (IAD4+4)
         MSGUNP(LUN) = 0
         GOTO 10
      ENDIF

C  IF KSUB FROM SECTION 3 DEFINES TABLE A THEN MSGUNP=1 (standard)
C  ---------------------------------------------------------------

c  .... get SUBSET from KSUB
      CALL NUMTAB(LUN,KSUB,SUBSET,TAB,ITAB)
c  .... is SUBSET from Table A?
      CALL NEMTBAX(LUN,SUBSET,MTY1,MSB1,INOD)
      IF(INOD.GT.0) THEN
c  .... yes it is
         MBYT(LUN) = 8*(IAD4+4)
         MSGUNP(LUN) = 1
         GOTO 10
      ENDIF

C  OKAY, STILL NO "SUBSET", LETS MAKE IT "NCtttsss" (where ttt=MTYP
C  and sss=MSBT) AND SEE IF IT DEFINES TABLE A.  IF NOT, THEN ALSO
C  TRY "FRtttsss" AND "FNtttsss".
C  ----------------------------------------------------------------

      II=1
      DO WHILE(II.LE.NCPFX)
         WRITE(SUBSET,'(A2,2I3.3)') CPFX(II),MTYP,MSBT
c  ....    is SUBSET from Table A?
         CALL NEMTBAX(LUN,SUBSET,MTY1,MSB1,INOD)
         IF(INOD.GT.0) THEN
c  ....     yes it is
            IF(KSUB.EQ.IBCT) THEN
               MBYT(LUN) = (IAD4+4)
               MSGUNP(LUN) = 0
            ELSE
               MBYT(LUN) = 8*(IAD4+4)
               MSGUNP(LUN) = 1
            ENDIF
            GOTO 10
         ENDIF
         II=II+1
      ENDDO

C  NOW WE HAVE A GENERATED "SUBSET", BUT IT STILL DOES NOT DEFINE
C  TABLE A - MAKE ONE LAST DESPERATE ATTEMPT - SEE IF AN EXTERNAL
C  USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER FORMAT IS DEFINED
C  IN OPENBT (ONLY POSSIBLE IF APPLICATION PROGRAM HAS AN IN-LINE
C  OPENBT OVERRIDING THE ONE IN THE BUFR ARCHIVE LIBRARY)
C  ------------------------------------------------------------------

      IF(TRYBT) THEN
         TRYBT = .FALSE.
         IF(IPRT.GE.1) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      ERRSTR = 'BUFRLIB: CKTABA - LAST RESORT, CHECK FOR EXTERNAL'//
     .  ' BUFR TABLE VIA CALL TO IN-LINE OPENBT'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         CALL OPENBT(LUNDX,MTYP)
         IF(LUNDX.GT.0) THEN
c  .... Good news, there is a unit (LUNDX) connected to a table file,
c  .... so store the table internally
            CALL RDUSDX(LUNDX,LUN)
            GOTO 5
         ENDIF
      ENDIF

C  IF ALL ATTEMPTS TO DEFINE TABLE A FAIL SKIP GIVE UP
C  ---------------------------------------------------

      IF(IPRT.GE.0)  THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: CKTABA - UNRECOGNIZED TABLE A MESSAGE TYPE ('//
     . SUBSET // ') - RETURN WITH IRET = -1'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      IRET = -1
      GOTO 100

C  CHECK THE VALIDITY OF THE MTYP/MSBT AND FOR COMPRESSION (MSGUNP=2)
C  ------------------------------------------------------------------

10    IF(ISC3(LUN).EQ.0) THEN
        IF(MTYP.NE.MTY1) GOTO 900
        IF(MSBT.NE.MSB1.AND.DIGIT(SUBSET(3:8))) GOTO 901
      ENDIF
      IF(IUPBS3(MBAY(1,LUN),'ICMP').GT.0) MSGUNP(LUN) = 2

C  SET THE OTHER REQUIRED PARAMETERS IN MESSAGE CONTROL WORD PARTITION
C  -------------------------------------------------------------------

c  .... Date for this message
      IDATE(LUN) = I4DY(JDATE)
c  .... Positional index of Table A mnem.
      INODE(LUN) = INOD
c  .... Number of subsets in this message
      MSUB(LUN) = IUPBS3(MBAY(1,LUN),'NSUB')
c  .... Number of subsets read so far from this message
      NSUB(LUN) = 0

      IF(IRET.NE.11) THEN
c   .... Number of non-dictionary messages read so far from this file
         NMSG(LUN) = NMSG(LUN)+1
      ENDIF

C  EXITS
C  -----

100   RETURN
900   WRITE(BORT_STR,'("BUFRLIB: CKTABA - MESSAGE TYPE MISMATCH '//
     . '(SUBSET=",A8,", MTYP=",I3,", MTY1=",I3)') SUBSET,MTYP,MTY1
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: CKTABA - MESSAGE SUBTYPE MISMATCH '//
     . '(SUBSET=",A8,", MSBT=",I3,", MSB1=",I3)') SUBSET,MSBT,MSB1
      CALL BORT(BORT_STR)
      END
