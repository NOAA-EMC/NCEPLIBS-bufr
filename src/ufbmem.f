C> @file
C> @brief Connect a new system file to the BUFRLIB software, and read
C> the entire file contents into internal arrays.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine connects a new system file to the BUFRLIB software
C> for input operations, then reads the entire file contents into
C> internal arrays so that any of the individual BUFR messages can
C> later be accessed from memory, instead of having to read them one
C> at a time sequentially from the system file.
C>
C> Any embedded DX BUFR tables contained within the file are also
C> read and processed into separate internal arrays for later use.
C>
C> Logical unit number LUNIT must already be associated with an
C> actual filename on the local system, typically via a Fortran "OPEN"
C> statement.
C>
C> When INEW = 0, the output value IUNIT will be set equal to the
C> input value LUNIT.  Otherwise, the output value IUNIT will be set to
C> the value of LUNIT that was input when this subroutine was previously
C> called with INEW = 0, and the system file connected to LUNIT will be
C> closed via an internal call to subroutine closbf() before exiting
C> this subroutine.  In either case, IUNIT can now be used to access
C> all BUFR messages that were read and stored by all previous calls
C> to this subroutine.
C>
C> @param[in] LUNIT   -- integer: Fortran logical unit number for BUFR
C>                       file
C> @param[in] INEW    -- integer: Processing option
C>                       - 0 = Initialize the internal arrays, then
C>                             read all BUFR messages from LUNIT into
C>                             internal arrays
C>                       - Otherwise, read all BUFR messages from LUNIT
C>                         and append them to the existing messages
C>                         within the internal arrays
C> @param[out] IRET   -- integer: Number of BUFR messages that were
C>                       read from LUNIT and stored into internal arrays
C> @param[out] IUNIT  -- integer: File status
C>                       - 0 = LUNIT was empty, so no messages were read
C>                       - Otherwise, the Fortran logical unit number to
C>                         use for later access to any of the messages
C>                         from the internal arrays
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE UFBMEM(LUNIT,INEW,IRET,IUNIT)

      USE MODV_MAXMEM
      USE MODV_MAXMSG
      USE MODV_IM8B

      USE MODA_MGWA
      USE MODA_MSGMEM

      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR,ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(INEW,MY_INEW,1)
         CALL UFBMEM(MY_LUNIT,MY_INEW,IRET,IUNIT)
         CALL X48(IRET,IRET,1)
         CALL X48(IUNIT,IUNIT,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  TRY TO OPEN BUFR FILE AND SET TO INITIALIZE OR CONCATENATE
C  ----------------------------------------------------------

      CALL OPENBF(LUNIT,'IN',LUNIT)

      IF(INEW.EQ.0) THEN
         MSGP(0) = 0
         MUNIT = 0
         MLAST = 0
         NDXTS = 0
         LDXTS = 0
         NDXM = 0
         LDXM = 0
      ENDIF

      NMSG = MSGP(0)
      IRET = 0
      IFLG = 0
      ITIM = 0

C     Copy any BUFR dictionary table messages from the beginning of
C     LUNIT into MODULE MSGMEM for possible later use.  Note that
C     such a table (if one exists) is already now in scope due to the
C     prior call to subroutine OPENBF, which in turn would have
C     automatically called subroutines READDX, RDBFDX and MAKESTAB
C     for this table.

      ITEMP = NDXTS
      CALL STATUS(LUNIT,LUN,IL,IM)
      CALL CEWIND(LUN)
      CALL CPDXMM(LUNIT)

C     If a table was indeed present at the beginning of the file,
C     then set the flag to indicate that this table is now in scope.

      IF ((ITEMP+1).EQ.NDXTS) LDXTS = NDXTS

C  TRANSFER MESSAGES FROM FILE TO MEMORY - SET MESSAGE POINTERS
C  ------------------------------------------------------------

1     CALL RDMSGW(LUNIT,MGWA,IER)
      IF(IER.EQ.-1) GOTO 100
      IF(IER.EQ.-2) GOTO 900

      IF(IDXMSG(MGWA).EQ.1) THEN

C       New "embedded" BUFR dictionary table messages have been found in
C       this file.  Copy them into MODULE MSGMEM for later use.

        CALL BACKBUFR(LUN) !BACKSPACE LUNIT
        CALL CPDXMM(LUNIT)
        GOTO 1
      ENDIF

      NMSG = NMSG+1
      IF(NMSG      .GT.MAXMSG) IFLG = 1
      LMEM = NMWRD(MGWA)
      IF(LMEM+MLAST.GT.MAXMEM) IFLG = 2

      IF(IFLG.EQ.0) THEN
         IRET = IRET+1
         DO I=1,LMEM
            MSGS(MLAST+I) = MGWA(I)
         ENDDO
         MSGP(0)    = NMSG
         MSGP(NMSG) = MLAST+1
      ELSE
         IF(ITIM.EQ.0) THEN
            MLAST0 = MLAST
            ITIM=1
         ENDIF
      ENDIF
      MLAST = MLAST+LMEM
      GOTO 1

C  EXITS
C  -----

100   IF(IFLG.EQ.1) THEN

C  EMERGENCY ROOM TREATMENT FOR MAXMSG ARRAY OVERFLOW
C  --------------------------------------------------

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I8,A)' )
     . 'BUFRLIB: UFBMEM - THE NO. OF MESSAGES REQUIRED TO STORE ',
     . 'ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', MAXMSG,
     . ') - INCOMPLETE READ'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEM STORED ', MSGP(0), ' MESSAGES OUT OF ', NMSG, '<<<'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEM STORED ', MLAST0, ' BYTES OUT OF ', MLAST, '<<<'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      MLAST=MLAST0
      ENDIF

      IF(IFLG.EQ.2) THEN

C  EMERGENCY ROOM TREATMENT FOR MAXMEM ARRAY OVERFLOW
C  --------------------------------------------------

      IF(IPRT.GE.0) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      WRITE ( UNIT=ERRSTR, FMT='(A,A,I8,A)' )
     . 'BUFRLIB: UFBMEM - THE NO. OF BYTES REQUIRED TO STORE ',
     . 'ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', MAXMEM,
     . ') - INCOMPLETE READ'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEM STORED ', MLAST0, ' BYTES OUT OF ', MLAST, '<<<'
      CALL ERRWRT(ERRSTR)
      WRITE ( UNIT=ERRSTR, FMT='(A,I8,A,I8,A)' )
     . '>>>UFBMEM STORED ', MSGP(0), ' MESSAGES OUT OF ', NMSG, '<<<'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF
      MLAST=MLAST0
      ENDIF

      IF(IRET.EQ.0) THEN
         CALL CLOSBF(LUNIT)
      ELSE
         IF(MUNIT.NE.0) CALL CLOSBF(LUNIT)
         IF(MUNIT.EQ.0) MUNIT = LUNIT
      ENDIF
      IUNIT = MUNIT

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: UFBMEM - ERROR READING MESSAGE '//
     . 'NUMBER",I5," INTO MEMORY FROM UNIT",I3)') NMSG+1,LUNIT
      CALL BORT(BORT_STR)
      END
