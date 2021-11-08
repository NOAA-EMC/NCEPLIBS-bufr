C> @file
C> @brief Read a BUFR message from a memory array.

C> This subroutine is similar to subroutine readmg(), except that it
C> reads a BUFR message from an array already in memory, whereas
C> readmg() reads a BUFR message from a file on the local system.
C>
C> @authors J. Woollen
C> @authors J. Ator
C> @date 1995-06-28
C>
C> @param[in] MESG    -- integer(*): BUFR message
C> @param[in] LUNIT   -- integer: Fortran logical unit number for
C>                       BUFR file
C> @param[out] SUBSET  -- character*8: Table A mnemonic for type of BUFR
C>                        message that was read
C>                        (see [DX BUFR Tables](@ref dfbftab)
C>                        for further information about Table A mnemonics)
C> @param[out] JDATE   -- integer: Date-time stored within Section 1 of
C>                        BUFR message that was read, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C> @param[out] IRET    -- integer: return code
C>                           - 0 = MESG was successfully read
C>                           - 11 = MESG contained a DX BUFR table message
C>                           - -1 = MESG contained an unrecognized
C>                                  Table A message type
C>
C> <p>This subroutine looks and behaves a lot like subroutine readmg() 
C> except that here we have one additional input argument MESG which
C> contains the BUFR message to be read by the BUFRLIB software.
C> As such, this subroutine can be used in any context in which readmg()
C> might otherwise be used, and from that point on, the application
C> program can proceed with a call to one of the
C> [subset-reading subroutines](@ref hierarchy) (and then,
C> subsequently, to any of the
C> [values-reading subroutines](@ref hierarchy)), just
C> like if readmg() had been called instead.
C>
C> <p>When using this subroutine, it's still necessary for the
C> application program to have previously called subroutine openbf() in
C> order to associate a DX BUFR tables file with the messages that are
C> being input via MESG, and it's still also necessary to pass in the
C> relevant LUNIT value as a call argument, even though in this case
C> the subroutine will not actually try to read from the associated
C> Fortran logical unit.
C>
C> <p>If MESG contains a DX BUFR table message, the subroutine will
C> store the contents internally and use them to process any
C> future BUFR messages associated with LUNIT.  In this case, the
C> subroutine will return with IRET = 11, and any number of
C> DX BUFR table messages passed in via consecutive calls to this
C> subroutine will accumulate internally and be treated as a single DX
C> BUFR table, up until a call is made where MESG no longer contains a
C> DX BUFR table message.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1995-06-28 | J. Woollen | Original author |
C> | 1997-07-29 | J. Woollen | Modified to process GOES soundings from NESDIS |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine "ABORT" with call to new internal routine bort(); modified to make Y2K compliant; improved machine portability |
C> | 1999-11-18 | J. Woollen | The number of BUFR files which can be opened at one time increased from 10 to 32; increased the maximum number of possible descriptors in a subset from 1000 to 3000 |
C> | 2000-09-19 | J. Woollen | Removed logic that had been replicated in this and other read routines and consolidated it into a new routine cktaba(); maximum message length increased from 10,000 to 20,000 bytes |
C> | 2003-11-04 | S. Bender  | Added remarks and routine interdependencies |
C> | 2003-11-04 | D. Keyser  | Unified/portable for WRF; added documentation; outputs more complete diagnostic info when routine terminates abnormally |
C> | 2004-08-18 | J. Ator    | Modified 'BUFR' string test for portability to EBCDIC machines; maximum message length increased from 20,000 to 50,000 bytes |
C> | 2005-11-29 | J. Ator    | Use ichkstr() |
C> | 2009-03-23 | D. Keyser  | Call bort() in case of MBAY overflow |
C> | 2009-03-23 | J. Ator    | Add logic to allow Section 3 decoding; add logic to process dictionary messages |
C> | 2012-06-07 | J. Ator    | Don't respond to DX table messages if Section 3 decoding is being used |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C>
      SUBROUTINE READERME(MESG,LUNIT,SUBSET,JDATE,IRET)

      USE MODA_SC3BFR
      USE MODA_IDRDM
      USE MODA_BITBUF

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)
      COMMON /QUIET/  IPRT

      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*8 SUBSET,SEC0
      CHARACTER*1 CEC0(8)

      DIMENSION   MESG(*),IEC0(2)

      LOGICAL ENDTBL

      EQUIVALENCE (SEC0,IEC0,CEC0)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      CALL WTSTAT(LUNIT,LUN,IL, 1)

C  COPY THE INPUT MESSAGE INTO THE INTERNAL MESSAGE BUFFER
C  -------------------------------------------------------

      IEC0(1) = MESG(1)
      IEC0(2) = MESG(2)
      LNMSG = LMSG(SEC0)
      IF(LNMSG*NBYTW.GT.MXMSGL) GOTO 902
      DO I=1,LNMSG
        MBAY(I,LUN) = MESG(I)
      ENDDO

C     Confirm that the first 4 bytes of SEC0 contain 'BUFR' encoded in
C     CCITT IA5 (i.e. ASCII).

      IF(ICHKSTR('BUFR',CEC0,4).NE.0) GOTO 903

C  PARSE THE MESSAGE SECTION CONTENTS
C  ----------------------------------

      IF(ISC3(LUN).NE.0) CALL READS3(LUN)

      CALL CKTABA(LUN,SUBSET,JDATE,IRET)

      IF(ISC3(LUN).NE.0) RETURN

C  CHECK FOR A DX DICTIONARY MESSAGE
C  ---------------------------------

C     A new DX dictionary table can be passed in as a consecutive set of
C     DX dictionary messages.  Each message should be passed in one at a
C     time, via input argument MESG during consecutive calls to this
C     subroutine, and will be processed as a single dictionary table up
C     until the next message is passed in which either contains no data
C     subsets or else is a non-DX dictionary message.

      ENDTBL = .FALSE.

      IF(IDXMSG(MBAY(1,LUN)).EQ.1) THEN

C	This is a DX dictionary message that was generated by the
C	BUFRLIB archive library software.

	IF(IUPBS3(MBAY(1,LUN),'NSUB').EQ.0) THEN

C	  But it doesn't contain any actual dictionary information, so
C	  assume we've reached the end of the dictionary table.

	  IF(IDRDM(LUN).GT.0) THEN
	    ENDTBL = .TRUE.
          ENDIF
	ELSE
	  IF(IDRDM(LUN).EQ.0) THEN

C	    This is the first DX dictionary message that is part of a
C	    new dictionary table.

	    CALL DXINIT(LUN,0)
	  ENDIF
	  IDRDM(LUN) = IDRDM(LUN) + 1
	  CALL STBFDX(LUN,MBAY(1,LUN))
	ENDIF
      ELSE IF(IDRDM(LUN).GT.0) THEN

C	This is the first non-DX dictionary message received following a
C       string of DX dictionary messages, so assume we've reached the
C	end of the dictionary table.

	ENDTBL = .TRUE.
      ENDIF

      IF(ENDTBL) THEN
	IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	WRITE ( UNIT=ERRSTR, FMT='(A,I3,A)' )
     .    'BUFRLIB: READERME - STORED NEW DX TABLE CONSISTING OF (',
     .    IDRDM(LUN), ') MESSAGES;'
	CALL ERRWRT(ERRSTR)
	ERRSTR = 'WILL APPLY THIS TABLE TO ALL SUBSEQUENT DATA '//
     .    'MESSAGES UNTIL NEXT DX TABLE IS PASSED IN'
	CALL ERRWRT(ERRSTR)
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
	ENDIF
	IDRDM(LUN) = 0
	CALL MAKESTAB
      ENDIF

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: READERME - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: READERME - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   WRITE(BORT_STR,'("BUFRLIB: READERME - INPUT BUFR MESSAGE LENGTH",
     . 1X,I6," BYTES) IS LARGER THAN LIMIT OF ",I6," BYTES")')
     . LNMSG*NBYTW,MXMSGL
      CALL BORT(BORT_STR)
903   CALL BORT('BUFRLIB: READERME - FIRST 4 BYTES READ FROM RECORD'//
     . ' NOT "BUFR", DOES NOT CONTAIN BUFR DATA')
      END
