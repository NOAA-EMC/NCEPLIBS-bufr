C> @file
C> @brief Read a specified BUFR message from internal arrays.

C> This subroutine reads a specified BUFR message from internal
C> arrays in memory, so that it is now in scope for processing
C> via a subsequent call to subroutine rdmems().
C>
C> <p>BUFR messages should already be stored within internal
C> arrays in memory via one or more previous calls to
C> subroutine ufbmem().
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] IMSG    - integer: Number of BUFR message to be
C>                      read into scope for further processing,
C>                      counting from the beginning of the
C>                      internal arrays in memory
C> @param[out] SUBSET - character*8: Table A mnemonic for type of BUFR
C>                      message that was read into scope
C>                      (see [DX BUFR Tables](@ref dfbftab) for
C>                      further information about Table A mnemonics)
C> @param[out] JDATE  - integer: Date-time stored within Section 1 of
C>                      BUFR message that was read into scope,
C>                      in format of either YYMMDDHH or YYYYMMDDHH,
C>                      depending on the most
C>                      recent call to subroutine datelen()
C> @param[out] IRET   - integer: return code
C>                          - 0 = requested message was
C>                                successfully read into scope
C>                          - -1 = requested message number could not
C>                                 be found in internal arrays
C>      
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine
C>                           "ABORT" with call to new internal BUFRLIB
C>                           routine "BORT"; modified to make Y2K
C>                           compliant
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                           opened at one time increased from 10 to 32
C>                           (necessary in order to process multiple
C>                           BUFR files under the MPI); increased MAXMEM
C>                           from 4 Mb to 8 Mb
C> - 2000-09-19  J. Woollen -- Removed message decoding logic that had
C>                           been replicated in this and other read
C>                           routines and consolidated it into a new
C>                           routine cktaba(); maximum message
C>                           length increased from 10,000 to 20,000
C>                           bytes
C> - 2001-08-15  D. Keyser  -- Increased MAXMEM from 8 Mb to 16 Mb
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                             documentation
C> - 2004-08-09  J. Ator    -- Maximum message length increased
C>                             from 20,000 to 50,000 bytes
C> - 2004-11-15  D. Keyser  -- Increased MAXMEM from 16 Mb to 50 Mb
C> - 2009-03-23  J. Ator    -- Modified to handle embedded BUFR table
C>                           (dictionary) messages; use errwrt()
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
      SUBROUTINE RDMEMM(IMSG,SUBSET,JDATE,IRET)

      USE MODA_MSGCWD
      USE MODA_BITBUF
      USE MODA_MGWA
      USE MODA_MSGMEM

      COMMON /QUIET / IPRT

      CHARACTER*128 BORT_STR,ERRSTR
      CHARACTER*8   SUBSET

      LOGICAL KNOWN

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK THE MESSAGE REQUEST AND FILE STATUS
C  -----------------------------------------

      CALL STATUS(MUNIT,LUN,IL,IM)
      CALL WTSTAT(MUNIT,LUN,IL, 1)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IRET = 0

      IF(IMSG.EQ.0 .OR.IMSG.GT.MSGP(0)) THEN
         CALL WTSTAT(MUNIT,LUN,IL,0)
         IF(IPRT.GE.1) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
            IF(IMSG.EQ.0)  THEN
               ERRSTR = 'BUFRLIB: RDMEMM - REQUESTED MEMORY MESSAGE '//
     .          'NUMBER {FIRST (INPUT) ARGUMENT} IS 0, RETURN WITH '//
     .          'IRET = -1'
            ELSE
               WRITE ( UNIT=ERRSTR, FMT='(A,I6,A,I6,A)' )
     .          'BUFRLIB: RDMEMM - REQ. MEMORY MESSAGE #', IMSG,
     .          ' {= 1ST (INPUT) ARG.} > # OF MESSAGES IN MEMORY (',
     .          MSGP(0), '), RETURN WITH IRET = -1'
            ENDIF
            CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
         ENDIF
         IRET = -1
         GOTO 100
      ENDIF

C  ENSURE THAT THE PROPER DICTIONARY TABLE IS IN SCOPE
C  ---------------------------------------------------

C     Determine which table applies to this message.

      KNOWN = .FALSE.
      JJ = NDXTS
      DO WHILE ((.NOT.KNOWN).AND.(JJ.GE.1))
	 IF (IPMSGS(JJ).LE.IMSG) THEN
	    KNOWN = .TRUE.
	 ELSE
	    JJ = JJ - 1
	 ENDIF
      ENDDO
      IF (.NOT.KNOWN) GOTO 902

C     Is this table the one that is currently in scope?

      IF (JJ.NE.LDXTS) THEN

C	 No, so reset the software to use the proper table.

	 IF(IPRT.GE.2) THEN
            CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++')
            WRITE ( UNIT=ERRSTR, FMT='(A,I3,A,I3,A,I6)' )
     .	      'BUFRLIB: RDMEMM - RESETTING TO USE DX TABLE #', JJ,
     .	      ' INSTEAD OF DX TABLE #', LDXTS,
     .        ' FOR REQUESTED MESSAGE #', IMSG
            CALL ERRWRT(ERRSTR)
            CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++')
            CALL ERRWRT(' ')
	 ENDIF
	 CALL DXINIT(LUN,0)

C	 Store each of the DX dictionary messages which constitute
C	 this table.

	 DO II = IFDXTS(JJ), (IFDXTS(JJ)+ICDXTS(JJ)-1)
	    IF (II.EQ.NDXM) THEN
	       NWRD = LDXM - IPDXM(II) + 1
	    ELSE
	       NWRD = IPDXM(II+1) - IPDXM(II)
	    ENDIF
	    DO KK = 1, NWRD
	       MGWA(KK) = MDX(IPDXM(II)+KK-1)
	    ENDDO
	    CALL STBFDX(LUN,MGWA)
	 ENDDO

C	 Rebuild the internal jump/link table.

	 CALL MAKESTAB
	 LDXTS = JJ
      ENDIF

C  READ MEMORY MESSAGE NUMBER IMSG INTO A MESSAGE BUFFER
C  -----------------------------------------------------

      IPTR = MSGP(IMSG)
      IF(IMSG.LT.MSGP(0)) LPTR = MSGP(IMSG+1)-IPTR
      IF(IMSG.EQ.MSGP(0)) LPTR = MLAST-IPTR+1
      IPTR = IPTR-1

      DO I=1,LPTR
         MBAY(I,LUN) = MSGS(IPTR+I)
      ENDDO

C  PARSE THE MESSAGE SECTION CONTENTS
C  ----------------------------------

      CALL CKTABA(LUN,SUBSET,JDATE,JRET)
      NMSG(LUN) = IMSG

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: RDMEMM - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: RDMEMM - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   WRITE(BORT_STR,'("BUFRLIB: RDMEMM - UNKNOWN DX TABLE FOR '//
     . 'REQUESTED MESSAGE #",I5)') IMSG
      CALL BORT(BORT_STR)
      END
