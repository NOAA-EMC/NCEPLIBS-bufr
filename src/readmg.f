C> @file
C> @brief Read the next message from a BUFR file that was previously
C> opened for reading.

C> This subroutine reads the next BUFR message from logical unit
C> ABS(LUNXX) into internal arrays.
C>
C> @authors J. Woollen
C> @authors J. Ator
C> @date 1994-01-06
C>
C> @param[in] LUNXX    - integer: Absolute value is Fortran logical unit
C>                       number for BUFR file
C> @param[out] SUBSET   - character*8: Table A mnemonic for type of BUFR
C>                        message that was read
C>                        (see [DX BUFR Tables](@ref dfbftab)
C>                        for further information about Table A mnemonics)
C> @param[out] JDATE    - integer: Date-time stored within Section 1 of
C>                        BUFR message that was read, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C> @param[out] IRET     - integer: return code
C>                           - 0 = new BUFR message was successfully
C>                                 read into internal arrays
C>                           - -1 = there are no more BUFR messages in
C>                                 the file connected to logical unit
C>                                 ABS(LUNXX)
C>
C> <p>Logical unit ABS(LUNXX) should have already been opened for
C> input operations via a previous call to subroutine openbf().
C>
C> <p>Whenever this subroutine returns with IRET = 0, this indicates
C> that a new BUFR message of type SUBSET and date-time JDATE was
C> successfully read into internal arrays within the BUFRLIB
C> software, and from where it can then be easily manipulated or further
C> parsed via a call to subroutine readsb() or equivalent.  Otherwise,
C> if the subroutine returns with IRET = -1, then this indicates that
C> there are no more BUFR messages (i.e. end-of-file) within the file
C> connected to logical unit ABS(LUNXX).
C>
C> @remarks
C> - Any DX dictionary messages encountered within ABS(LUNXX) will be
C> automatically processed and stored internally, so a successful return
C> from this subroutine will always result in a BUFR message containing
C> actual data values within the internal arrays.
C> - In prior versions of the BUFRLIB software, an input value of
C> LUNXX < 0 was an indicator to the subroutine to treat any read error
C> from ABS(LUNXX) the same as an end-of-file condition.  This option is
C> no longer supported, but the capability to call this subroutine with
C> LUNXX < 0 is itself still supported for backwards-compatibiity with
C> certain legacy application programs. 
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1996-11-25  J. Woollen -- Modified to exit gracefully when the BUFR
C>                           file is positioned after an "end-of-file"
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine
C>                           "ABORT" with call to new internal BUFRLIB
C>                           routine "BORT"; modified to make Y2K
C>                           compliant
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                           opened at one time increased from 10 to 32
C>                           (necessary in order to process multiple
C>                           BUFR files under the MPI); modified with
C>                           semantic adjustments to ameliorate compiler
C>                           complaints from Linux boxes (increases
C>                           portability)
C> - 2000-09-19  J. Woollen -- Removed message decoding logic that had
C>                           been replicated in this and other read
C>                           routines and consolidated it into a new
C>                           routine cktaba(); maximum message
C>                           length increased from 10,000 to 20,000
C>                           bytes
C> - 2002-05-14  J. Woollen -- Removed entry point datelen() (it became a
C>                           separate routine in the BUFRLIB to increase
C>                           portability to other platforms)
C> - 2003-11-04  J. Ator    -- Added documentation
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- Unified/portable for WRF; added history
C>                           documentation; outputs more complete
C>                           diagnostic info when routine terminates
C>                           abnormally
C> - 2004-08-09  J. Ator    -- Maximum message length increased from
C>                           20,000 to 50,000 bytes
C> - 2005-11-29  J. Ator    -- Added rdmsgw() and rdmsgb calls to simulate
C>                           readibm; added LUNXX < 0 option to simulate
C>                           readft
C> - 2009-03-23  J. Ator    -- Add logic to allow Section 3 decoding;
C>                           add logic to process internal dictionary
C>                           messages 
C> - 2012-06-07  J. Ator    -- Don't respond to internal dictionary
C>                           messages if Section 3 decoding is being used
C> - 2012-09-15  J. Woollen -- Convert to C language I/O interface;
C>                           remove code to reread message as bytes;
C>                           replace Fortran BACKSPACE with C backbufr()
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
      SUBROUTINE READMG(LUNXX,SUBSET,JDATE,IRET)

      USE MODA_MSGCWD
      USE MODA_SC3BFR
      USE MODA_BITBUF

      INCLUDE 'bufrlib.inc'

      COMMON /QUIET / IPRT

      CHARACTER*128 ERRSTR
      CHARACTER*8 SUBSET

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      IRET = 0
      LUNIT = ABS(LUNXX)

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      CALL WTSTAT(LUNIT,LUN,IL,1)

C  READ A MESSAGE INTO THE INTERNAL MESSAGE BUFFER
C  -----------------------------------------------

1     CALL RDMSGW(LUNIT,MBAY(1,LUN),IER)
      IF(IER.EQ.-1) GOTO 200

C  PARSE THE MESSAGE SECTION CONTENTS
C  ----------------------------------

      IF(ISC3(LUN).NE.0) CALL READS3(LUN)
      CALL CKTABA(LUN,SUBSET,JDATE,IRET)

C  LOOK FOR A DICTIONARY MESSAGE
C  -----------------------------

      IF(IDXMSG(MBAY(1,LUN)).NE.1) RETURN

C     This is an internal dictionary message that was
C     generated by the BUFRLIB archive library software.

      IF(ISC3(LUN).NE.0) RETURN

C     Section 3 decoding isn't being used, so backspace the
C     file pointer and then use subroutine RDBFDX to read in
C     all such dictionary messages (they should be stored
C     consecutively!) and reset the internal tables.

      CALL BACKBUFR(LUN) 
      CALL RDBFDX(LUNIT,LUN)

      IF(IPRT.GE.1) THEN
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      ERRSTR = 'BUFRLIB: READMG - INTERNAL DICTIONARY MESSAGE READ;'//
     .' ACCOUNT FOR IT THEN READ IN NEXT MESSAGE WITHOUT RETURNING'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C     Now go read another message.

      GOTO 1

C  EOF ON ATTEMPTED READ
C  ---------------------

200   CALL WTSTAT(LUNIT,LUN,IL,0)
      INODE(LUN) = 0
      IDATE(LUN) = 0
      SUBSET = ' '
      JDATE = 0
      IRET = -1
      RETURN

C  EXITS
C  -----

900   CALL BORT('BUFRLIB: READMG - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: READMG - INPUT BUFR FILE IS OPEN FOR OUTPUT'//
     . ', IT MUST BE OPEN FOR INPUT')
      END
