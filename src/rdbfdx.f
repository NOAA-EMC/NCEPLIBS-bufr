C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> BEGINNING AT THE CURRENT FILE POINTER LOCATION WITHIN LUNIT,
C>   THIS SUBROUTINE READS A COMPLETE DICTIONARY TABLE (I.E. ONE OR MORE
C>   ADJACENT BUFR DX (DICTIONARY) MESSAGES) INTO INTERNAL MEMORY ARRAYS
C>   IN MODULE TABABD.
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C>                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C> 1996-12-17  J. WOOLLEN -- FIXED FOR SOME MVS COMPILER'S TREATMENT OF
C>                           INTERNAL READS (INCREASES PORTABILITY)
C> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"; CORRECTED SOME MINOR ERRORS
C> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           10,000 TO 20,000 BYTES
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C>                           TERMINATES ABNORMALLY
C> 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           20,000 TO 50,000 BYTES
C> 2005-11-29  J. ATOR    -- USE GETLENS, IUPBS01 AND RDMSGW
C> 2009-03-23  J. ATOR    -- USE STNTBIA; MODIFY LOGIC TO HANDLE BUFR
C>                           TABLE MESSAGES ENCOUNTERED ANYWHERE IN THE
C>                           FILE (AND NOT JUST AT THE BEGINNING!)
C> 2012-09-15  J. WOOLLEN -- MODIFIED FOR C/I/O/BUFR INTERFACE;
C>                           REPLACE FORTRAN BACKSPACE WITH C BACKBUFR
C> 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C>
C> USAGE:    CALL RDBFDX (LUNIT, LUN)
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>                (ASSOCIATED WITH FILE CONNECTED TO LOGICAL UNIT LUNIT)
C>
C>   INPUT FILES:
C>     UNIT "LUNIT" - BUFR FILE
C>
C> REMARKS:
C>
C>   THIS SUBROUTINE PERFORMS A FUNCTION SIMILAR TO BUFR ARCHIVE LIBRARY
C>   SUBROUTINE RDUSDX, EXCEPT THAT RDUSDX READS FROM A FILE CONTAINING
C>   A USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER FORMAT.  SEE THE
C>   DOCBLOCK IN RDUSDX FOR A DESCRIPTION OF THE ARRAYS THAT ARE FILLED
C>   IN MODULE TABABD.
C>
C>   THIS SUBROUTINE PERFORMS A FUNCTION SIMILAR TO BUFR ARCHIVE LIBRARY
C>   SUBROUTINE CPDXMM, EXCEPT THAT CPDXMM WRITES TO THE INTERNAL MEMORY
C>   ARRAYS IN MODULE MSGMEM, FOR USE WITH A FILE OF BUFR MESSAGES THAT
C>   IS BEING READ AND STORED INTO INTERNAL MEMORY BY BUFR ARCHIVE
C>   LIBRARY SUBROUTINE UFBMEM.
C>
C>    THIS ROUTINE CALLS:        BORT     DXINIT   ERRWRT   IDXMSG
C>                               IUPBS3   MAKESTAB RDMSGW   STBFDX
C>                               BACKBUFR
C>    THIS ROUTINE IS CALLED BY: POSAPX   READDX   READMG
C>                               Normally not called by any application
C>                               programs.
C>
      SUBROUTINE RDBFDX(LUNIT,LUN)



	USE MODA_MGWA

	INCLUDE 'bufrlib.inc'

	COMMON /QUIET/  IPRT

	CHARACTER*128 ERRSTR

	LOGICAL DONE

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	CALL DXINIT(LUN,0)

	ICT = 0
	DONE = .FALSE.

C	Read a complete dictionary table from LUNIT, as a set of one or
C	more DX dictionary messages.

	DO WHILE ( .NOT. DONE )
          CALL RDMSGW ( LUNIT, MGWA, IER )
          IF ( IER .EQ. -1 ) THEN

C	    Don't abort for an end-of-file condition, since it may be
C	    possible for a file to end with dictionary messages.
C	    Instead, backspace the file pointer and let the calling
C	    routine diagnose the end-of-file condition and deal with
C	    it as it sees fit.

	    CALL BACKBUFR(LUN)
	    DONE = .TRUE.
          ELSE IF ( IER .EQ. -2 ) THEN
	    GOTO 900
	  ELSE IF ( IDXMSG(MGWA) .NE. 1 ) THEN

C	    This is a non-DX dictionary message.  Assume we've reached
C	    the end of the dictionary table, and backspace LUNIT so that
C	    the next read (e.g. in the calling routine) will get this
C	    same message.

	    CALL BACKBUFR(LUN)
	    DONE = .TRUE.
	  ELSE IF ( IUPBS3(MGWA,'NSUB') .EQ. 0 ) THEN

C	    This is a DX dictionary message, but it doesn't contain any
C	    actual dictionary information.  Assume we've reached the end
C	    of the dictionary table.

	    DONE = .TRUE.
	  ELSE

C	    Store this message into MODULE TABABD.

            ICT = ICT + 1
	    CALL STBFDX(LUN,MGWA)
	  ENDIF
	ENDDO

	IF ( IPRT .GE. 2 ) THEN
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
	WRITE ( UNIT=ERRSTR, FMT='(A,I3,A)' )
     .    'BUFRLIB: RDBFDX - STORED NEW DX TABLE CONSISTING OF (',
     .    ICT, ') MESSAGES;'
	CALL ERRWRT(ERRSTR)
	ERRSTR = 'WILL APPLY THIS TABLE TO ALL SUBSEQUENT DATA IN '//
     .    'FILE UNTIL NEXT DX TABLE IS FOUND'
	CALL ERRWRT(ERRSTR)
	CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
	ENDIF

	CALL MAKESTAB

	RETURN
 900	CALL BORT('BUFRLIB: RDBFDX - ERROR READING A BUFR DICTIONARY '//
     .    'MESSAGE')
	END
