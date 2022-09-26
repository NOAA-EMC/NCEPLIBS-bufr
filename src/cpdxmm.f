C> @file
C> Read embedded DX BUFR table into internal arrays.

C> This subroutine reads an entire DX BUFR table from a specified
C> file into internal arrays.
C>
C> @author J. Ator
C> @date 2009-03-23
C>
C> @param[in] LUNIT   -- integer: Fortran logical unit number for BUFR
C>                       file
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2009-03-23 | J. Ator    | Original author |
C> | 2012-09-15 | J. Woollen | Modified for C/I/O/BUFR interface; replace Fortran BACKSPACE with C backbufr() |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

	SUBROUTINE CPDXMM( LUNIT )

        USE MODV_MXDXTS

	USE MODA_MGWA
	USE MODA_MSGMEM
        USE MODV_IM8B

	COMMON /QUIET/  IPRT

	CHARACTER*128	ERRSTR

	INTEGER*8 LUNIT_8

	LOGICAL DONE

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Check for I8 integers

	IF(IM8B) THEN
	   IM8B=.FALSE.
	   LUNIT_8=LUNIT
	   CALL CPDXMM_8( LUNIT_8 )
	   IM8B=.TRUE.
	   RETURN
	ENDIF

	IF ( NDXTS .GE. MXDXTS ) GOTO 900

	ICT = 0
	DONE = .FALSE.
        CALL STATUS(LUNIT,LUN,IL,IM)

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
	    GOTO 901
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

C	    Store this message into MODULE MSGMEM.

            ICT = ICT + 1
	    IF ( ( NDXM + ICT ) .GT. MXDXM ) GOTO 902
            IPDXM(NDXM+ICT) = LDXM + 1
            LMEM = NMWRD(MGWA)
	    IF ( ( LDXM + LMEM ) .GT. MXDXW ) GOTO 903
            DO J = 1, LMEM
              MDX(LDXM+J) = MGWA(J)
            ENDDO
            LDXM = LDXM + LMEM
          ENDIF
	ENDDO

C	Update the table information within MODULE MSGMEM.

	IF ( ICT .GT. 0 ) THEN
          IFDXTS(NDXTS+1) = NDXM + 1
          ICDXTS(NDXTS+1) = ICT
          IPMSGS(NDXTS+1) = MSGP(0) + 1
          NDXM = NDXM + ICT
          NDXTS = NDXTS + 1
	  IF ( IPRT .GE. 2 ) THEN
	    CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++')
	    WRITE ( UNIT=ERRSTR, FMT='(A,I3,A,I3,A)') 
     .		'BUFRLIB: CPDXMM - STORED NEW DX TABLE #', NDXTS,
     .		' CONSISTING OF ', ICT, ' MESSAGES'
	    CALL ERRWRT(ERRSTR)
	    CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++')
            CALL ERRWRT(' ')
	  ENDIF
	ENDIF

	RETURN
 900	CALL BORT('BUFRLIB: CPDXMM - MXDXTS OVERFLOW')
 901	CALL BORT('BUFRLIB: CPDXMM - UNEXPECTED READ ERROR')
 902	CALL BORT('BUFRLIB: CPDXMM - MXDXM OVERFLOW')
 903	CALL BORT('BUFRLIB: CPDXMM - MXDXW OVERFLOW')
	END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine cpdxmm().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine cpdxmm() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIT_8 -- integer*8: Fortran logical unit number for
C>                       BUFR file
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

	SUBROUTINE CPDXMM_8( LUNIT_8 )

	INTEGER*8 LUNIT_8

	LUNIT=LUNIT_8
	CALL CPDXMM( LUNIT )

	RETURN
	END
