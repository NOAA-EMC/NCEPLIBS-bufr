	INTEGER FUNCTION ICBFMS ( STR, LSTR )

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    ICBFMS
C   PRGMMR: J. ATOR          ORG: NP12       DATE: 2012-06-07
C
C ABSTRACT: THIS FUNCTION TESTS WHETHER THE INPUT CHARACTER STRING
C   IS "MISSING" BY CHECKING IF ALL OF THE EQUIVALENT BITS ARE SET TO 1.
C   IT IS SIMILAR TO BUFR ARCHIVE LIBRARY FUNCTION IBFMS, EXCEPT THAT
C   IBFMS TESTS REAL*8 VALUES FOR EQUIVALENCE TO THE PARAMETER BMISS,
C   WHEREAS ICBFMS CHECKS THAT ALL EQUIVALENT BITS ARE SET TO 1 AND IS
C   THEREFORE A MORE PORTABLE AND RELIABLE TEST FOR USE WITH CHARACTER
C   STRINGS.
C
C PROGRAM HISTORY LOG:
C 2012-06-07  J. ATOR    -- ORIGINAL AUTHOR
C 2015-03-10  J. WOOLLEN -- IMPROVED LOGIC FOR TESTING LEGACY CASES
C                           PRIOR TO BUFRLIB V10.2.0
C 2016-02-12  J. ATOR    -- MODIFIED FOR CRAYFTN COMPATIBILITY
C
C USAGE:    ICBFMS ( STR, LSTR )
C   INPUT ARGUMENT LIST:
C     STR      - CHARACTER*(*): STRING TO BE TESTED
C     LSTR     - INTEGER: NUMBER OF CHARACTERS TO BE TESTED WITHIN STR
C
C   OUTPUT ARGUMENT LIST:
C     ICBFMS   - INTEGER: RETURN CODE:
C                0 - STR IS NOT "MISSING"
C                1 - STR IS "MISSING"
C
C REMARKS:
C    THIS ROUTINE CALLS:        IUPM
C    THIS ROUTINE IS CALLED BY: RDCMPS   RDTREE   UFDUMP
C                               Also called by application programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

	INCLUDE	'bufrlib.prm'

	CHARACTER*(*)	STR

	CHARACTER*8	STRZ
	REAL*8		RL8Z

	CHARACTER*16	ZZ

	CHARACTER*16	ZM_BE
	PARAMETER	( ZM_BE = '202020E076483742' )
C*		10E10 stored as hexadecimal on a big-endian system.

	CHARACTER*16	ZM_LE
	PARAMETER	( ZM_LE = '42374876E8000000' )
C*		10E10 stored as hexadecimal on a little-endian system.

	EQUIVALENCE(STRZ,RL8Z)

C-----------------------------------------------------------------------

	ICBFMS = 0

	NUMCHR = MIN(LSTR,LEN(STR))

C*	Beginning with version 10.2.0 of the BUFRLIB, "missing" strings
C*	have always been explicitly encoded with all bits set to 1,
C*	which is the correct encoding per WMO regulations.  However,
C*	prior to version 10.2.0, the BUFRLIB stored "missing" strings by
C*	encoding the REAL*8 value of 10E10 into the string, so the
C*	following logic attempts to identify some of these earlier
C	cases, at least for strings between 4 and 8 bytes in length.

	IF ( NUMCHR.GE.4 .AND. NUMCHR.LE.8 ) THEN
	    DO II = 1, NUMCHR
		STRZ(II:II) = STR(II:II)
	    END DO
	    WRITE (ZZ,'(Z16.16)') RL8Z
	    I = 2*(8-NUMCHR)+1
	    N = 16
	    IF ( ZZ(I:N).EQ.ZM_BE(I:N) .OR. ZZ(I:N).EQ.ZM_LE(I:N) ) THEN
		ICBFMS = 1
		RETURN
	    END IF
	END IF

C*	Otherwise, the logic below will check for "missing" strings of
C*	any length which are correctly encoded with all bits set to 1,
C*	including those encoded by BUFRLIB version 10.2.0 or later.

	DO I=1,NUMCHR
	   IF ( IUPM(STR(I:I),8).NE.255 ) RETURN
	ENDDO

	ICBFMS = 1

	RETURN
	END
