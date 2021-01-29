C> @file
C> @brief Test whether a character string is "missing"

C> This function provides a handy way to check whether a
C> character string returned from a previous call to subroutine
C> readlc() was encoded as "missing" (all bits set to 1)
C> within the actual BUFR data subset.
C>
C> @author J. Ator
C> @date 2012-06-07
C>
C> @param[in] STR - character*(*): String
C> @param[in] LSTR - integer: Length of string, i.e. number of
C>                   characters within STR to be tested
C>
C> @returns icbfms  -  integer:
C>                    - 0 = STR is not "missing"
C>                    - 1 = STR is "missing"
C>
C> @remarks
C> - The use of an integer return code allows this function
C> to be called in a logical context from application programs
C> written in C as well as in Fortran.
C>
C> <b>Program history log:</b>
C> - 2012-06-07  J. Ator    -- Original author
C> - 2015-03-10  J. Woollen -- Improved logic for testing legacy cases
C>                           prior to BUFRLIB V10.2.0
C> - 2016-02-12  J. Ator    -- Modified for CRAYFTN compatibility
C>
C> <b>This routine calls:</b> iupm()
C>
C> <b>This routine is called by:</b>
C>                            rdcmps()  rdtree()  ufbdmp()  ufdump()
C>                     <br>Also called by application programs.
C>
	INTEGER FUNCTION ICBFMS ( STR, LSTR )

	INCLUDE	'bufrlib.inc'

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
