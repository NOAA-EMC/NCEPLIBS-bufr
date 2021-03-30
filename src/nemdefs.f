C> @file
C> @brief Get the element name and units associated with a
C> Table B mnemonic.

C> Given a Table B mnemonic defined in the
C> [DX BUFR Tables](@ref dfbftab) associated with a BUFR file
C> (or in the [master BUFR tables](@ref dfbfmstab), if the file
C> was opened in subroutine openbf() with IO = 'SEC3'), this
C> subroutine returns the element name and units associated
C> with that mnemonic.
C>
C> @author J. Ator
C> @date 2014-10-02
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for
C>                     BUFR file
C> @param[in] NEMO   - character*(*): Table B mnemonic
C> @param[out] CELEM - character*55: Element name associated
C>                     with NEMO
C> @param[out] CUNIT - character*24: Units associated with NEMO
C> @param[out] IRET  - integer: return code
C>                     - 0 = normal return
C>                     - -1 = NEMO could not be found, or some
C>                            other error occurred
C>
C> <p>Logical unit LUNIT should have already been opened for
C> input or output operations via a previous call to subroutine
C> openbf().
C>
C> <b>Program history log:</b>
C> - 2014-10-02  J. Ator    -- Original version
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
	SUBROUTINE NEMDEFS ( LUNIT, NEMO, CELEM, CUNIT, IRET )

	USE MODA_TABABD

	CHARACTER*1   TAB

	CHARACTER*(*) NEMO, CELEM, CUNIT

C----------------------------------------------------------------------
C----------------------------------------------------------------------

	IRET = -1

C	Get LUN from LUNIT.

	CALL STATUS( LUNIT, LUN, IL, IM )
	IF ( IL .EQ. 0 ) RETURN

C	Find the requested mnemonic in the internal Table B arrays.

	CALL NEMTAB( LUN, NEMO, IDN, TAB, ILOC )
	IF ( ( ILOC .EQ. 0 ) .OR. ( TAB .NE. 'B' ) ) RETURN

C	Get the element name and units of the requested mnemonic.

	CELEM = ' '
	LS = MIN(LEN(CELEM),55)
	CELEM(1:LS) = TABB(ILOC,LUN)(16:15+LS)

	CUNIT = ' '
	LS = MIN(LEN(CUNIT),24)
	CUNIT(1:LS) = TABB(ILOC,LUN)(71:70+LS)

	IRET = 0

	RETURN
	END
