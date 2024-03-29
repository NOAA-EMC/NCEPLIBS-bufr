C> @file
C> @brief Get the element name and units associated with a
C> Table B mnemonic.
C>
C> @author J. Ator @date 2014-10-02

C> Given a Table B mnemonic defined in the
C> [DX BUFR Tables](@ref dfbftab) associated with a BUFR file
C> (or in the [master BUFR tables](@ref dfbfmstab), if the file
C> was opened in subroutine openbf() with IO = 'SEC3'), this
C> subroutine returns the element name and units associated
C> with that mnemonic.
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for
C>                     BUFR file
C> @param[in] NEMO   - character*(*): Table B mnemonic
C> @param[out] CELEM - character*55: Element name associated
C>                      with NEMO
C> @param[out] CUNIT - character*24: Units associated with NEMO
C> @param[out] IRET  - integer: return code
C>                      - 0 normal return
C>                      - -1 NEMO could not be found, or some
C>                           other error occurred
C>
C> Logical unit LUNIT should have already been opened for
C> input or output operations via a previous call to subroutine
C> openbf().
C>
C> @author J. Ator @date 2014-10-02

        RECURSIVE SUBROUTINE NEMDEFS ( LUNIT, NEMO, CELEM, CUNIT, IRET )

        use modv_vars, only: im8b

        use moda_tababd

        CHARACTER*1   TAB

        CHARACTER*(*) NEMO, CELEM, CUNIT

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C       Check for I8 integers.

        IF(IM8B) THEN
           IM8B=.FALSE.

           CALL X84 ( LUNIT, MY_LUNIT, 1 )
           CALL NEMDEFS ( MY_LUNIT, NEMO, CELEM, CUNIT, IRET )
           CALL X48 ( IRET, IRET, 1 )

           IM8B=.TRUE.
           RETURN
        ENDIF

        IRET = -1

C       Get LUN from LUNIT.

        CALL STATUS( LUNIT, LUN, IL, IM )
        IF ( IL .EQ. 0 ) RETURN

C       Find the requested mnemonic in the internal Table B arrays.

        CALL NEMTAB( LUN, NEMO, IDN, TAB, ILOC )
        IF ( ( ILOC .EQ. 0 ) .OR. ( TAB .NE. 'B' ) ) RETURN

C       Get the element name and units of the requested mnemonic.

        CELEM = ' '
        LS = MIN(LEN(CELEM),55)
        CELEM(1:LS) = TABB(ILOC,LUN)(16:15+LS)

        CUNIT = ' '
        LS = MIN(LEN(CUNIT),24)
        CUNIT(1:LS) = TABB(ILOC,LUN)(71:70+LS)

        IRET = 0

        RETURN
        END
