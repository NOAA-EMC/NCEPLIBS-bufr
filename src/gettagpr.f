C> @file
C> @brief Get the parent for a specified occurrence of a Table B or
C> Table D mnemonic.
C>
C> @author J. Ator @date 2012-09-12

C> Return the Table D mnemonic corresponding to the
C> parent sequence of a specified Table B or Table D mnemonic within
C> a data subset definition.
C>
C> @param[in] LUNIT  -- integer: Fortran logical unit number for
C>                      BUFR file
C> @param[in] TAGCH  -- character*(*): Table B or Table D mnemonic
C> @param[in] NTAGCH -- integer: Ordinal occurrence of TAGCH for
C>                      which the parent Table D mnemonic is to be
C>                      returned, counting from the beginning of the
C>                      overall subset definition
C> @param[out] TAGPR -- character*(*): Table D mnemonic corresponding
C>                      to parent sequence of (NTAGCH)th occurrence
C>                      of TAGCH
C> @param[out] IRET  -- integer: return code
C>                      - 0 = normal return
C>                      - -1 = TAGPR could not be found, or some
C>                            other error occurred
C>
C> A data subset must already be in scope within the BUFRLIB
C> internal arrays for LUNIT, either via a previous call to one
C> of the [subset-reading subroutines](@ref hierarchy)
C> (when reading BUFR data subsets) or via a previous call to one
C> of the [message-writing subroutines](@ref hierarchy)
C> (when writing BUFR data subsets).
C>
C> @author J. Ator @date 2012-09-12
        RECURSIVE SUBROUTINE GETTAGPR
     .          ( LUNIT, TAGCH, NTAGCH, TAGPR, IRET )

        USE MODA_USRINT
        USE MODA_MSGCWD
        USE MODA_TABLES
        USE MODV_IM8B

        CHARACTER*(*) TAGCH, TAGPR

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C       Check for I8 integers.

        IF(IM8B) THEN
           IM8B=.FALSE.

           CALL X84 ( LUNIT, MY_LUNIT, 1 )
           CALL X84 ( NTAGCH, MY_NTAGCH, 1 )
           CALL GETTAGPR ( MY_LUNIT, TAGCH, MY_NTAGCH, TAGPR, IRET )
           CALL X48 ( IRET, IRET, 1 )

           IM8B=.TRUE.
           RETURN
        ENDIF

        IRET = -1

C       Get LUN from LUNIT.

        CALL STATUS( LUNIT, LUN, IL, IM )
        IF ( IL .EQ. 0 ) RETURN
        IF ( INODE(LUN) .NE. INV(1,LUN) ) RETURN

C       Get TAGPR from the (NTAGCH)th occurrence of TAGCH.

        CALL FSTAG( LUN, TAGCH, NTAGCH, 1, NCH, IRET )
        IF ( IRET .NE. 0 ) RETURN

        TAGPR = TAG(JMPB(INV(NCH,LUN)))

        RETURN
        END
