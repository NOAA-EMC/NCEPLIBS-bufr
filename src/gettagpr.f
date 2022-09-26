C> @file
C> @brief Get the parent for a specified occurrence of a Table B or
C> Table D mnemonic.

C> This subroutine returns the Table D mnemonic corresponding to the
C> parent sequence of a specified Table B or Table D mnemonic within
C> a data subset definition.
C>
C> @author J. Ator
C> @date 2012-09-12
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
C> <p>A data subset must already be in scope within the BUFRLIB
C> internal arrays for LUNIT, either via a previous call to one
C> of the [subset-reading subroutines](@ref hierarchy)
C> (when reading BUFR data subsets) or via a previous call to one
C> of the [message-writing subroutines](@ref hierarchy)
C> (when writing BUFR data subsets).
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2012-09-12 | J. Ator | Original author |
C> | 2014-10-02 | J. Ator | Modified to use fstag() |
C> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

	SUBROUTINE GETTAGPR ( LUNIT, TAGCH, NTAGCH, TAGPR, IRET )

	USE MODA_USRINT
	USE MODA_MSGCWD
	USE MODA_TABLES
        USE MODV_IM8B

	CHARACTER*(*) TAGCH, TAGPR

	INTEGER*8 LUNIT_8,NTAGCH_8,IRET_8

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C	Check for I8 integers.

	IF(IM8B) THEN
	   IM8B=.FALSE.

	   LUNIT_8=LUNIT
	   NTAGCH_8=NTAGCH
	   CALL GETTAGPR_8 ( LUNIT_8, TAGCH, NTAGCH_8, TAGPR, IRET_8 )
	   IRET=IRET_8

	   IM8B=.TRUE.
	   RETURN
	ENDIF

	IRET = -1

C	Get LUN from LUNIT.

	CALL STATUS( LUNIT, LUN, IL, IM )
	IF ( IL .EQ. 0 ) RETURN
	IF ( INODE(LUN) .NE. INV(1,LUN) ) RETURN

C	Get TAGPR from the (NTAGCH)th occurrence of TAGCH.

	CALL FSTAG( LUN, TAGCH, NTAGCH, 1, NCH, IRET )
	IF ( IRET .NE. 0 ) RETURN

	TAGPR = TAG(JMPB(INV(NCH,LUN)))

	RETURN
	END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine gettagpr().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine gettagpr() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIT_8  -- integer*8: Fortran logical unit number for
C>                        BUFR file
C> @param[in] TAGCH  -- character*(*): Table B or Table D mnemonic
C> @param[in] NTAGCH_8 -- integer*8: Ordinal occurrence of TAGCH for
C>                        which the parent Table D mnemonic is to be
C>                        returned, counting from the beginning of the
C>                        overall subset definition
C> @param[out] TAGPR -- character*(*): Table D mnemonic corresponding
C>                      to parent sequence of (NTAGCH_8)th occurrence
C>                      of TAGCH
C> @param[out] IRET_8  -- integer*8: return code
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

	SUBROUTINE GETTAGPR_8(LUNIT_8,TAGCH,NTAGCH_8,TAGPR,IRET_8)

	CHARACTER*(*) TAGCH, TAGPR

	INTEGER*8 LUNIT_8,NTAGCH_8,IRET_8

	LUNIT=LUNIT_8
	NTAGCH=NTAGCH_8
	CALL GETTAGPR ( LUNIT, TAGCH, NTAGCH, TAGPR, IRET )
	IRET_8=IRET

	RETURN
	END
