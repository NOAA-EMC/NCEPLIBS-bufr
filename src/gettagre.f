C> @file
C> @brief Check whether a Table B mnemonic references another
C> Table B mnemonic via an internal bitmap.

C> This subroutine determines whether a specified Table B mnemonic
C> references another Table B mnemonic within the same data subset
C> via an internal bitmap, and if so returns the referenced
C> mnemonic and its location within the subset.
C>
C> @author J. Ator
C> @date 2016-06-07
C>
C> @param[in] LUNIT  -- integer: Fortran logical unit number for
C>                      BUFR file
C> @param[in] TAGI   -- character*(*): Table B mnemonic
C> @param[in] NTAGI  -- integer: Ordinal occurrence of TAGI for
C>                      which TAGRE is to be returned, counting from
C>                      the beginning of the overall subset definition
C> @param[out] TAGRE -- character*(*): Table B mnemonic referenced by
C>                      TAGI via an internal bitmap
C> @param[out] NTAGRE -- integer: Ordinal occurrence of TAGRE
C>                       referenced by (NTAGI)th occurrence of TAGI,
C>                       counting from the beginning of the overall
C>                       subset definition
C> @param[out] IRET  -- integer: return code
C>                      - 0 = normal return
C>                      - -1 = TAGRE could not be found, or some
C>                             other error occurred
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
C> | 2016-06-07 | J. Ator | Original author |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

	SUBROUTINE GETTAGRE ( LUNIT, TAGI, NTAGI, TAGRE, NTAGRE, IRET )

	USE MODA_USRINT
	USE MODA_MSGCWD
	USE MODA_TABLES
        USE MODV_IM8B

	CHARACTER*(*) TAGI, TAGRE

	INTEGER*8 LUNIT_8,NTAGI_8,NTAGRE_8,IRET_8

	CHARACTER*10 TAGTMP

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C	Check for I8 integers.

	IF(IM8B) THEN
	   IM8B=.FALSE.

	   LUNIT_8=LUNIT
	   NTAGI_8=NTAGI
	   CALL GETTAGRE_8(LUNIT_8,TAGI,NTAGI_8,TAGRE,NTAGRE_8,IRET_8)
	   NTAGRE=NTAGRE_8
	   IRET=IRET_8

	   IM8B=.TRUE.
	   RETURN
	ENDIF

	IRET = -1

C	Get LUN from LUNIT.

	CALL STATUS( LUNIT, LUN, IL, IM )
	IF ( IL .EQ. 0 ) RETURN
	IF ( INODE(LUN) .NE. INV(1,LUN) ) RETURN

C	Get TAGRE and NTAGRE from the (NTAGI)th occurrence of TAGI.

	CALL FSTAG( LUN, TAGI, NTAGI, 1, NI, IRET )
	IF ( IRET .NE. 0 ) RETURN
	NRE = NRFELM(NI,LUN)
	IF ( NRE .GT. 0 ) THEN
	    IRET = 0
	    TAGRE = TAG(INV(NRE,LUN))
	    CALL STRSUC( TAGRE, TAGTMP, LTRE )
	    NTAGRE = 0
	    DO II = 1, NRE
		IF ( TAG(INV(II,LUN))(1:LTRE) .EQ. TAGRE(1:LTRE) ) THEN
		  NTAGRE = NTAGRE + 1
		END IF
	    END DO
	END IF

	RETURN
	END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine gettagre().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine gettagre() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIT_8 -- integer*8: Fortran logical unit number for
C>                       BUFR file
C> @param[in] TAGI   -- character*(*): Table B mnemonic
C> @param[in] NTAGI_8 -- integer*8: Ordinal occurrence of TAGI for
C>                       which TAGRE is to be returned, counting from
C>                       the beginning of the overall subset definition
C> @param[out] TAGRE -- character*(*): Table B mnemonic referenced by
C>                      TAGI via an internal bitmap
C> @param[out] NTAGRE_8 -- integer*8: Ordinal occurrence of TAGRE
C>                         referenced by (NTAGI_8)th occurrence of TAGI,
C>                         counting from the beginning of the overall
C>                         subset definition
C> @param[out] IRET_8 -- integer*8: return code
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

	SUBROUTINE GETTAGRE_8(LUNIT_8,TAGI,NTAGI_8,TAGRE,NTAGRE_8,IRET_8)

	INTEGER*8 LUNIT_8,NTAGI_8,NTAGRE_8,IRET_8

	LUNIT=LUNIT_8
	NTAGI=NTAGI_8
	CALL GETTAGRE(LUNIT,TAGI,NTAGI,TAGRE,NTAGRE,IRET)
	NTAGRE_8=NTAGRE
	IRET_8=IRET

	RETURN
	END
