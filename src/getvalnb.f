C> @file
C> @brief Read one data value from a data subset.

C> This function can be used to read a data value corresponding to
C> a specific occurrence of a mnemonic within a data subset, based on
C> its position relative to a different mnemonic within the subset.
C>
C> <p>The function first searches for a specific occurrence of a pivot
C> mnemonic, counting from the beginning of the subset.  From there,
C> it then searches in either a forward or backward direction for a
C> specific occurrence of a nearby mnemonic, and if found
C> returns the data value from the corresponding location
C> within the subset.
C>
C> @author J. Ator
C> @date 2012-09-12
C>
C> @param[in] LUNIT  -- integer: Fortran logical unit number for
C>                      BUFR file
C> @param[in] TAGPV  -- character*(*): Pivot mnemonic; the subroutine
C>                      will first search for the (NTAGPV)th occurrence
C>                      of this mnemonic, counting from the beginning
C>                      of the overall subset definition
C> @param[in] NTAGPV -- integer: Ordinal occurrence of TAGPV to search for,
C>                      counting from the beginning of the overall
C>                      subset definition
C> @param[in] TAGNB  -- character*(*): Nearby mnemonic; assuming TAGPV is
C>                      successfully found, the subroutine will then search
C>                      nearby for the (NTAGNB)th occurrence of TAGNB and
C>                      return the corresponding value
C> @param[in] NTAGNB -- integer: Ordinal occurrence of TAGNB to search for,
C>                      counting from the location of TAGPV within the
C>                      overall subset definition.  If NTAGNB is positive,
C>                      the subroutine will search in a forward direction
C>                      from the location of TAGPV; otherwise, if NTAGNB is
C>                      negative, it will instead search in a backwards
C>                      direction from the location of TAGPV.
C> @returns getvalnb -- real*8: Value corresponding to (NTAGNB)th occurrence
C>                      of TAGNB.  If for any reason this value cannot be
C>                      located, then the current placeholder value for
C>                      "missing" data will be returned instead.
C>
C> <p>The current placeholder value for "missing" data can be determined
C> via a separate call to function getbmiss().
C>
C> <p>Before calling this function, a BUFR data subset should already be
C> open for reading via a previous call to one of the BUFRLIB
C> [subset-reading subroutines](@ref hierarchy).
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2012-09-12 | J. Ator | Original author |
C> | 2014-10-02 | J. Ator | Modified to use fstag() |
C> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
C> | 2022-10-04 | J. Ator | Added 8-byte wrapper |

	REAL*8 FUNCTION GETVALNB ( LUNIT, TAGPV, NTAGPV, TAGNB, NTAGNB )

        USE MODV_BMISS
	USE MODV_IM8B

	USE MODA_USRINT
	USE MODA_MSGCWD
	USE MODA_TABLES

	INTEGER*8 LUNIT_8, NTAGPV_8, NTAGNB_8

	CHARACTER*(*) TAGPV, TAGNB

	REAL*8 GETVALNB_8

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C	Check for I8 integers.

	IF(IM8B) THEN
	   IM8B=.FALSE.

	   LUNIT_8=LUNIT
	   NTAGPV_8=NTAGPV
	   NTAGNB_8=NTAGNB
	   GETVALNB=GETVALNB_8(LUNIT_8,TAGPV,NTAGPV_8,TAGNB,NTAGNB_8)

	   IM8B=.TRUE.
	   RETURN
	ENDIF

	GETVALNB = BMISS

C	Get LUN from LUNIT.

	CALL STATUS (LUNIT, LUN, IL, IM )
	IF ( IL .GE. 0 ) RETURN
	IF ( INODE(LUN) .NE. INV(1,LUN) ) RETURN

C	Starting from the beginning of the subset, locate the (NTAGPV)th
C	occurrence of TAGPV.

	CALL FSTAG( LUN, TAGPV, NTAGPV, 1, NPV, IRET )
	IF ( IRET .NE. 0 ) RETURN

C	Now, starting from the (NTAGPV)th occurrence of TAGPV, search
C	forward or backward for the (NTAGNB)th occurrence of TAGNB.

	CALL FSTAG( LUN, TAGNB, NTAGNB, NPV, NNB, IRET )
	IF ( IRET .NE. 0 ) RETURN

	GETVALNB = VAL(NNB,LUN)
	    
	RETURN
	END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine getvalnb().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine getvalnb() directly.
C>
C> @author J. Ator
C> @date 2022-10-04
C>
C> @param[in] LUNIT_8  -- integer*8: Fortran logical unit number for
C>                        BUFR file
C> @param[in] TAGPV  -- character*(*): Pivot mnemonic; the subroutine
C>                      will first search for the (NTAGPV_8)th occurrence
C>                      of this mnemonic, counting from the beginning
C>                      of the overall subset definition
C> @param[in] NTAGPV_8 -- integer*8: Ordinal occurrence of TAGPV to search for,
C>                        counting from the beginning of the overall
C>                        subset definition
C> @param[in] TAGNB  -- character*(*): Nearby mnemonic; assuming TAGPV is
C>                      successfully found, the subroutine will then search
C>                      nearby for the (NTAGNB_8)th occurrence of TAGNB and
C>                      return the corresponding value
C> @param[in] NTAGNB_8 -- integer*8: Ordinal occurrence of TAGNB to search for,
C>                        counting from the location of TAGPV within the
C>                        overall subset definition.
C> @returns getvalnb_8 -- real*8: Value corresponding to (NTAGNB_8)th occurrence
C>                        of TAGNB.
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-10-04 | J. Ator    | Original author      |

	REAL*8 FUNCTION GETVALNB_8
     .		( LUNIT_8, TAGPV, NTAGPV_8, TAGNB, NTAGNB_8 )

	INTEGER*8 LUNIT_8, NTAGPV_8, NTAGNB_8

	CHARACTER*(*) TAGPV, TAGNB

	REAL*8 GETVALNB

	LUNIT=LUNIT_8
	NTAGPV=NTAGPV_8
	NTAGNB=NTAGNB_8
	GETVALNB_8=GETVALNB(LUNIT,TAGPV,NTAGPV,TAGNB,NTAGNB)

	RETURN
	END
