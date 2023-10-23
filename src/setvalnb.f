C> @file
C> @brief Write one data value to a data subset.
C>
C> @author J. Ator @date 2016-07-29

C> Write a data value corresponding to
C> a specific occurrence of a mnemonic within a data subset, based on
C> its position relative to a different mnemonic within the subset.
C>
C> The subroutine first searches for a specific occurrence of a pivot
C> mnemonic, counting from the beginning of the subset.  From there,
C> it then searches in either a forward or backward direction for a
C> specific occurrence of a nearby mnemonic, and if found
C> stores the specified data value in the corresponding location
C> within the subset.
C>
C> Before calling this subroutine, a BUFR message should already be
C> opened and initialized for output via a previous call to one of the
C> BUFRLIB [message-writing subroutines](@ref hierarchy).
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
C>                      store R8VAL as the corresponding value
C> @param[in] NTAGNB -- integer: Ordinal occurrence of TAGNB to search for,
C>                      counting from the location of TAGPV within the
C>                      overall subset definition.  If NTAGNB is positive,
C>                      the subroutine will search in a forward direction
C>                      from the location of TAGPV; otherwise, if NTAGNB is
C>                      negative, it will instead search in a backwards
C>                      direction from the location of TAGPV.
C> @param[in] R8VAL  -- real*8: Value to be stored corresponding to
C>                      (NTAGNB)th occurrence of TAGNB within the subset
C> @param[out] IRET  -- integer: return code
C>                        - 0 = R8VAL was successfully stored
C>                        - -1 = the (NTAGNB)th occurence of mnemonic TAGNB
C>                               could not be found, or some other error
C>                               occurred
C>
C> @author J. Ator @date 2016-07-29
        RECURSIVE SUBROUTINE SETVALNB
     .          ( LUNIT, TAGPV, NTAGPV, TAGNB, NTAGNB, R8VAL, IRET )

        USE MODA_USRINT
        USE MODA_MSGCWD
        USE MODA_TABLES
        USE MODV_IM8B

        CHARACTER*(*) TAGPV, TAGNB

        REAL*8  R8VAL

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C       Check for I8 integers.

        IF(IM8B) THEN
           IM8B=.FALSE.

           CALL X84 ( LUNIT, MY_LUNIT, 1 )
           CALL X84 ( NTAGPV, MY_NTAGPV, 1 )
           CALL X84 ( NTAGNB, MY_NTAGNB, 1 )
           CALL SETVALNB ( MY_LUNIT, TAGPV, MY_NTAGPV, TAGNB, MY_NTAGNB,
     .                     R8VAL, IRET )
           CALL X48 ( IRET, IRET, 1 )

           IM8B=.TRUE.
           RETURN
        ENDIF

        IRET = -1

C       Get LUN from LUNIT.

        CALL STATUS (LUNIT, LUN, IL, IM )
        IF ( IL .LE. 0 ) RETURN
        IF ( INODE(LUN) .NE. INV(1,LUN) ) RETURN

C       Starting from the beginning of the subset, locate the (NTAGPV)th
C       occurrence of TAGPV.

        CALL FSTAG( LUN, TAGPV, NTAGPV, 1, NPV, IERFT )
        IF ( IERFT .NE. 0 ) RETURN

C       Now, starting from the (NTAGPV)th occurrence of TAGPV, search
C       forward or backward for the (NTAGNB)th occurrence of TAGNB.

        CALL FSTAG( LUN, TAGNB, NTAGNB, NPV, NNB, IERFT )
        IF ( IERFT .NE. 0 ) RETURN

        IRET = 0
        VAL(NNB,LUN) = R8VAL

        RETURN
        END
