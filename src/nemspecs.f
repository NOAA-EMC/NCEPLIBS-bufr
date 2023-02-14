C> @file
C> @brief Get the scale factor, reference value and bit width
C> associated with a specified occurrence of a Table B mnemonic.
C>
C> @author J. Ator @date 2014-10-02

C> Given a Table B mnemonic defined within a data subset, this
C> subroutine returns the scale factor, reference value and bit
C> width of a specified occurrence of that mnemonic within the
C> overall data subset definition, counting from the beginning
C> of the subset.
C>
C> The values returned include the application of any Table C
C> operators (e.g. 2-01-YYY, 2-02-YYY, 2-03-YYY, 2-07-YYY,
C> 2-08-YYY) which may be in effect for the specified occurrence
C> of the mnemonic.
C>
C> @param[in] LUNIT  - integer: Fortran logical unit number for
C>                     BUFR file
C> @param[in] NEMO   - character*(*): Table B mnemonic
C> @param[in] NNEMO  - integer: Ordinal occurrence of NEMO for
C>                     which information is to be returned,
C>                     counting from the beginning of the overall
C>                     subset definition
C> @param[out] NSCL  - integer: Scale factor in effect for
C>                     (NNEMO)th occurrence of NEMO
C> @param[out] NREF  - integer: Reference value in effect for
C>                     (NNEMO)th occurrence of NEMO
C> @param[out] NBTS  - integer: Bit width in effect for
C>                     (NNEMO)th occurrence of NEMO
C> @param[out] IRET  - integer: return code
C>                      - 0 normal return
C>                      - -1 NEMO could not be found, or some
C>                           other error occurred
C>
C> A data subset must already be in scope within the BUFRLIB
C> internal arrays for LUNIT, either via a previous call to one
C> of the [subset-reading subroutines](@ref hierarchy)
C> (when reading BUFR data subsets) or via a previous call to one
C> of the [message-writing subroutines](@ref hierarchy)
C> (when writing BUFR data subsets).
C>
C> @author J. Ator @date 2014-10-02

        RECURSIVE SUBROUTINE NEMSPECS
     .          ( LUNIT, NEMO, NNEMO, NSCL, NREF, NBTS, IRET )

        USE MODA_USRINT
        USE MODA_MSGCWD
        USE MODA_TABLES
        USE MODA_NRV203
        USE MODV_IM8B

        CHARACTER*10  TAGN

        CHARACTER*(*) NEMO

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C       Check for I8 integers.

        IF(IM8B) THEN
           IM8B=.FALSE.

           CALL X84(LUNIT,MY_LUNIT,1)
           CALL X84(NNEMO,MY_NNEMO,1)
           CALL NEMSPECS(MY_LUNIT,NEMO,MY_NNEMO,NSCL,NREF,NBTS,IRET)
           CALL X48(NSCL,NSCL,1)
           CALL X48(NREF,NREF,1)
           CALL X48(NBTS,NBTS,1)
           CALL X48(IRET,IRET,1)

           IM8B=.TRUE.
           RETURN
        ENDIF

        IRET = -1

C       Get LUN from LUNIT.

        CALL STATUS( LUNIT, LUN, IL, IM )
        IF ( IL .EQ. 0 ) RETURN
        IF ( INODE(LUN) .NE. INV(1,LUN) ) RETURN

C       Starting from the beginning of the subset, locate the (NNEMO)th
C       occurrence of NEMO.

        CALL FSTAG( LUN, NEMO, NNEMO, 1, NIDX, IERFST )
        IF ( IERFST .NE. 0 ) RETURN

C       Confirm that NEMO is a Table B mnemonic.

        NODE = INV(NIDX,LUN)
        IF ( ( TYP(NODE) .NE. 'NUM' ) .AND. ( TYP(NODE) .NE. 'CHR' ) )
     .      RETURN

C       Get the scale factor, reference value and bit width, including
C       accounting for any Table C operators which may be in scope for
C       this particular occurrence of NEMO.

        IRET = 0

        NSCL = ISC(NODE)
        NBTS = IBT(NODE)
        NREF = IRF(NODE)

        IF ( NNRV .GT. 0 ) THEN

C         There are nodes containing redefined reference values (from
C         one or more 2-03-YYY operators) in the jump/link table, so we
C         need to check if this node is one of them.

          TAGN = ' '
          CALL STRSUC( NEMO, TAGN, LTN )
          IF ( ( LTN .LE. 0 ) .OR. ( LTN .GT. 8 ) ) RETURN

          DO JJ = 1, NNRV
            IF ( ( NODE .NE. INODNRV(JJ) ) .AND.
     .          ( TAGN(1:8) .EQ. TAGNRV(JJ) ) .AND.
     .          ( NODE .GE. ISNRV(JJ) ) .AND.
     .          ( NODE .LE. IENRV(JJ) ) ) THEN
              NREF = NRV(JJ)
              RETURN
            END IF
          END DO

        END IF

        RETURN
        END
