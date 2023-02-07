C> @file
C> @brief Store a master Table B entry into Fortran arrays

C> This subroutine stores an entry that was previously read from an
C> ASCII master Table B file into a set of merged Fortran arrays.
C>
C> @author J. Ator
C> @date 2007-01-19
C>
C> @param[in] IFXYN  -- integer: Bit-wise representation of FXY number
C> @param[in]  LINE  -- character*(*): Table B entry
C> @param[in] MXMTBB -- integer: Dimensioned size (in integers) of
C>                      merged output arrays; used by the subroutine
C>                      to ensure that it doesn't overflow these
C>                      arrays
C> @param[out] NMTBB -- integer: Number of entries in merged output
C>                      arrays
C> @param[out] IMFXYN -- integer(*): Merged array containing bit-wise
C>                       representations of FXY numbers
C> @param[out] CMSCL -- character*4(*): Merged array containing
C>                      scale factors
C> @param[out] CMSREF -- character*12(*): Merged array containing
C>                       reference values
C> @param[out] CMBW  -- character*4(*): Merged array containing
C>                      bit widths
C> @param[out] CMUNIT -- character*24(*): Merged array containing units
C> @param[out] CMMNEM -- character*8(*): Merged array containing
C>                       mnemonics
C> @param[out] CMDSC -- character*4(*): Merged array containing
C>                      descriptor codes
C> @param[out] CMELEM -- character*120(*): Merged array containing
C>                       element names
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2007-01-19 | J. Ator | Original author |
C> | 2021-01-08 | J. Ator | Modified mstabs array declarations for GNUv10 portability |
C> | 2021-05-17 | J. Ator | Allow up to 24 characters in cmunit |
C> | 2021-09-30 | J. Ator | Replace jstchr with Fortran intrinsic adjustl; replace rjust with Fortran intrinsic adjustr |
C>
        SUBROUTINE SNTBBE ( IFXYN, LINE, MXMTBB,
     .                      NMTBB, IMFXYN, CMSCL, CMSREF, CMBW,
     .                      CMUNIT, CMMNEM, CMDSC, CMELEM )

        CHARACTER*(*)   LINE
        CHARACTER*200   TAGS(10), WKTAG
        CHARACTER*128   BORT_STR1, BORT_STR2
        CHARACTER*4     CMDSC(*)
        CHARACTER       CMELEM(120,*)
        CHARACTER       CMUNIT(24,*)
        CHARACTER       CMSREF(12,*)
        CHARACTER       CMMNEM(8,*)
        CHARACTER       CMSCL(4,*), CMBW(4,*)

        INTEGER         IMFXYN(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        IF ( NMTBB .GE. MXMTBB ) GOTO 900
        NMTBB = NMTBB + 1

C       Store the FXY number.  This is the element descriptor.

        IMFXYN ( NMTBB ) = IFXYN

C       Parse the table entry.

        CALL PARSTR ( LINE, TAGS, 10, NTAG, '|', .FALSE. )
        IF ( NTAG .LT. 4 ) THEN
            BORT_STR2 = '                  HAS TOO FEW FIELDS'
            GOTO 901
        ENDIF

C       Scale factor.

        TAGS(2) = ADJUSTL( TAGS(2) )
        IF ( TAGS(2) .EQ. ' ' ) THEN
            BORT_STR2 = '                  HAS MISSING SCALE FACTOR'
            GOTO 901
        ENDIF
        TAGS(2)(1:4) = ADJUSTR( TAGS(2)(1:4) )
        DO II = 1, 4
            CMSCL ( II, NMTBB ) = TAGS(2)(II:II)
        ENDDO

C       Reference value.

        TAGS(3) = ADJUSTL( TAGS(3) )
        IF ( TAGS(3) .EQ. ' ' ) THEN
            BORT_STR2 = '                  HAS MISSING REFERENCE VALUE'
            GOTO 901
        ENDIF
        TAGS(3)(1:12) = ADJUSTR( TAGS(3)(1:12) )
        DO II = 1, 12
            CMSREF ( II, NMTBB ) = TAGS(3)(II:II)
        ENDDO

C       Bit width.

        TAGS(4) = ADJUSTL( TAGS(4) )
        IF ( TAGS(4) .EQ. ' ' ) THEN
            BORT_STR2 = '                  HAS MISSING BIT WIDTH'
            GOTO 901
        ENDIF
        TAGS(4)(1:4) = ADJUSTR( TAGS(4)(1:4) )
        DO II = 1, 4
            CMBW ( II, NMTBB ) = TAGS(4)(II:II)
        END DO

C       Units.  Note that this field is allowed to be blank.

        IF ( NTAG .GT. 4 ) THEN
            TAGS(5) = ADJUSTL( TAGS(5) )
            DO II = 1, 24
                CMUNIT ( II, NMTBB ) = TAGS(5)(II:II)
            ENDDO
        ELSE
            DO II = 1, 24
                CMUNIT ( II, NMTBB ) = ' '
            ENDDO
        ENDIF

C       Comment (additional) fields.  Any of these fields may be blank.

        CMDSC ( NMTBB ) = ' '
        DO II = 1, 8
            CMMNEM ( II, NMTBB ) = ' '
        ENDDO
        DO II = 1, 120
            CMELEM ( II, NMTBB ) = ' '
        ENDDO
        IF ( NTAG .GT. 5 ) THEN
            WKTAG = TAGS(6)
            CALL PARSTR ( WKTAG, TAGS, 10, NTAG, ';', .FALSE. )
            IF ( NTAG .GT. 0 ) THEN
C               The first additional field contains the mnemonic.
                TAGS(1) = ADJUSTL( TAGS(1) )
C               If there is a mnemonic, then make sure it's legal.
                IF ( ( TAGS(1) .NE. ' ' ) .AND.
     .              ( NEMOCK ( TAGS(1) ) .NE. 0 ) ) THEN
                    BORT_STR2 = '                  HAS ILLEGAL MNEMONIC'
                    GOTO 901
                ENDIF
                DO II = 1, 8
                    CMMNEM ( II, NMTBB ) = TAGS(1)(II:II)
                ENDDO
            ENDIF
            IF ( NTAG .GT. 1 ) THEN
C               The second additional field contains descriptor codes.
                TAGS(2) = ADJUSTL( TAGS(2) )
                CMDSC ( NMTBB ) = TAGS(2)(1:4)
            ENDIF
            IF ( NTAG .GT. 2 ) THEN
C               The third additional field contains the element name.
                TAGS(3) = ADJUSTL( TAGS(3) )
                DO II = 1, 120
                    CMELEM ( II, NMTBB ) = TAGS(3)(II:II)
                ENDDO
            ENDIF
        ENDIF

        RETURN
 900    CALL BORT('BUFRLIB: SNTBBE - OVERFLOW OF MERGED ARRAYS')
 901    BORT_STR1 = 'BUFRLIB: SNTBBE - CARD BEGINNING WITH: ' //
     .     LINE(1:20)
        CALL BORT2(BORT_STR1,BORT_STR2)
        END
