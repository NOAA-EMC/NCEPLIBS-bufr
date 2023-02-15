C> @file
C> @brief Store a master Table D entry into Fortran arrays
C>
C> @author J. Ator @date 2007-01-19

C> This subroutine stores the first line of an entry that was
C> previously read from an ASCII master Table D file into a set of
C> merged Fortran arrays.  It then reads and stores all remaining
C> lines of that same entry into the same merged Fortran arrays.
C>
C> @param[in] LUNT   -- integer: Fortran logical unit number for
C>                      ASCII file containing Table D information
C> @param[in] IFXYN  -- integer: Bit-wise representation of FXY number
C> @param[in]  LINE  -- character*(*): First line of Table D entry
C> @param[in] MXMTBD -- integer: Dimensioned size (in integers) of
C>                      merged output arrays; used by the subroutine
C>                      to ensure that it doesn't overflow these
C>                      arrays
C> @param[in] MXELEM -- integer: Maximum number of elements to be
C>                      stored per Table D sequence within merged
C>                      output arrays; used by the subroutine to
C>                      ensure that it doesn't overflow these arrays
C> @param[out] NMTBD -- integer: Number of entries in merged output
C>                      arrays
C> @param[out] IMFXYN -- integer(*): Merged array containing bit-wise
C>                       representations of FXY numbers
C> @param[out] CMMNEM -- character*8(*): Merged array containing
C>                       mnemonics
C> @param[out] CMDSC -- character*4(*): Merged array containing
C>                      descriptor codes
C> @param[out] CMSEQ -- character*120(*): Merged array containing
C>                      sequence names
C> @param[out] NMELEM -- integer(*): Merged array containing number of
C>                       elements stored for each sequence
C> @param[out] IEFXYN -- integer(*,*): Merged array containing bit-wise
C>                       representations of element FXY numbers
C> @param[out] CEELEM -- character*120(*,*): Merged array containing
C>                       element names
C>
C> @author J. Ator @date 2007-01-19
        SUBROUTINE SNTBDE ( LUNT, IFXYN, LINE, MXMTBD, MXELEM,
     .                      NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .                      NMELEM, IEFXYN, CEELEM )

        CHARACTER*(*)   LINE
        CHARACTER*200   TAGS(10), CLINE
        CHARACTER*128   BORT_STR1, BORT_STR2
        CHARACTER*120   CEELEM(MXMTBD,MXELEM)
        CHARACTER*6     ADN30, ADSC, CLEMON
        CHARACTER*4     CMDSC(*)
        CHARACTER       CMSEQ(120,*)
        CHARACTER       CMMNEM(8,*)

        INTEGER         IMFXYN(*), NMELEM(*),
     .                  IEFXYN(MXMTBD,MXELEM)

        LOGICAL DONE

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        IF ( NMTBD .GE. MXMTBD ) GOTO 900
        NMTBD = NMTBD + 1

C       Store the FXY number.  This is the sequence descriptor.

        IMFXYN ( NMTBD ) = IFXYN

C       Is there any other information within the first line of the
C       table entry?  If so, it follows a "|" separator.

        DO II = 1, 8
            CMMNEM ( II, NMTBD ) = ' '
        ENDDO
        CMDSC ( NMTBD ) = ' '
        DO II = 1, 120
            CMSEQ ( II, NMTBD ) = ' '
        ENDDO
        IPT = INDEX ( LINE, '|' )
        IF ( IPT .NE. 0 ) THEN

C           Parse the rest of the line.  Any of the fields may be blank.

            CALL PARSTR ( LINE(IPT+1:), TAGS, 10, NTAG, ';', .FALSE. )
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
                    CMMNEM ( II, NMTBD ) = TAGS(1)(II:II)
                ENDDO
            ENDIF
            IF ( NTAG .GT. 1 ) THEN
C               The second additional field contains descriptor codes.
                TAGS(2) = ADJUSTL( TAGS(2) )
                CMDSC ( NMTBD ) = TAGS(2)(1:4)
            ENDIF
            IF ( NTAG .GT. 2 ) THEN
C               The third additional field contains the sequence name.
                TAGS(3) = ADJUSTL( TAGS(3) )
                DO II = 1, 120
                    CMSEQ ( II, NMTBD ) = TAGS(3)(II:II)
                ENDDO
            ENDIF
        ENDIF

C       Now, read and parse all remaining lines from this table entry.
C       Each line should contain an element descriptor for the sequence
C       represented by the current sequence descriptor.

        NELEM = 0
        DONE = .FALSE.
        DO WHILE ( .NOT. DONE )
            IF ( IGETNTBL ( LUNT, CLINE ) .NE. 0 ) THEN
                BORT_STR2 = '                  IS INCOMPLETE'
                GOTO 901
            ENDIF
            CALL PARSTR ( CLINE, TAGS, 10, NTAG, '|', .FALSE. )
            IF ( NTAG .LT. 2 ) THEN
                BORT_STR2 = '                  HAS BAD ELEMENT CARD'
                GOTO 901
            ENDIF

C           The second field contains the FXY number for this element.

            IF ( IGETFXY ( TAGS(2), ADSC ) .NE. 0 ) THEN
                BORT_STR2 = '                  HAS BAD OR MISSING' //
     .                      ' ELEMENT FXY NUMBER'
                GOTO 901
            ENDIF
            IF ( NELEM .GE. MXELEM ) GOTO 900
            NELEM = NELEM + 1
            IEFXYN ( NMTBD, NELEM ) = IFXY ( ADSC )

C           The third field (if it exists) contains the element name.

            IF ( NTAG .GT. 2 ) THEN
                TAGS(3) = ADJUSTL( TAGS(3) )
                CEELEM ( NMTBD, NELEM ) = TAGS(3)(1:120)
            ELSE
                CEELEM ( NMTBD, NELEM ) = ' '
            ENDIF

C           Is this the last line for this table entry?

            IF ( INDEX ( TAGS(2), ' >' ) .EQ. 0 ) DONE = .TRUE.
        ENDDO
        NMELEM ( NMTBD ) = NELEM

        RETURN

 900    CALL BORT('BUFRLIB: SNTBDE - OVERFLOW OF MERGED ARRAYS')
 901    CLEMON = ADN30 ( IFXYN, 6 )
        WRITE(BORT_STR1,'("BUFRLIB: SNTBDE - TABLE D ENTRY FOR' //
     .     ' SEQUENCE DESCRIPTOR: ",5A)')
     .     CLEMON(1:1), '-', CLEMON(2:3), '-', CLEMON(4:6)
        CALL BORT2(BORT_STR1,BORT_STR2)
        END
