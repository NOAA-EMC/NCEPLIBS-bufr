C> @file
C> @brief Store a master Code/Flag table entry into internal memory.
C>
C> @author J. Ator @date 2017-11-02

C> Read an entire entry from a previously-opened
C> ASCII master Code/Flag table file, then store the information
C> into an internal memory structure.
C>
C> @param[in] LUNT   - integer: Fortran logical unit number for
C> ASCII file containing Code/Flag table information.
C> @param[in] IFXYN  - integer: WMO bit-wise representation of FXY number.
C>
C> @author J. Ator @date 2017-11-02
        SUBROUTINE SNTBFE ( LUNT, IFXYN )

        use bufrlib

        CHARACTER*160   CLINE, TAGS(4), CDSTR(2), ADSC(10), CVAL(25)
        CHARACTER*128   BORT_STR1, BORT_STR2
        CHARACTER*6     ADN30, CLEMON, CDSC
        DIMENSION       IDFXY(10), IDVAL(25)

        LOGICAL DONE, LSTNBLK

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       We already have the FXY number.  Now we need to read and parse
C       all of the remaining lines from the table entry for this FXY
C       number.  The information for each individual code figure or bit
C       number will then be stored as a separate entry within the
C       internal memory structure.

        DONE = .FALSE.
        NIDFXY = 0
        NIDVAL = 0

        DO WHILE ( .NOT. DONE )

            IF ( IGETNTBL ( LUNT, CLINE ) .NE. 0 ) THEN
                BORT_STR2 = '                  IS INCOMPLETE'
                GOTO 900
            ENDIF

            CALL PARSTR ( CLINE, TAGS, 4, NTAG, '|', .FALSE. )
            IF ( ( NTAG .LT. 2 ) .OR. ( NTAG .GT. 3 ) ) THEN
                BORT_STR2 = '                  HAS BAD CARD'
                GOTO 900
            ENDIF

            IF ( NTAG .EQ. 2 ) THEN

C               This line contains a list of dependencies.

                CALL PARSTR ( TAGS(2), CDSTR, 2, NTAG, '=', .FALSE. )
                IF ( NTAG .NE. 2 ) THEN
                  BORT_STR2 = '           HAS BAD DEPENDENCY CARD'
                  GOTO 900
                ENDIF

C               Parse the list of FXY numbers.

                CALL PARSTR ( CDSTR(1), ADSC, 10, NIDFXY, ',', .FALSE. )
                IF ( NIDFXY .EQ. 0 ) THEN
                  BORT_STR2 = '        HAS BAD DEPENDENCY LIST (FXY)'
                  GOTO 900
                ENDIF
                DO II = 1, NIDFXY
                  IF ( IGETFXY ( ADSC(II), CDSC ) .NE. 0 ) THEN
                    BORT_STR2 = '        HAS BAD DEPENDENCY (FXY)'
                    GOTO 900
                  ENDIF
                  IDFXY(II) = IFXY( CDSC )
                ENDDO

C               Parse the list of values.

                CALL PARSTR ( CDSTR(2), CVAL, 25, NIDVAL, ',', .FALSE. )
                IF ( NIDVAL .EQ. 0 ) THEN
                  BORT_STR2 = '        HAS BAD DEPENDENCY LIST (VAL)'
                  GOTO 900
                ENDIF
                DO II = 1, NIDVAL
                  CVAL(II) = ADJUSTL( CVAL(II) )
                  CALL STRNUM ( CVAL(II), IVAL, IER )
                  IDVAL(II) = IVAL
                ENDDO

            ELSE

C               This line contains a value (code figure or bit number)
C               and corresponding meaning.

                IPT = INDEX ( TAGS(2), ' >' )
                IF ( IPT .EQ. 0 ) THEN

C                 This is the last line for this table entry.

                  DONE = .TRUE.
                ELSE
                  TAGS(2)(IPT+1:IPT+1) = ' '
                ENDIF

                TAGS(2) = ADJUSTL( TAGS(2) )
                CALL STRNUM ( TAGS(2), IVAL, IER )

C               Find the last non-blank character in the meaning string.

                TAGS(3) = ADJUSTL( TAGS(3) )
                LT3 = LEN(TAGS(3))
                LSTNBLK = .FALSE.
                DO WHILE ( ( LT3 .GT. 0 ) .AND. ( .NOT. LSTNBLK ) )
                  IF ( TAGS(3)(LT3:LT3) .NE. ' ' ) THEN
                    LSTNBLK = .TRUE.
                  ELSE
                    LT3 = LT3 - 1
                  ENDIF
                ENDDO

C               Store the information for this value within the internal
C               memory structure.

                IF ( ( NIDFXY .EQ. 0 ) .AND. ( NIDVAL .EQ. 0 ) ) THEN
                  CALL STRTBFE_C ( IFXYN, IVAL, TAGS(3), LT3, -1, -1 )
                ELSE
                  DO II = 1, NIDFXY
                    DO JJ = 1, NIDVAL
                      CALL STRTBFE_C ( IFXYN, IVAL, TAGS(3), LT3,
     +                                 IDFXY(II), IDVAL(JJ) )
                    ENDDO
                  ENDDO
                ENDIF

            ENDIF

        ENDDO

        RETURN

 900    CLEMON = ADN30 ( IFXYN, 6 )
        WRITE(BORT_STR1,'("BUFRLIB: SNTBFE - TABLE F ENTRY FOR' //
     .     ' ELEMENT DESCRIPTOR: ",5A)')
     .     CLEMON(1:1), '-', CLEMON(2:3), '-', CLEMON(4:6)
        CALL BORT2(BORT_STR1,BORT_STR2)
        END
