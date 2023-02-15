C> @file
C> @brief Search for a valid FXY number within a character string.
C>
C> @author Ator @date 2007-01-19

C> This function looks for and returns a valid FXY number
C> from within the given input string. The FXY number may be in
C> format of either FXXYYY or F-XX-YYY within the input string, but
C> it is always returned in format FXXYYY upon output.
C>
C> @param[in] STR - character*(*): input string.
C> @param[in] CFXY - character*6: FXY number in format FXXYYY.
C>
C> @return
C> - 0 normal return.
C> - -1 could not find a valid FXY number in STR.
C>
C> @author Ator @date 2007-01-19
        FUNCTION IGETFXY ( STR, CFXY )

        CHARACTER*(*)   STR
        CHARACTER*6     CFXY

        PARAMETER  ( LSTR2 = 120 )
        CHARACTER*(LSTR2)  STR2

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        IGETFXY = -1

        LSTR = LEN ( STR )
        IF ( LSTR .LT. 6 ) RETURN

C       Left-justify a copy of the input string.

        IF ( LSTR .GT. LSTR2 ) THEN
            STR2(1:LSTR2) = STR(1:LSTR2)
        ELSE
            STR2 = STR
        ENDIF
        STR2 = ADJUSTL ( STR2 )
        IF ( STR2 .EQ. ' ' ) RETURN

C       Look for an FXY number.

        IF ( INDEX ( STR2, '-' ) .NE. 0 ) THEN
C           Format of field is F-XX-YYY.
            CFXY(1:1) = STR2(1:1)
            CFXY(2:3) = STR2(3:4)
            CFXY(4:6) = STR2(6:8)
        ELSE
C           Format of field is FXXYYY.
            CFXY = STR2(1:6)
        ENDIF

C       Check that the FXY number is valid.

        IF ( NUMBCK ( CFXY ) .EQ. 0 ) IGETFXY = 0

        RETURN
        END
