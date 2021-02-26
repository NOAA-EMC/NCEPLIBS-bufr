C> @file
C> @brief Read the first line of the next entry from an ASCII master
C> table B, table D or Code/Flag file.
	
C> This subroutine gets the first line of the next entry in
C> the specified ASCII master table B, table D or table F (Code/Flag)
C> file. This line contains, among other things, the FXY number
C> corresponding to this entry.
C>
C> @author J. Ator
C> @date 2007-01-19
C>
C> @param[in] LUNT integer: Fortran logical unit number of ASCII file
C> containing master table B, table D or table F (Code/Flag) information
C> @param[out] IFXYN integer: bit-wise representation of FXY number for
C> next table entry
C> @param[out] LINE character*(*): first line of next table entry
C> @param[out] IRET integer: return code:
C> -  0 = normal return
C> -  -1 = end-of-file encountered while reading from LUNT
C> -  -2 = I/O error encountered while reading from LUNT
C>
C> <b>Program history log:</b>
C> - 2007-01-19  J. Ator    -- Original author
C>
	SUBROUTINE GETNTBE ( LUNT, IFXYN, LINE, IRET )

	CHARACTER*(*)	LINE
	CHARACTER*128	BORT_STR1, BORT_STR2
	CHARACTER*20	TAGS(4)
	CHARACTER*6	ADSC

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Get the first line of the next entry in the file.

	IRET = IGETNTBL ( LUNT, LINE )
	IF ( IRET .EQ. 0 ) THEN

C	    The first field within this line should contain the
C	    FXY number.

	    CALL PARSTR ( LINE(1:20), TAGS, 4, NTAG, '|', .FALSE. )
	    IF ( NTAG .LT. 1 ) GOTO 900
	    IF ( IGETFXY ( TAGS(1), ADSC ) .NE. 0 ) GOTO 900

C	    Store the bit-wise representation of the FXY number.

	    IFXYN = IFXY ( ADSC )
	ENDIF

	RETURN

 900    BORT_STR1 = 'BUFRLIB: GETNTBE - CARD BEGINNING WITH: ' //
     .     LINE(1:20)
        BORT_STR2 = '                  HAS BAD OR MISSING FXY NUMBER'
	CALL BORT2(BORT_STR1,BORT_STR2)

	END
