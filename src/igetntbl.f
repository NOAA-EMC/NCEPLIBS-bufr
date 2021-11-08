C> @file
C> @brief Read the next line from a master table B, table D or Code/Flag
C> table file

C> This subroutine reads the next line from an ASCII master table B,
C> table D or Code/Flag table file, ignoring any blank lines or comment
C> lines in the process.
C>
C> @author J. Ator
C> @date 2007-01-19
C>
C> @param[in] LUNT   -- integer: Fortran logical unit number for
C>                      ASCII file containing table information
C> @param[out] LINE  -- character*(*): Next non-blank, non-comment line
C>                      that was read from LUNT
C> @returns igetntbl -- integer: return code
C>                        -  0 = normal return
C>                        - -1 = end-of-file encountered while reading
C>                               from LUNT
C>                        - -2 = I/O error encountered while reading
C>                               from LUNT
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2007-01-19 | J. Ator | Original author |
C>
	FUNCTION IGETNTBL ( LUNT, LINE )

	CHARACTER*(*)	LINE

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

  10	READ ( LUNT, '(A)', END=100, ERR=200 ) LINE
	IF ( ( LINE .EQ. ' ' ) .OR. ( LINE(1:1) .EQ. '#' ) ) GOTO 10
	IF ( LINE(1:3) .EQ. 'END' ) GOTO 100

	IGETNTBL = 0
	RETURN

 100	IGETNTBL = -1
	RETURN

 200	IGETNTBL = -2
	RETURN

	END
