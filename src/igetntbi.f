C> @file
C> @brief Get the next index for storing an entry within an internal
C> DX BUFR table

C> This function returns the next available index for storing an
C> entry within a specified internal DX BUFR table.
C>
C> @author J. Ator
C> @date 2009-03-23
C>
C> @param[in]  LUN    - integer: Internal I/O stream index associated
C>                      with BUFR file
C> @param[in]  CTB    - character: Type of internal DX BUFR table for
C>                      which to return the next available index
C>                        - 'A' = Table A
C>                        - 'B' = Table B
C>                        - 'D' = Table D
C> @returns igetntbi  - integer: Next available index for storing an
C>                      entry within CTB
C>
C> <b>Program history log:</b>
C> - 2009-03-23  J. Ator    -- Original author
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>
	FUNCTION IGETNTBI ( LUN, CTB )

	USE MODA_TABABD

	CHARACTER*128 BORT_STR
	CHARACTER*1   CTB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

	IF ( CTB .EQ. 'A' ) THEN
	  IGETNTBI = NTBA(LUN) + 1
	  IMAX = NTBA(0)
	ELSE IF ( CTB .EQ. 'B' ) THEN
	  IGETNTBI = NTBB(LUN) + 1
	  IMAX = NTBB(0)
	ELSE IF ( CTB .EQ. 'D' ) THEN
	  IGETNTBI = NTBD(LUN) + 1
	  IMAX = NTBD(0)
	ENDIF
	IF ( IGETNTBI .GT. IMAX ) GOTO 900

	RETURN
900	WRITE(BORT_STR,'("BUFRLIB: IGETNTBI - NUMBER OF INTERNAL TABLE'
     .    //'",A1," ENTRIES EXCEEDS THE LIMIT (",I4,")")') CTB, IMAX
	CALL BORT(BORT_STR)
	END
