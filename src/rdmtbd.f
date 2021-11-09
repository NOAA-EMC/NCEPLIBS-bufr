C> @file
C> @brief Read master Table D information from local file system

C> This subroutine reads master Table D information from two separate
C> ASCII files (one standard and one local) and then merges the
C> output into a single set of arrays.
C>
C> <p>Each of the two ASCII files must already be individually sorted
C> in ascending order with respect to the FXY numbers.
C>
C> @author J. Ator
C> @date 2007-01-19
C>
C> @param[in] LUNSTD -- integer: Fortran logical unit number for
C>                      ASCII file containing standard Table D
C>                      information
C> @param[in] LUNLTD -- integer: Fortran logical unit number for
C>                      ASCII file containing local Table D
C>                      information
C> @param[in] MXMTBD -- integer: Dimensioned size (in integers) of
C>                      merged output arrays; used by the subroutine
C>                      to ensure that it doesn't overflow these
C>                      arrays
C> @param[in] MXELEM -- integer: Maximum number of elements to be
C>                      stored per Table D sequence within merged
C>                      output arrays; used by the subroutine to
C>                      ensure that it doesn't overflow these arrays
C> @param[out] IMT   -- integer: Master table
C>                      - This value is read from both ASCII
C>                        files and must be identical between them.
C> @param[out] IMTV  -- integer: Version number of master table
C>                      - This value is read from the standard ASCII
C>                        file.
C> @param[out] IOGCE -- integer: Originating center
C>                      - This value is read from the local ASCII
C>                        file.
C> @param[out] ILTV  -- integer: Version number of local table
C>                      - This value is read from the local ASCII
C>                        file.
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
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2007-01-19 | J. Ator | Original author |
C> | 2021-01-08 | J. Ator | Modified mstabs array declarations for GNUv10 portability |
C>
	SUBROUTINE RDMTBD ( LUNSTD, LUNLTD, MXMTBD, MXELEM,
     .			    IMT, IMTV, IOGCE, ILTV,
     .			    NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .			    NMELEM, IEFXYN, CEELEM )

	CHARACTER*200	STLINE, LTLINE
	CHARACTER*128	BORT_STR
	CHARACTER*120	CEELEM(MXMTBD,MXELEM)
	CHARACTER*6	CMATCH, ADN30
	CHARACTER*4	CMDSC(*)
	CHARACTER	CMSEQ(120,*)
	CHARACTER	CMMNEM(8,*)

	INTEGER		IMFXYN(*), NMELEM(*),
     .			IEFXYN(MXMTBD,MXELEM)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C	Call WRDLEN to initialize some important information about the
C	local machine, just in case it hasn't already been called.

	CALL WRDLEN

C	Read and parse the header lines of both files.

	CALL GETTBH ( LUNSTD, LUNLTD, 'D', IMT, IMTV, IOGCE, ILTV )

C	Read through the remainder of both files, merging the
C	contents into a unified set of master Table D arrays.

	NMTBD = 0
	CALL GETNTBE ( LUNSTD, ISFXYN, STLINE, IERS )
	CALL GETNTBE ( LUNLTD, ILFXYN, LTLINE, IERL )
	DO WHILE ( ( IERS .EQ. 0 ) .OR. ( IERL .EQ. 0 ) )
	  IF ( ( IERS .EQ. 0 ) .AND. ( IERL .EQ. 0 ) ) THEN
	    IF ( ISFXYN .EQ. ILFXYN ) THEN
	      CMATCH = ADN30 ( ISFXYN, 6 )
	      GOTO 900
	    ELSE IF ( ISFXYN .LT. ILFXYN ) THEN
	      CALL SNTBDE ( LUNSTD, ISFXYN, STLINE, MXMTBD, MXELEM,
     .			    NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .			    NMELEM, IEFXYN, CEELEM )
	      CALL GETNTBE ( LUNSTD, ISFXYN, STLINE, IERS )
	    ELSE
	      CALL SNTBDE ( LUNLTD, ILFXYN, LTLINE, MXMTBD, MXELEM,
     .			    NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .			    NMELEM, IEFXYN, CEELEM )
	      CALL GETNTBE ( LUNLTD, ILFXYN, LTLINE, IERL )
	    ENDIF
	  ELSE IF ( IERS .EQ. 0 ) THEN
	    CALL SNTBDE ( LUNSTD, ISFXYN, STLINE, MXMTBD, MXELEM,
     .			  NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .			  NMELEM, IEFXYN, CEELEM )
	    CALL GETNTBE ( LUNSTD, ISFXYN, STLINE, IERS )
	  ELSE IF ( IERL .EQ. 0 ) THEN
	    CALL SNTBDE ( LUNLTD, ILFXYN, LTLINE, MXMTBD, MXELEM,
     .			  NMTBD, IMFXYN, CMMNEM, CMDSC, CMSEQ,
     .			  NMELEM, IEFXYN, CEELEM )
	    CALL GETNTBE ( LUNLTD, ILFXYN, LTLINE, IERL )
	  ENDIF
	ENDDO

	RETURN
 900	WRITE(BORT_STR,'("BUFRLIB: RDMTBD - STANDARD AND LOCAL'//
     . ' TABLE D FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)')
     .	 CMATCH(1:1), '-', CMATCH(2:3), '-', CMATCH(4:6)	
	CALL BORT(BORT_STR)
	END
