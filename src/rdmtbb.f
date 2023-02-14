C> @file
C> @brief Read master Table B information from local file system
C>
C> @author J. Ator @date 2007-01-19

C> This subroutine reads master Table B information from two separate
C> ASCII files (one standard and one local) and then merges the
C> output into a single set of arrays.
C>
C> Each of the two ASCII files must already be individually sorted
C> in ascending order with respect to the FXY numbers.
C>
C> @param[in] LUNSTB -- integer: Fortran logical unit number for
C>                      ASCII file containing standard Table B
C>                      information
C> @param[in] LUNLTB -- integer: Fortran logical unit number for
C>                      ASCII file containing local Table B
C>                      information
C> @param[in] MXMTBB -- integer: Dimensioned size (in integers) of
C>                      merged output arrays; used by the subroutine
C>                      to ensure that it doesn't overflow these
C>                      arrays
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
C>                      mnemonics
C> @param[out] CMDSC -- character*4(*): Merged array containing
C>                      descriptor codes
C> @param[out] CMELEM -- character*120(*): Merged array containing
C>                       element names
C>
C> @author J. Ator @date 2007-01-19
        SUBROUTINE RDMTBB ( LUNSTB, LUNLTB, MXMTBB,
     .                      IMT, IMTV, IOGCE, ILTV,
     .                      NMTBB, IMFXYN, CMSCL, CMSREF, CMBW,
     .                      CMUNIT, CMMNEM, CMDSC, CMELEM )

        CHARACTER*200   STLINE, LTLINE
        CHARACTER*128   BORT_STR
        CHARACTER*6     CMATCH, ADN30
        CHARACTER*4     CMDSC(*)
        CHARACTER       CMELEM(120,*)
        CHARACTER       CMUNIT(24,*)
        CHARACTER       CMSREF(12,*)
        CHARACTER       CMMNEM(8,*)
        CHARACTER       CMSCL(4,*), CMBW(4,*)

        INTEGER         IMFXYN(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       Call WRDLEN to initialize some important information about the
C       local machine, just in case it hasn't already been called.

        CALL WRDLEN

C       Read and parse the header lines of both files.

        CALL GETTBH ( LUNSTB, LUNLTB, 'B', IMT, IMTV, IOGCE, ILTV )

C       Read through the remainder of both files, merging the
C       contents into a unified set of master Table B arrays.

        NMTBB = 0
        CALL GETNTBE ( LUNSTB, ISFXYN, STLINE, IERS )
        CALL GETNTBE ( LUNLTB, ILFXYN, LTLINE, IERL )
        DO WHILE ( ( IERS .EQ. 0 ) .OR. ( IERL .EQ. 0 ) )
          IF ( ( IERS .EQ. 0 ) .AND. ( IERL .EQ. 0 ) ) THEN
            IF ( ISFXYN .EQ. ILFXYN ) THEN
              CMATCH = ADN30 ( ISFXYN, 6 )
              GOTO 900
            ELSE IF ( ISFXYN .LT. ILFXYN ) THEN
              CALL SNTBBE ( ISFXYN, STLINE, MXMTBB,
     .                      NMTBB, IMFXYN, CMSCL, CMSREF, CMBW,
     .                      CMUNIT, CMMNEM, CMDSC, CMELEM )
              CALL GETNTBE ( LUNSTB, ISFXYN, STLINE, IERS )
            ELSE
              CALL SNTBBE ( ILFXYN, LTLINE, MXMTBB,
     .                      NMTBB, IMFXYN, CMSCL, CMSREF, CMBW,
     .                      CMUNIT, CMMNEM, CMDSC, CMELEM )
              CALL GETNTBE ( LUNLTB, ILFXYN, LTLINE, IERL )
            ENDIF
          ELSE IF ( IERS .EQ. 0 ) THEN
            CALL SNTBBE ( ISFXYN, STLINE, MXMTBB,
     .                    NMTBB, IMFXYN, CMSCL, CMSREF, CMBW,
     .                    CMUNIT, CMMNEM, CMDSC, CMELEM )
            CALL GETNTBE ( LUNSTB, ISFXYN, STLINE, IERS )
          ELSE IF ( IERL .EQ. 0 ) THEN
            CALL SNTBBE ( ILFXYN, LTLINE, MXMTBB,
     .                    NMTBB, IMFXYN, CMSCL, CMSREF, CMBW,
     .                    CMUNIT, CMMNEM, CMDSC, CMELEM )
            CALL GETNTBE ( LUNLTB, ILFXYN, LTLINE, IERL )
          ENDIF
        ENDDO

        RETURN
 900    WRITE(BORT_STR,'("BUFRLIB: RDMTBB - STANDARD AND LOCAL'//
     . ' TABLE B FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)')
     .   CMATCH(1:1), '-', CMATCH(2:3), '-', CMATCH(4:6)
        CALL BORT(BORT_STR)
        END
