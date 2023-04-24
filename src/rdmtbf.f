C> @file
C> @brief Read master Code/Flag table information from local file system.
C>
C> @author J. Ator @date 2017-10-17

C> This subroutine reads master Code/Flag table information from two
C> separate ASCII files (one standard and one local) and then merges the
C> output into a single set of arrays.
C>
C> Each of the two ASCII files must already be individually sorted
C> in ascending order with respect to the FXY numbers.
C>
C> @param[in] LUNSTF -- integer: Fortran logical unit number for
C>                      ASCII file containing standard Code/Flag table
C>                      information
C> @param[in] LUNLTF -- integer: Fortran logical unit number for
C>                      ASCII file containing local Code/Flag table
C>                      information
C>
C> @author J. Ator @date 2017-10-17
        SUBROUTINE RDMTBF ( LUNSTF, LUNLTF )

        use bufrlib

        CHARACTER*160   STLINE, LTLINE
        CHARACTER*128   BORT_STR
        CHARACTER*6     CMATCH, ADN30

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       Call WRDLEN to initialize some important information about the
C       local machine, just in case it hasn't already been called.

        CALL WRDLEN

C       Initialize the internal memory structure, including allocating
C       space for it in case this hasn't already been done.

        CALL INITTBF_C

C       Read and parse the header lines of both files.

        CALL GETTBH ( LUNSTF, LUNLTF, 'F', IMT, IMTV, IOGCE, ILTV )

C       Read through the remainder of both files, merging the
C       contents into a unified internal memory structure.

        CALL GETNTBE ( LUNSTF, ISFXYN, STLINE, IERS )
        CALL GETNTBE ( LUNLTF, ILFXYN, LTLINE, IERL )
        DO WHILE ( ( IERS .EQ. 0 ) .OR. ( IERL .EQ. 0 ) )
          IF ( ( IERS .EQ. 0 ) .AND. ( IERL .EQ. 0 ) ) THEN
            IF ( ISFXYN .EQ. ILFXYN ) THEN
              CMATCH = ADN30 ( ISFXYN, 6 )
              GOTO 900
            ELSE IF ( ISFXYN .LT. ILFXYN ) THEN
              CALL SNTBFE ( LUNSTF, ISFXYN )
              CALL GETNTBE ( LUNSTF, ISFXYN, STLINE, IERS )
            ELSE
              CALL SNTBFE ( LUNLTF, ILFXYN )
              CALL GETNTBE ( LUNLTF, ILFXYN, LTLINE, IERL )
            ENDIF
          ELSE IF ( IERS .EQ. 0 ) THEN
            CALL SNTBFE ( LUNSTF, ISFXYN )
            CALL GETNTBE ( LUNSTF, ISFXYN, STLINE, IERS )
          ELSE IF ( IERL .EQ. 0 ) THEN
            CALL SNTBFE ( LUNLTF, ILFXYN )
            CALL GETNTBE ( LUNLTF, ILFXYN, LTLINE, IERL )
          ENDIF
        ENDDO

C       Sort the contents of the internal memory structure.

        CALL SORTTBF_C

        RETURN
 900    WRITE(BORT_STR,'("BUFRLIB: RDMTBF - STANDARD AND LOCAL'//
     . ' CODE/FLAG TABLE FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)')
     .   CMATCH(1:1), '-', CMATCH(2:3), '-', CMATCH(4:6)
        CALL BORT(BORT_STR)
        END
