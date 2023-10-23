C> @file
C> @brief Write DX BUFR tables messages to the beginning of a
C> BUFR file.
C>
C> @author Woollen @date 1994-01-06

C> Write DX BUFR table (dictionary) messages to
C> the beginning of an output BUFR file in lunit. The table messages
C> are read from arrays in internal memory (module @ref moda_tababd).
C> An initial call to subroutine readdx() generates
C> these internal arrays.
C>
C> @param[in] lunit - integer: Fortran logical unit number for BUFR
C> file being written.
C> @param[in] lun - integer: file ID of open BUFR file.
C> @param[in] lundx - integer: fortran logical unit number containing
C> dictionary table information to be used (by readdx()) to create
C> internal tables written to lunit; if set equal to lunit, this
C> subroutine calls bort().
C>
C> @author Woollen @date 1994-01-06

      SUBROUTINE WRITDX(LUNIT,LUN,LUNDX)

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK UNITS, TABLE MUST BE COMING FROM AN INPUT FILE
C  ----------------------------------------------------

      IF(LUNIT.EQ.LUNDX) GOTO 900

C  MUST FIRST CALL READDX TO GENERATE INTERNAL DICTIONARY TABLE ARRAYS
C  -------------------------------------------------------------------

      CALL READDX(LUNIT,LUN,LUNDX)

C  NOW CALL WRDXTB TO WRITE OUT DICTIONARY MESSAGES FROM THESE ARRAYS
C  ------------------------------------------------------------------

      CALL WRDXTB(LUNIT,LUNIT)

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: WRITDX - FILES CONTAINING BUFR DATA '//
     . 'AND DICTIONARY TABLE CANNOT BE THE SAME (HERE BOTH SHARE '//
     . 'FORTRAN UNIT NUMBER ",I3,")")') LUNIT
      CALL BORT(BORT_STR)
      END
