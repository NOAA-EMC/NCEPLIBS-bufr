C> @file
C> @brief Write DX BUFR tables messages to the beginning of an output BUFR file.
c>
C> @author Woollen @date 1994-01-06

C> This subroutine writes BUFR table (dictionary) messages to
C> the beginning of an output BUFR file in lunit. The table messages
C> are read from arrays in internal memory (module tababd).
C> An initial call to BUFR archive library subroutine readdx() generates
C> these internal arrays.
C>
C>
C> @param[in]  lunit -- integer: fortran logical unit number for BUFR file being written
C> @param[in]  lun   -- integer: i/o stream index into internal memory arrays
C> @param[in]  lundx -- integer: fortran logical unit number containing
C>                               dictionary table information to be used (by readdx()) to
C>                               create internal tables written to lunit;
C>                               if set equal to lunit, this subroutine calls bort()
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
