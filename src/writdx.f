C> @file
C> @brief Writes bufr table (dictionary) messages to the beginning of an output bufr file
c>
C> <b>Program History Log:</b>
C> Date | Programmer | Comments |
C> -----|------------|----------|
C> 1994-01-06 | J. Woollen  |  original author 
C> 1995-06-28 | J. Woollen  |  increased the size of internal bufr table arrays in order to handle bigger files 
C> 1998-07-08 | J. Woollen  |  replaced call to cray library routine "abort" with call to new internal bufrlib routine "bort" 
C> 1999-11-18 | J. Woollen  |  the number of bufr files which can be opened at one time increased from 10 to 32 
C> 2000-09-19 | J. Woollen  |  maximum message length increased from 10,000 to 20,000 bytes 
C> 2003-11-04 | S. Bender   |  added remarks/bufrlib routine interdependencies 
C> 2003-11-04 | D. Keyser   |  unified/portable for wrf 
C> 2004-08-09 | J. Ator     |  maximum message length increased from 20,000 to 50,000 bytes 
C> 2009-03-23 | J. Ator     |  use wrdxtb 
C>
C> @author Woollen @date 1994-01-06

C> This subroutine writes bufr table (dictionary) messages to
C> the beginning of an output bufr file in lunit.  the table messages
C> are read from arrays in internal memory (module tababd).
C> an initial call to bufr archive library subroutine readdx generates
C> these internal arrays.
C>
C>
C> @param[in]  lunit -- integer: fortran logical unit number for bufr file being written
C> @param[in]  lun   -- integer: i/o stream index into internal memory arrays
C> @param[in]  lundx -- integer: fortran logical unit number containing
C>                               dictionary table information to be used (by readdx) to
C>                               create internal tables written to lunit (see readdx);
C>                               if set equal to lunit, this subroutine calls bort
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
