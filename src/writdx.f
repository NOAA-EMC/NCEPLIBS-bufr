C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS SUBROUTINE WRITES BUFR TABLE (DICTIONARY) MESSAGES TO
C>   THE BEGINNING OF AN OUTPUT BUFR FILE IN LUNIT.  THE TABLE MESSAGES
C>   ARE READ FROM ARRAYS IN INTERNAL MEMORY (MODULE TABABD).
C>   AN INITIAL CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE READDX GENERATES
C>   THESE INTERNAL ARRAYS.
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C>                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"
C> 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C>                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C>                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C>                           BUFR FILES UNDER THE MPI)
C> 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           10,000 TO 20,000 BYTES
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C>                           TERMINATES ABNORMALLY
C> 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C>                           20,000 TO 50,000 BYTES
C> 2009-03-23  J. ATOR    -- USE WRDXTB
C>
C> USAGE:    CALL WRITDX (LUNIT, LUN, LUNDX)
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>                BEING WRITTEN
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>                (ASSOCIATED WITH FILE CONNECTED TO LOGICAL UNIT LUNIT)
C>     LUNDX    - INTEGER: FORTRAN LOGICAL UNIT NUMBER CONTAINING
C>                DICTIONARY TABLE INFORMATION TO BE USED (BY READDX) TO
C>                CREATE INTERNAL TABLES WRITTEN TO LUNIT (SEE READDX);
C>                IF SET EQUAL TO LUNIT, THIS SUBROUTINE CALLS BORT
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     READDX   WRDXTB
C>    THIS ROUTINE IS CALLED BY: OPENBF
C>                               Normally not called by any application
C>                               programs.
C>
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
