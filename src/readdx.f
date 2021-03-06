C> @file
C> @author WOOLLEN @date 1994-01-06
      
C> THIS SUBROUTINE GENERATES INTERNAL ARRAYS CONTAINING BUFR
C>   DICTIONARY TABLES WHICH ARE NEEDED TO READ, WRITE, INITIALIZE OR
C>   APPEND A BUFR FILE.  THE INFORMATION USED TO CREATE THE INTERNAL
C>   DICTIONARY TABLE ARRAYS (IN MODULE TABABD) AND THE DICTIONARY
C>   MESSAGE CONTROL WORD PARTITION ARRAYS (IN MODULE MSGCWD)
C>   (WHICH ARE ALWAYS THEN ASSOCIATED WITH THE BUFR FILE IN LUNIT)
C>   MAY COME FROM AN EXTERNAL, USER-SUPPLIED, BUFR DICTIONARY
C>   TABLE FILE IN CHARACTER FORMAT (I.E., A BUFR MNEMONIC TABLE), FROM
C>   THE BUFR FILE BEING ACTED UPON (IN WHICH CASE THE FILE MUST BE
C>   OPENED FOR INPUT PROCESSING AND POSITIONED AT A DICTIONARY TABLE
C>   MESSAGE SOMEWHERE IN THE FILE), OR FROM ANOTHER CURRENTLY OPENED
C>   AND DEFINED BUFR FILE.  IN THIS LATTER CASE, THE BUFR FILE WOULD
C>   MOST LIKELY BE OPENED FOR INPUT, HOWEVER THERE IS NOTHING
C>   PREVENTING THE USE OF A FILE OPEN FOR OUTPUT AS LONG AS IT IS
C>   ASSOCIATED WITH INTERNAL DICTIONARY ARRAYS THAT CAN BE USED.
C>
C> PROGRAM HISTORY LOG:
C> 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C> 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C>                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C>                           ROUTINE "BORT"
C> 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C>                           INTERDEPENDENCIES
C> 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C>                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C>                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C>                           TERMINATES ABNORMALLY OR FOR INFORMATIONAL
C>                           PURPOSES
C> 2009-04-21  J. ATOR    -- USE ERRWRT
C>
C> USAGE:    CALL READDX (LUNIT, LUN, LUNDX)
C>   INPUT ARGUMENT LIST:
C>     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C>                BEING READ, WRITTEN, INITIALIZED OR APPENDED
C>     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C>                (ASSOCIATED WITH FILE CONNECTED TO LOGICAL UNIT LUNIT)
C>     LUNDX    - INTEGER: FORTRAN LOGICAL UNIT NUMBER CONTAINING
C>                DICTIONARY TABLE INFORMATION TO BE USED IN READING/
C>                WRITING FROM/TO LUNIT (DEPENDING ON THE CASE); MAY BE
C>                SET EQUAL TO LUNIT IF DICTIONARY TABLE INFORMATION IS
C>                ALREADY EMBEDDED IN LUNIT (BUT ONLY IF LUNIT IS BEING
C>                READ)
C>
C> REMARKS:
C>    THIS ROUTINE CALLS:        BORT     CPBFDX   ERRWRT   MAKESTAB
C>                               RDBFDX   RDUSDX   STATUS
C>    THIS ROUTINE IS CALLED BY: OPENBF   WRITDX
C>                               Normally not called by any application
C>                               programs.
C>
      SUBROUTINE READDX(LUNIT,LUN,LUNDX)



      COMMON /QUIET/ IPRT

      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  GET THE BUFR STATUS OF UNIT LUNDX
C  ---------------------------------

      CALL STATUS(LUNDX,LUD,ILDX,IMDX)

C  READ A DICTIONARY TABLE FROM THE INDICATED SOURCE
C  -------------------------------------------------

      IF (LUNIT.EQ.LUNDX) THEN
c  .... Source is input BUFR file in LUNIT
         IF(IPRT.GE.2) THEN
          CALL ERRWRT('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
          WRITE ( UNIT=ERRSTR, FMT='(A,A,I3,A)' )
     .     'BUFRLIB: READDX - READING BUFR DICTIONARY TABLE FROM ',
     .     'INPUT BUFR FILE IN UNIT ', LUNDX, ' INTO INTERNAL ARRAYS'
          CALL ERRWRT(ERRSTR)
          CALL ERRWRT('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
          CALL ERRWRT(' ')
         ENDIF
         REWIND LUNIT
         CALL RDBFDX(LUNIT,LUN)
      ELSEIF(ILDX.EQ.-1) THEN
c  .... Source is input BUFR file in LUNDX
c  .... BUFR file in LUNIT may be input or output
         IF(IPRT.GE.2) THEN
          CALL ERRWRT('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
          WRITE ( UNIT=ERRSTR, FMT='(A,A,I3,A,A,I3)' )
     .     'BUFRLIB: READDX - COPYING BUFR DCTY TBL FROM INTERNAL ',
     .     'ARRAYS ASSOC. W/ INPUT UNIT ', LUNDX, ' TO THOSE ASSOC. ',
     .     'W/ UNIT ', LUNIT
          CALL ERRWRT(ERRSTR)
          CALL ERRWRT('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
          CALL ERRWRT(' ')
         ENDIF
         CALL CPBFDX(LUD,LUN)
         CALL MAKESTAB
      ELSEIF(ILDX.EQ.1) THEN
c  .... Source is output BUFR file in LUNDX
c  .... BUFR file in LUNIT may be input or output
         IF(IPRT.GE.2) THEN
          CALL ERRWRT('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
          WRITE ( UNIT=ERRSTR, FMT='(A,A,I3,A,A,I3)' )
     .     'BUFRLIB: READDX - COPYING BUFR DCTY TBL FROM INTERNAL ',
     .     'ARRAYS ASSOC. W/ OUTPUT UNIT ', LUNDX, ' TO THOSE ASSOC. ',
     .     'W/ UNIT ', LUNIT
          CALL ERRWRT(ERRSTR)
          CALL ERRWRT('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
          CALL ERRWRT(' ')
         ENDIF
         CALL CPBFDX(LUD,LUN)
         CALL MAKESTAB
      ELSEIF(ILDX.EQ.0) THEN
c  .... Source is user-supplied character table in LUNDX
c  .... BUFR file in LUNIT may be input or output
         IF(IPRT.GE.2) THEN
          CALL ERRWRT('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
          WRITE ( UNIT=ERRSTR, FMT='(A,A,I3,A)' )
     .     'BUFRLIB: READDX - READING BUFR DICTIONARY TABLE FROM ',
     .     'USER-SUPPLIED TEXT FILE IN UNIT ', LUNDX,
     .     ' INTO INTERNAL ARRAYS'
          CALL ERRWRT(ERRSTR)
          CALL ERRWRT('++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++')
          CALL ERRWRT(' ')
         ENDIF
         REWIND LUNDX
         CALL RDUSDX(LUNDX,LUN)
      ELSE
         GOTO 900
      ENDIF

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: READDX - CANNOT DETERMINE SOURCE OF '//
     . 'INPUT DICTIONARY TABLE')
      END
