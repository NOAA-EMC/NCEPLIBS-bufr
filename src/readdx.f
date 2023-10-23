C> @file
C> @brief Read DX BUFR table information into internal arrays.
C>
C> @author Woollen @date 1994-01-06

C> Initialize modules @ref moda_tababd and @ref
C> moda_msgcwd with DX BUFR (dictionary) tables. These tables are needed
C> to read, write, initialize or append a BUFR file.
C>
C> The modules are initialized from either:
C> 1. an external, user-supplied BURF dictionary table file (i.e., a
C> BUFR mnemonic table), or
C> 2. the BUFR file indicated by LUNIT, or
C> 3. another currently opened BUFR file.
C>
C> If the modules are initialized by the BUFR file indicated by LUNIT,
C> then it must have been opened for input processing and positioned at a
C> dictionary table > message somewhere in the file.
C>
C> Once initialzed, the dictionary arrays are associated with the BUFR
C> file indicated by LUNIT, until the file is closed with closbf().
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file
C> being read, written, initialized or appended.
C> @param[in] LUN - integer: I/O stream index into internal memory arrays
C> (associated with file connected to logical unit LUNIT)
C> @param[in] LUNDX - integer: Fortran logical unit number
C> containing dictionary table information to be used in reading/
C> writing from/to LUNIT (depending on the case); may be
C> set equal to LUNIT if dictionary table information is
C> already embedded in LUNIT (but only if LUNIT is being read).
C>
C> @author Woollen @date 1994-01-06
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
