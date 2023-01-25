C> @file
C> @brief Read DX BUFR table information into internal arrays.
C>
C> ### Program History Log
C> Date | Programmer | Comments 
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | original author
C> 1998-07-08 | J. Woollen | replaced call to cray library routine "abort" with call to bort()
C> 2003-11-04 | S. Bender  | added remarks/bufrlib routine interdependencies
C> 2003-11-04 | D. Keyser  | unified/portable for wrf; documentation; more diagnostic info
C> 2009-04-21 | J. Ator    | use errwrt
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine generates internal arrays containing DX BUFR
C> (dictionary) tables which are needed to read, write, initialize or
C> append a BUFR file. The information used to create the internal
C> dictionary table arrays (in module tababd) and the dictionary
C> message control word partition arrays (in module msgcwd)
C> (which are always then associated with the BUFR file in LUNIT)
C> may come from an external, user-supplied, BUFR dictionary
C> table file in character format (i.e., a BUFR mnemonic table), from
C> the BUFR file being acted upon (in which case the file must be
C> opened for input processing and positioned at a dictionary table
C> message somewhere in the file), or from another currently opened
C> and defined BUFR file.  In this latter case, the BUFR file would
C> most likely be opened for input, however there is nothing
C> preventing the use of a file open for output as long as it is
C> associated with internal dictionary arrays that can be used.
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
