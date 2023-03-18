C> @file
C> @brief Read a complete DX BUFR table.
C>
C> @author Woollen @date 1994-01-06

C> Beginning at the current file pointer location within LUNIT,
C> this subroutine reads a complete DX BUFR table into internal memory arrays
C> in module @ref moda_tababd. A DX BUFR table consists of one or more consecutive
C> DX BUFR messages.
C>
C> This subroutine performs a function similar to
C> rdusdx(), except that rdusdx() reads from a file containing
C> a user-supplied DX BUFR table in character format. See rdusdx()
C> for a description of the arrays that are filled
C> in module @ref moda_tababd.
C>
C> This subroutine performs a function similar to
C> cpdxmm(), except that cpdxmm() writes to the internal memory
C> arrays in module @ref moda_msgmem, for use with a file of BUFR messages that
C> is being read and stored into internal memory via subroutine ufbmem().
C>
C> @param[in] LUNIT - integer: fortran logical unit number for BUFR file.
C> @param[in] LUN - integer: I/O stream index into internal memory arrays
C> (associated with file connected to logical unit LUNIT).
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE RDBFDX(LUNIT,LUN)

        USE MODA_MGWA

        COMMON /QUIET/  IPRT

        CHARACTER*128 ERRSTR

        LOGICAL DONE

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        CALL DXINIT(LUN,0)

        ICT = 0
        DONE = .FALSE.

C       Read a complete dictionary table from LUNIT, as a set of one or
C       more DX dictionary messages.

        DO WHILE ( .NOT. DONE )
          CALL RDMSGW ( LUNIT, MGWA, IER )
          IF ( IER .EQ. -1 ) THEN

C           Don't abort for an end-of-file condition, since it may be
C           possible for a file to end with dictionary messages.
C           Instead, backspace the file pointer and let the calling
C           routine diagnose the end-of-file condition and deal with
C           it as it sees fit.

            CALL BACKBUFR(LUN)
            DONE = .TRUE.
          ELSE IF ( IER .EQ. -2 ) THEN
            GOTO 900
          ELSE IF ( IDXMSG(MGWA) .NE. 1 ) THEN

C           This is a non-DX dictionary message.  Assume we've reached
C           the end of the dictionary table, and backspace LUNIT so that
C           the next read (e.g. in the calling routine) will get this
C           same message.

            CALL BACKBUFR(LUN)
            DONE = .TRUE.
          ELSE IF ( IUPBS3(MGWA,'NSUB') .EQ. 0 ) THEN

C           This is a DX dictionary message, but it doesn't contain any
C           actual dictionary information.  Assume we've reached the end
C           of the dictionary table.

            DONE = .TRUE.
          ELSE

C           Store this message into MODULE TABABD.

            ICT = ICT + 1
            CALL STBFDX(LUN,MGWA)
          ENDIF
        ENDDO

        IF ( IPRT .GE. 2 ) THEN
        CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        WRITE ( UNIT=ERRSTR, FMT='(A,I3,A)' )
     .    'BUFRLIB: RDBFDX - STORED NEW DX TABLE CONSISTING OF (',
     .    ICT, ') MESSAGES;'
        CALL ERRWRT(ERRSTR)
        ERRSTR = 'WILL APPLY THIS TABLE TO ALL SUBSEQUENT DATA IN '//
     .    'FILE UNTIL NEXT DX TABLE IS FOUND'
        CALL ERRWRT(ERRSTR)
        CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++++++')
        CALL ERRWRT(' ')
        ENDIF

        CALL MAKESTAB

        RETURN
 900    CALL BORT('BUFRLIB: RDBFDX - ERROR READING A BUFR DICTIONARY '//
     .    'MESSAGE')
        END
