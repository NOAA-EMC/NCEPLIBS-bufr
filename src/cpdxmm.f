C> @file
C> @brief Read embedded DX BUFR table into internal arrays.
C>
C> @author J. Ator @date 2009-03-23

C> Read an entire DX BUFR table from a specified file into internal arrays.
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR file
C>
C> @author J. Ator @date 2009-03-23
        SUBROUTINE CPDXMM( LUNIT )

        use bufrlib

        USE MODV_MXDXTS

        USE MODA_MGWA
        USE MODA_MSGMEM

        COMMON /QUIET/  IPRT

        CHARACTER*128   ERRSTR

        LOGICAL DONE

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        IF ( NDXTS .GE. MXDXTS ) GOTO 900

        ICT = 0
        DONE = .FALSE.
        CALL STATUS(LUNIT,LUN,IL,IM)

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

            CALL BACKBUFR_C(LUN)
            DONE = .TRUE.
          ELSE IF ( IER .EQ. -2 ) THEN
            GOTO 901
          ELSE IF ( IDXMSG(MGWA) .NE. 1 ) THEN

C           This is a non-DX dictionary message.  Assume we've reached
C           the end of the dictionary table, and backspace LUNIT so that
C           the next read (e.g. in the calling routine) will get this
C           same message.

            CALL BACKBUFR_C(LUN)
            DONE = .TRUE.
          ELSE IF ( IUPBS3(MGWA,'NSUB') .EQ. 0 ) THEN

C           This is a DX dictionary message, but it doesn't contain any
C           actual dictionary information.  Assume we've reached the end
C           of the dictionary table.

            DONE = .TRUE.
          ELSE

C           Store this message into MODULE MSGMEM.

            ICT = ICT + 1
            IF ( ( NDXM + ICT ) .GT. MXDXM ) GOTO 902
            IPDXM(NDXM+ICT) = LDXM + 1
            LMEM = NMWRD(MGWA)
            IF ( ( LDXM + LMEM ) .GT. MXDXW ) GOTO 903
            DO J = 1, LMEM
              MDX(LDXM+J) = MGWA(J)
            ENDDO
            LDXM = LDXM + LMEM
          ENDIF
        ENDDO

C       Update the table information within MODULE MSGMEM.

        IF ( ICT .GT. 0 ) THEN
          IFDXTS(NDXTS+1) = NDXM + 1
          ICDXTS(NDXTS+1) = ICT
          IPMSGS(NDXTS+1) = MSGP(0) + 1
          NDXM = NDXM + ICT
          NDXTS = NDXTS + 1
          IF ( IPRT .GE. 2 ) THEN
            CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++')
            WRITE ( UNIT=ERRSTR, FMT='(A,I3,A,I3,A)')
     .          'BUFRLIB: CPDXMM - STORED NEW DX TABLE #', NDXTS,
     .          ' CONSISTING OF ', ICT, ' MESSAGES'
            CALL ERRWRT(ERRSTR)
            CALL ERRWRT('+++++++++++++++++++++++++++++++++++++++++++++')
            CALL ERRWRT(' ')
          ENDIF
        ENDIF

        RETURN
 900    CALL BORT('BUFRLIB: CPDXMM - MXDXTS OVERFLOW')
 901    CALL BORT('BUFRLIB: CPDXMM - UNEXPECTED READ ERROR')
 902    CALL BORT('BUFRLIB: CPDXMM - MXDXM OVERFLOW')
 903    CALL BORT('BUFRLIB: CPDXMM - MXDXW OVERFLOW')
        END
