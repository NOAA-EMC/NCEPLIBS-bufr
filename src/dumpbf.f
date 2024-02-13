C> @file
C> @brief Read the Section 1 date-time from the first two "dummy"
C> messages of an NCEP dump file
C>
C> @author J. Woollen @date 1996-12-11

C> Read the Section 1 date-time from the first two "dummy" messages of an
C> NCEP dump file.
C>
C> This bypasses any messages at the beginning of the
C> file which may contain embedded DX BUFR table information. Normally,
C> the first of these two "dummy" messages contains the dump center
C> date-time in Section 1, while the second message contains the dump
C> initiation date-time in Section 1. Neither of these two "dummy"
C> messages should contain any data subsets in Section 4.
C>
C> Logical unit LUNIT must already be associated with a filename
C> on the local system, typically via a Fortran "OPEN" statement.
C>
C> If the subroutine fails to locate either of the two "dummy"
C> messages within the file pointed to by LUNIT, then the corresponding
C> JDATE or JDUMP array will be filled with all values set to (-1).
C>
C> @param[in] LUNIT - integer: Fortran logical unit number for BUFR
C> dump file.
C> @param[out] JDATE - integer(5): Dump center date-time stored
C> within Section 1 of first "dummy" message:
C> - Index 1 contains the year, in format of either YY or YYYY,
C>   depending on the most recent call to subroutine datelen().
C> - Index 2 contains the month.
C> - Index 3 contains the day.
C> - Index 4 contains the hour.
C> - Index 5 contains the minute.
C> @param[out] JDUMP  -- integer(5): Dump initiation date-time stored
C> within Section 1 of second "dummy" message:
C> - Index 1 contains the year, in format of either YY or YYYY,
C>   depending on the most recent call to subroutine datelen().
C> - Index 2 contains the month.
C> - Index 3 contains the day.
C> - Index 4 contains the hour.
C> - Index 5 contains the minute.
C>
C> @author J. Woollen @date 1996-12-11
      RECURSIVE SUBROUTINE DUMPBF(LUNIT,JDATE,JDUMP)

      use modv_vars, only: im8b

      use moda_mgwa

      COMMON /QUIET / IPRT

      DIMENSION     JDATE(*),JDUMP(*)

      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL DUMPBF(MY_LUNIT,JDATE,JDUMP)
         CALL X48(JDATE,JDATE,5)
         CALL X48(JDUMP,JDUMP,5)

         IM8B=.TRUE.
         RETURN
      ENDIF

      DO I=1,5
        JDATE(I) = -1
        JDUMP(I) = -1
      ENDDO

C  SEE IF THE FILE IS ALREADY OPEN TO BUFR INTERFACE (A NO-NO)
C  -----------------------------------------------------------

      CALL STATUS(LUNIT,LUN,JL,JM)
      IF(JL.NE.0) GOTO 900
      call openbf(lunit,'INX',lunit)

C  READ PAST ANY DICTIONARY MESSAGES
C  ---------------------------------

1     CALL RDMSGW(LUNIT,MGWA,IER)
      IF(IER.LT.0) GOTO 200
      IF(IDXMSG(MGWA).EQ.1) GOTO 1

C  DUMP CENTER YY,MM,DD,HH,MM IS IN THE FIRST EMPTY MESSAGE
C  --------------------------------------------------------
C  i.e. the first message containing zero subsets

      IF(IUPBS3(MGWA,'NSUB').NE.0) GOTO 200

      IGD = IGETDATE(MGWA,JDATE(1),JDATE(2),JDATE(3),JDATE(4))
      JDATE(5) = IUPBS01(MGWA,'MINU')

C  DUMP CLOCK YY,MM,DD,HH,MM IS IN THE SECOND EMPTY MESSAGE
C  --------------------------------------------------------
C  i.e. the second message containing zero subsets

      CALL RDMSGW(LUNIT,MGWA,IER)
      IF(IER.LT.0) GOTO 200

      IF(IUPBS3(MGWA,'NSUB').NE.0) GOTO 200

      IGD = IGETDATE(MGWA,JDUMP(1),JDUMP(2),JDUMP(3),JDUMP(4))
      JDUMP(5) = IUPBS01(MGWA,'MINU')

      CALL CLOSBF(LUNIT)
      GOTO 100

200   IF(IPRT.GE.1 .AND. (JDATE(1).EQ.-1.OR.JDUMP(1).EQ.-1)) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      IF(JDATE(1).EQ.-1) THEN
        ERRSTR = 'BUFRLIB: DUMPBF - FIRST  EMPTY BUFR MESSAGE '//
     .    'SECTION 1 DATE COULD NOT BE LOCATED - RETURN WITH '//
     .    'JDATE = 5*-1'
        CALL ERRWRT(ERRSTR)
      ENDIF
      IF(JDUMP(1).EQ.-1) THEN
        ERRSTR = 'BUFRLIB: DUMPBF - SECOND EMPTY BUFR MESSAGE '//
     .    'SECTION 1 DATE COULD NOT BE LOCATED - RETURN WITH '//
     .    'JDUMP = 5*-1'
        CALL ERRWRT(ERRSTR)
      ENDIF
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT
     . ('BUFRLIB: DUMPBF - INPUT BUFR FILE IS OPEN, IT MUST BE CLOSED')
      END
