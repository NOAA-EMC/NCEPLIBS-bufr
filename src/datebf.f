C> @file
C> @brief Read the Section 1 date-time from the first data message
C> of a BUFR file.
C> @author J. Woollen @date 1994-01-06

C> Read and return the Section 1 date-time from
C> the first data message of a BUFR file, bypassing any messages
C> at the beginning of the file which may contain embedded DX BUFR
C> table information.
C>
C> @param[in] LUNIT   -- integer: Fortran logical unit number for BUFR
C>                       file
C> @param[out] MEAR   -- integer: Year stored within Section 1 of
C>                       first data message, in format of either
C>                       YY or YYYY, depending on the most
C>                       recent call to subroutine datelen()
C> @param[out] MMON   -- integer: Month stored within Section 1 of
C>                       first data message
C> @param[out] MDAY   -- integer: Day stored within Section 1 of
C>                       first data message
C> @param[out] MOUR   -- integer: Hour stored within Section 1 of
C>                       first data message
C> @param[out] IDATE   -- integer: Date-time stored within Section 1 of
C>                        first data message, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C>
C> Logical unit LUNIT must already be associated with a filename
C> on the local system, typically via a Fortran "OPEN" statement.
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE DATEBF(LUNIT,MEAR,MMON,MDAY,MOUR,IDATE)

      USE MODA_MGWA
      USE MODV_IM8B

      COMMON /QUIET / IPRT

      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL DATEBF(MY_LUNIT,MEAR,MMON,MDAY,MOUR,IDATE)
         CALL X48(MEAR,MEAR,1)
         CALL X48(MMON,MMON,1)
         CALL X48(MDAY,MDAY,1)
         CALL X48(MOUR,MOUR,1)
         CALL X48(IDATE,IDATE,1)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  Initialization, in case OPENBF hasn't been called yet.
C  ------------------------------------------------------

      IF ( .NOT. ALLOCATED(MGWA) ) THEN
        CALL OPENBF(LUNIT,'FIRST',LUNIT)
      ENDIF

      IDATE = -1

C  SEE IF THE FILE IS ALREADY OPEN TO BUFR INTERFACE (A NO-NO)
C  -----------------------------------------------------------

      CALL STATUS(LUNIT,LUN,JL,JM)
      IF(JL.NE.0) GOTO 900
      CALL OPENBF(LUNIT,'INX',LUNIT)

C  READ TO A DATA MESSAGE AND PICK OUT THE DATE
C  --------------------------------------------

1     CALL RDMSGW(LUNIT,MGWA,IER)
      IF(IER.LT.0) GOTO 100
      IF(IDXMSG(MGWA).EQ.1) GOTO 1

      IDATE = IGETDATE(MGWA,MEAR,MMON,MDAY,MOUR)

100   IF(IPRT.GE.1 .AND. IDATE.EQ.-1) THEN
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      ERRSTR = 'BUFRLIB: DATEBF - SECTION 1 DATE COULD NOT BE '//
     .  'LOCATED - RETURN WITH IDATE = -1'
      CALL ERRWRT(ERRSTR)
      CALL ERRWRT('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      CALL ERRWRT(' ')
      ENDIF

C  EXITS
C  -----

      CALL CLOSBF(LUNIT)
      RETURN
900   CALL BORT
     . ('BUFRLIB: DATEBF - INPUT BUFR FILE IS OPEN, IT MUST BE CLOSED')
      END
