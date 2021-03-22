C> @file
C> @brief Read the Section 1 date-time from the first data message
C> of a BUFR file.

C> This subroutine reads and returns the Section 1 date-time from
C> the first data message of a BUFR file, bypassing any messages
C> at the beginning of the file which may contain embedded DX BUFR
C> table information.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIT    - integer: Fortran logical unit number for BUFR
C>                       file
C> @param[out] MEAR    - integer: Year stored within Section 1 of
C>                       first data message, in format of either
C>                       YY or YYYY, depending on the most
C>                       recent call to subroutine datelen()
C> @param[out] MMON    - integer: Month stored within Section 1 of
C>                       first data message
C> @param[out] MDAY    - integer: Day stored within Section 1 of
C>                       first data message
C> @param[out] MOUR    - integer: Hour stored within Section 1 of
C>                       first data message
C> @param[out] IDATE    - integer: Date-time stored within Section 1 of
C>                        first data message, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C>
C> <p>Logical unit LUNIT must already be associated with a filename
C> on the local system, typically via a Fortran "OPEN" statement.
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1998-07-08  J. Woollen -- Replaced call to Cray library routine ABORT
C>                             with call to new internal routine bort()
C> - 1998-08-31  J. Woollen -- Modified to correct an error which led to
C>                             MEAR being returned as 2-digit when it was
C>                             requested as 4-digit via a prior call to
C>                             datelen()
C> - 1998-10-27  J. Woollen -- Modified to correct problems caused by
C>                             in-lining code with fpp directives
C> - 2003-05-19  M. Shirey  -- Replaced calls to Fortran insrinsic
C>                             function ICHAR with the NCEP W3LIB
C>                             function MOVA2I
C> - 2003-11-04  D. Keyser  -- Modified date calculations to no longer
C>                             use floating point arithmetic
C> - 2004-08-18  J. Ator    -- Modified 'BUFR' string test for portability
C>                             to EBCDIC machines
C> - 2004-12-20  D. Keyser  -- Calls wrdlen() to initialize local machine
C>                             information, in case it has not yet been
C>                             called
C> - 2005-11-29  J. Ator    -- Use igetdate(), iupbs01() and rdmsgw()
C> - 2009-03-23  J. Ator    -- Use idxmsg() and errwrt()
C> - 2012-09-15  J. Woollen -- Modified for C/I/O/BUFR interface;
C>                             use new openbf type 'INX' to open and close
C>                             the C file without closing the Fortran file
C> - 2014-12-10  J. Ator    -- Use modules instead of COMMON blocks
C>                           
      SUBROUTINE DATEBF(LUNIT,MEAR,MMON,MDAY,MOUR,IDATE)

      USE MODA_MGWA

      INCLUDE 'bufrlib.inc'

      COMMON /QUIET / IPRT

      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  Initialization, in case OPENBF or WRDLEN haven't been called yet.

#ifdef DYNAMIC_ALLOCATION
      IF ( .NOT. ALLOCATED(MGWA) ) THEN
        CALL OPENBF(LUNIT,'FIRST',LUNIT)
      ENDIF
#else
      CALL WRDLEN
#endif

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
