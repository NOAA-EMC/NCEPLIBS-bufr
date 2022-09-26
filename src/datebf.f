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
C> <p>Logical unit LUNIT must already be associated with a filename
C> on the local system, typically via a Fortran "OPEN" statement.
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1994-01-06 | J. Woollen | Original author |
C> | 1998-07-08 | J. Woollen | Replaced call to Cray library routine ABORT with call to new internal routine bort() |
C> | 1998-08-31 | J. Woollen | Modified to correct an error which led to MEAR being returned as 2-digit when it was requested as 4-digit via a prior call to datelen() |
C> | 1998-10-27 | J. Woollen | Modified to correct problems caused by in-lining code with fpp directives |
C> | 2003-05-19 | M. Shirey  | Replaced calls to Fortran insrinsic function ICHAR with the NCEP W3LIB function MOVA2I |
C> | 2003-11-04 | D. Keyser  | Modified date calculations to no longer use floating point arithmetic |
C> | 2004-08-18 | J. Ator    | Modified 'BUFR' string test for portability to EBCDIC machines |
C> | 2004-12-20 | D. Keyser  | Calls wrdlen() to initialize local machine information, in case it has not yet been called |
C> | 2005-11-29 | J. Ator    | Use igetdate(), iupbs01() and rdmsgw() |
C> | 2009-03-23 | J. Ator    | Use idxmsg() and errwrt() |
C> | 2012-09-15 | J. Woollen | Modified for C/I/O/BUFR interface; use new openbf type 'INX' to open and close the C file without closing the Fortran file |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      SUBROUTINE DATEBF(LUNIT,MEAR,MMON,MDAY,MOUR,IDATE)

      USE MODA_MGWA
      USE MODV_IM8B

      COMMON /QUIET / IPRT

      INTEGER*8 LUNIT_8,MEAR_8,MMON_8,MDAY_8,MOUR_8,IDATE_8

      CHARACTER*128 ERRSTR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         LUNIT_8=LUNIT
         CALL DATEBF_8(LUNIT_8,MEAR_8,MMON_8,MDAY_8,MOUR_8,IDATE_8)
         MEAR=MEAR_8
         MMON=MMON_8
         MDAY=MDAY_8
         MOUR=MOUR_8
         IDATE=IDATE_8

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

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine datebf().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine datebf() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIT_8 -- integer*8: Fortran logical unit number for BUFR
C>                       file
C> @param[out] MEAR_8 -- integer*8: Year stored within Section 1 of
C>                       first data message, in format of either
C>                       YY or YYYY, depending on the most
C>                       recent call to subroutine datelen()
C> @param[out] MMON_8 -- integer*8: Month stored within Section 1 of
C>                       first data message
C> @param[out] MDAY_8 -- integer*8: Day stored within Section 1 of
C>                       first data message
C> @param[out] MOUR_8 -- integer*8: Hour stored within Section 1 of
C>                       first data message
C> @param[out] IDATE_8 -- integer*8: Date-time stored within Section 1 of
C>                        first data message, in format of either
C>                        YYMMDDHH or YYYYMMDDHH, depending on the most
C>                        recent call to subroutine datelen()
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

      SUBROUTINE DATEBF_8(LUNIT_8,MEAR_8,MMON_8,MDAY_8,MOUR_8,IDATE_8) 

      INTEGER*8 LUNIT_8,MEAR_8,MMON_8,MDAY_8,MOUR_8,IDATE_8

      LUNIT=LUNIT_8
      CALL DATEBF(LUNIT,MEAR,MMON,MDAY,MOUR,IDATE)
      MEAR_8=MEAR
      MMON_8=MMON
      MDAY_8=MDAY
      MOUR_8=MOUR
      IDATE_8=IDATE

      RETURN
      END
