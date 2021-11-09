C> @file
C> @brief Specify format of Section 1 date-time when reading
C> BUFR messages.

C> This subroutine is used to specify the format of Section 1
C> date-time values that will be output by future calls to
C> any of the BUFRLIB [message-reading subroutines](@ref hierarchy).
C> 
C> @author J. Woollen
C> @date 1998-07-08
C>
C> @param[in] LEN --  integer: Length of Section 1 date-time
C>                    values to be output by all future calls
C>                    to message-reading subroutines
C>                    -  8 = YYMMDDHH format with 2-digit year
C>                           (the default)
C>                    - 10 = YYYYMMDDHH format with 4-digit year
C>
C> <p>This subroutine can be called at any time from within the
C> application program, and the specified value for LEN will remain
C> in effect for all future calls to any of the BUFRLIB subroutines
C> which read BUFR messages, unless a subsequent call is made to this
C> subroutine to reset the value of LEN again.  If this subroutine is
C> never called, a default value of 8 is used for LEN, as set within
C> subroutine bfrini().
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1998-07-08 | J. Woollen | Original author |
C> | 2002-05-14 | J. Woollen | Changed from an entry point in readmg() to stand-alone subroutine, to increase portability to other platforms |
C> | 2003-11-04 | J. Ator    | Added documentation |
C> | 2004-12-20 | D. Keyser  | Calls wrdlen() to initialize local machine information, in case it has not yet been called |
C>
      SUBROUTINE DATELEN(LEN)

      COMMON /DATELN/ LENDAT

      CHARACTER*128 BORT_STR

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CALL SUBROUTINE WRDLEN TO INITIALIZE SOME IMPORTANT INFORMATION
C  ABOUT THE LOCAL MACHINE (IN CASE IT HAS NOT YET BEEN CALLED)
C  ---------------------------------------------------------------

      CALL WRDLEN

      IF(LEN.NE.8 .AND. LEN.NE.10) GOTO 900
      LENDAT = LEN

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: DATELEN - INPUT ARGUMENT IS",I4," - '//
     . 'IT MUST BE EITHER 8 OR 10")') LEN
      CALL BORT(BORT_STR)
      END
