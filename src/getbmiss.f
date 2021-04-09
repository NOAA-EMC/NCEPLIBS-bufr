C> @file
C> @brief Get the current placeholder value for "missing" data

C> This function returns the current placeholder value which
C> represents "missing" data when reading from or writing to
C> BUFR files.
C>
C> @author J. Woollen
C> @date 2012-09-15
C>
C> @returns getbmiss - real*8: current placeholder value for
C>                     "missing" data
C>
C> <p>This subroutine can be called at any time from within an
C> application program, and the returned value can then be
C> used to represent "missing" data within the context of
C> future calls to any of the other BUFRLIB
C> [values-reading subroutines](@ref hierarchy) or
C> [values-writing subroutines](@ref hierarchy).
C> This placeholder value can also be changed at any
C> time via a separate call to subroutine setbmiss().
C>
C> <b>Program history log:</b>
C> - 2012-09-15  J. Woollen -- Original author
C>
      REAL*8 FUNCTION GETBMISS()

      USE MODV_BMISS

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------

      CALL OPENBF(0,'FIRST',0)

      GETBMISS = BMISS

      RETURN
      END
