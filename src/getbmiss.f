C> @file
C> @brief Get the current placeholder value for "missing" data
C>
C> @author J. Woollen @date 2012-09-15

C> This function returns the current placeholder value which
C> represents "missing" data when reading from or writing to
C> BUFR files.
C>
C> @returns getbmiss -- real*8: current placeholder value for
C>                      "missing" data
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
C> @author J. Woollen @date 2012-09-15
      RECURSIVE FUNCTION GETBMISS() RESULT(R8VAL)

      USE MODV_IM8B
      USE MODV_BMISS

      REAL*8 R8VAL

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         R8VAL=GETBMISS()

         IM8B=.TRUE.
         RETURN
      ENDIF

      CALL OPENBF(0,'FIRST',0)

      R8VAL = BMISS

      RETURN
      END
