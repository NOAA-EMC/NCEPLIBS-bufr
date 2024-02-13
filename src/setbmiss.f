C> @file
C> @brief Define a customized placeholder value for "missing" data.
C>
C> @author J. Woollen @date 2012-09-15

C> Specify a customized value to
C> represent "missing" data when reading from or writing to BUFR files.
C>
C> This subroutine can be called at any time from within an
C> application program, and the value XMISS will then be treated as
C> "missing" when reading or writing BUFR data during all future
C> calls to any of the other NCEPLIBS-bufr
C> [values-reading subroutines](@ref hierarchy) or
C> [values-writing subroutines](@ref hierarchy).
C> Otherwise, if this subroutine is never called, a default
C> placeholder value of 10E10_8 is used for "missing".
C>
C> Any data value can always be checked for equivalence to the
C> current "missing" value via a call to function ibfms().  See also
C> function getbmiss().
C>
C> @remarks
C> - The value XMISS is never actually encoded within a BUFR data
C> subset; rather, XMISS is a user-friendly placeholder value to
C> represent "missing" data values within the scope of the
C> application program.  In any actual BUFR data subset, "missing"
C> values are always encoded as all bits set to 1, per WMO
C> regulations.
C>
C> @param[in] XMISS -- real*8: New placeholder value to represent
C>                     "missing" data
C>
C> @author J. Woollen @date 2012-09-15
      RECURSIVE SUBROUTINE SETBMISS(XMISS)

      use modv_vars, only: im8b, bmiss

      REAL*8 XMISS

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C     Check for I8 integers.

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL SETBMISS(XMISS)

         IM8B=.TRUE.
         RETURN
      ENDIF

      BMISS = XMISS

      RETURN
      END
