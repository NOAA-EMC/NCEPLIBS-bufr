C> @file
C> @brief Copy a BUFR data subset.
C>
C> @author J. Woollen @date 1994-01-06

C> Call subroutine copysb() and pass
C> back its return code as the function value.
C>
C> @param[in] LUNIN   -- integer: Fortran logical unit number for
C>                       source BUFR file
C> @param[in] LUNOT   -- integer: Fortran logical unit number for
C>                       target BUFR file
C> @returns icopysb   -- integer: return code
C>                       - 0 = normal return
C>                       - -1 = a BUFR data subset could not be
C>                              read from the BUFR message in
C>                              internal arrays for LUNIN
C>
C> @remarks
C> - The use of this function allows the return code from copysb() to be
C> used as the target variable within an iterative program loop.
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE FUNCTION ICOPYSB(LUNIN,LUNOT) RESULT(IRET)

      use modv_vars, only: im8b

      IF(IM8B) THEN
        IM8B=.FALSE.

        CALL X84(LUNIN,MY_LUNIN,1)
        CALL X84(LUNOT,MY_LUNOT,1)
        IRET=ICOPYSB(MY_LUNIN,MY_LUNOT)

        IM8B=.TRUE.
        RETURN
      ENDIF

      CALL COPYSB(LUNIN,LUNOT,IRET)

      RETURN
      END
