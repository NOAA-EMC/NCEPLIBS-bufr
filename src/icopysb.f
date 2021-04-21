C> @file
C> @brief Copy a BUFR data subset.
      
C> This function calls BUFRLIB subroutine copysb() and passes
C> back its return code as the function value.
C>
C> @author J. Woollen
C> @date 1994-01-06
C>
C> @param[in] LUNIN    - integer: Fortran logical unit number for
C>                       source BUFR file
C> @param[in] LUNOT    - integer: Fortran logical unit number for
C>                       target BUFR file
C> @returns icopysb    - integer: return code
C>                       - 0 = normal return
C>                       - -1 = a BUFR data subset could not be
C>                              read from the BUFR message in
C>                              internal arrays for LUNIN
C>
C> @remarks
C> - The use of this function allows the return code from copysb() to be
C> used as the target variable within an iterative program loop.
C>
C> <b>Program history log:</b>
C> - 1994-01-06 J. Woollen -- Original author
C> - 2002-05-14 J. Woollen -- Changed from an entry point to increase
C>                           portability to other platforms
C>
      FUNCTION ICOPYSB(LUNIN,LUNOT)

      CALL COPYSB(LUNIN,LUNOT,IRET)
      ICOPYSB = IRET
      RETURN
      END
