C> @file
C> @brief Log two error messages and abort application program.

C> This subroutine calls subroutine errwrt() to log two error messages,
C> then calls subroutine bort_exit() to abort the application program.
C>
C> <p>It is similar to subroutine bort(), except that bort() logs
C> one error message instead of two.
C>
C> @author D. Keyser
C> @date 2003-11-04
C>
C> @param[in] STR1  -- character*(*): First error message
C> @param[in] STR2  -- character*(*): Second error message
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2003-11-04 | D. Keyser | Original author |
C> | 2009-04-21 | J. Ator   | Use errwrt() |
C>
      SUBROUTINE BORT2(STR1,STR2)

      CHARACTER*(*) STR1, STR2

      CALL ERRWRT(' ')
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(STR1)
      CALL ERRWRT(STR2)
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(' ')

      CALL BORT_EXIT

      END
