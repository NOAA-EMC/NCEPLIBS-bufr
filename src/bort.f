C> @file
C> @brief Log one error message and abort application program.

C> This subroutine calls subroutine errwrt() to log an error message,
C> then calls subroutine bort_exit() to abort the application program.
C>
C> <p>It is similar to subroutine bort2(), except that bort2() logs
C> two error messages instead of one.
C>
C> @author J. Woollen
C> @date 1998-07-08
C>
C> @param[in] STR   -- character*(*): Error message
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 1998-07-08 | J. Woollen | Original author |
C> | 2003-11-04 | J. Ator    | Added documentation; replaced call to intrinsic C routine "exit" with call to bort_exit() to ensure return of non-zero status code |
C> | 2009-04-21 | J. Ator    | Use errwrt() |
C>
      SUBROUTINE BORT(STR)

      CHARACTER*(*) STR

      CALL ERRWRT(' ')
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(STR)
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(' ')

      CALL BORT_EXIT

      END
