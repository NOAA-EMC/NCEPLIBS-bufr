C> @file
C> @brief Log one error message and abort application program.
C>
C> @author J. Woollen @date 1998-07-08

C> Call subroutine errwrt() to log an error message,
C> then call subroutine bort_exit() to abort the application program.
C>
C> This subroutine is similar to subroutine bort2(), except that bort2() logs
C> two error messages instead of one.
C>
C> @param[in] STR - character*(*): Error message
C>
C> @author J. Woollen @date 1998-07-08
      SUBROUTINE BORT(STR)

      use bufrlib

      CHARACTER*(*) STR

      CALL ERRWRT(' ')
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(STR)
      CALL ERRWRT('***********BUFR ARCHIVE LIBRARY ABORT**************')
      CALL ERRWRT(' ')

      CALL BORT_EXIT_C

      END
