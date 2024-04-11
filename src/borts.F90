!> @file
!> @brief Log one or more error messages and abort application program.
!>
!> @author J. Ator @date 2009-04-21

!> Log an error message, then abort the application program.
!>
!> This subroutine is similar to subroutine bort2(), except that bort2() logs
!> two error messages instead of one.
!>
!> @param str - Error message
!>
!> @author J. Woollen @date 1998-07-08
subroutine bort(str)

  implicit none

  character*(*), intent(in) :: str

  call errwrt(' ')
  call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
  call errwrt(str)
  call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
  call errwrt(' ')

  stop 8
end subroutine bort

!> Log two error messages, then abort the application program.
!>
!> This subroutine is similar to subroutine bort(), except that bort() logs
!> one error message instead of two.
!>
!> @param str1 - First error message
!> @param str2 - Second error message
!>
!> @author D. Keyser @date 2003-11-04
subroutine bort2(str1,str2)

  implicit none

  character*(*), intent(in) :: str1, str2

  call errwrt(' ')
  call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
  call errwrt(str1)
  call errwrt(str2)
  call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
  call errwrt(' ')

  stop 8
end subroutine bort2
