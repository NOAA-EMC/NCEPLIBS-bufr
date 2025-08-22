!> @file
!> @brief Log one or more error messages, then either return to or abort the application program.
!>
!> @author J. Ator @date 2009-04-21

!> Log an error message, then either return to or abort the application program.
!>
!> This subroutine is similar to subroutine bort2(), except that bort2() logs
!> two error messages instead of one.
!>
!> @param str - Error message
!>
!> @author J. Woollen @date 1998-07-08
subroutine bort(str)

  use bufrlib

  use moda_borts

  implicit none

  character*(*), intent(in) :: str

  if (cbortcatch=='Y') then
    call strsuc(str, caught_str, caught_str_len)
    call bort_goto_target_c()
  else
    call errwrt(' ')
    call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
    call errwrt(str)
    call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
    call errwrt(' ')
    stop 8
  endif

end subroutine bort

!> Log two error messages, then either return to or abort the application program.
!>
!> This subroutine is similar to subroutine bort(), except that bort() logs
!> one error message instead of two.
!>
!> @param str1 - First error message
!> @param str2 - Second error message
!>
!> @author D. Keyser @date 2003-11-04
subroutine bort2(str1,str2)

  use bufrlib

  use moda_borts

  implicit none

  character*(*), intent(in) :: str1, str2

  if (cbortcatch=='Y') then
    call strsuc(str1, caught_str, caught_str_len)
    caught_str = str1(1:caught_str_len) // str2
    call strsuc(caught_str, caught_str, caught_str_len)
    call bort_goto_target_c()
  else
    call errwrt(' ')
    call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
    call errwrt(str1)
    call errwrt(str2)
    call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
    call errwrt(' ')
    stop 8
  endif
end subroutine bort2

!> Set a target location at which to return to the application program for
!> any future bort error which occurs within the library.
!>
!> This subroutine can be called at any point from within an application program,
!> and in which case it will initially return with bort_str_len = 0 to indicate
!> that a target location has been successfully set.  From then on, if a bort error
!> is subsequently triggered from anywhere within the NCEPLIBS-bufr software during
!> the remainder of the application program, control will immediately return to the
!> program through that same subroutine call with bort_str_len set to a positive number,
!> and with bort_str providing more details about the error itself.
!>
!> The use of this subroutine allows for a more graceful exit from an application
!> program in the event of any subsequent bort error within the NCEPLIBS-bufr
!> software.  Otherwise, if this subroutine is never called, then any such bort
!> error within the library will trigger an abort of the application program, and with
!> bort_str instead written to the location specified via subroutine errwrt().
!>
!> @param bort_str - Error string; set to an empty string if bort_str_len = 0
!> @param bort_str_len - Length of bort_str:
!>   0 = Target return location was successfully set
!>  >0 = Length of bort_str, up to a maximum of 300 characters
!>
!> @author J. Ator @date 2025-08-20
subroutine bort_catcher(bort_str, bort_str_len)

  use bufrlib

  use moda_borts

  implicit none

  character*(*), intent(out) :: bort_str

  integer, intent(out) :: bort_str_len
  integer ibst

  ibst = bort_set_target_c()

  bort_str = ' '

  if (ibst==0) then
    bort_str_len = 0
    cbortcatch = 'Y'
  else
    bort_str_len = min(len(bort_str),caught_str_len)
    bort_str = caught_str(1:bort_str_len)
  endif

end subroutine bort_catcher
