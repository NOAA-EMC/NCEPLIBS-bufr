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
recursive subroutine bort(str)

  use bufrlib

  use moda_borts

  implicit none

  character*(*), intent(in) :: str

  if (bort_catch == 'Y') then
    call strsuc(str, caught_str, caught_str_len)
    call bort_goto_target_c()
  endif

  call errwrt(' ')
  call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
  call errwrt(str)
  call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
  call errwrt(' ')
  stop 8

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
recursive subroutine bort2(str1,str2)

  use bufrlib

  use moda_borts

  implicit none

  character*(*), intent(in) :: str1, str2

  if (bort_catch == 'Y') then
    call strsuc(str1, caught_str, caught_str_len)
    caught_str = str1(1:caught_str_len) // str2
    call strsuc(caught_str, caught_str, caught_str_len)
    call bort_goto_target_c()
  endif

  call errwrt(' ')
  call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
  call errwrt(str1)
  call errwrt(str2)
  call errwrt('***********BUFR ARCHIVE LIBRARY ABORT**************')
  call errwrt(' ')
  stop 8

end subroutine bort2

!> Specify whether subsequent bort errors should be caught and returned to the
!> application program.
!>
!> The use of this function allows for a more graceful exit from an application
!> program in the event of any future bort error within the NCEPLIBS-bufr software.
!> Specifically, whenever this capability is activated, and following any future call
!> to any other library subroutine or function, the application program can immediately call
!> subroutine check_for_bort() to check whether a bort error occurred within that
!> previous subroutine or function and then react accordingly.  Otherwise, and by default,
!> any such bort error within the library will trigger an immediate abort of the
!> application program, and with the same error information instead written to the
!> location specified via subroutine errwrt().
!>
!> This function can be called at any point from within an application program,
!> and the specified value for cbc will remain in effect for all future calls
!> to all NCEPLIBS-bufr subroutines and functions, unless a subsequent call is
!> made to this function to reset the value of cbc again. If this function is never
!> called, then a default value of 'N' is used for cbc.
!>
!> @note Application programs should exercise caution when the catching and
!> returning of bort errors is enabled.  Specifically, and depending on the nature or severity
!> of any error caught or its depth within the internal call stack, there's no guarantee that
!> the library remains fully functional or in a useable state (for example, certain
!> intermediate values may not have been restored to previous settings, internal memory may not
!> have been fully deallocated, etc.).  Rather, the intent of this function is to allow an
!> application program to gracefully exit if a serious NCEPLIBS-bufr error does occur,
!> including potentially cleanly disengaging from other linked libraries or tasks.  So unless
!> the caught error is fairly benign and local to the subroutine or function in question, it
!> may not be possible for the application program to make an adjustment and then reattempt
!> another call to the library without leading to unpredictable results.  Instead, and if the
!> application program intends to continue running, the safest option in such cases may be
!> to use subroutine exitbufr() to fully reset the NCEPLIBS-bufr software before attempting
!> any future calls to the library.
!>
!> @param cbc - Flag indicating whether subsequent bort errors within the NCEPLIBS-bufr
!> software should be caught and made available to the application program via
!> subroutine check_for_bort():
!>  - 'N' (or 'n') = No (the default)
!>  - 'Y' (or 'y') = Yes
!> @returns catch_borts - Return code:
!>  -  0 = Normal return
!>  - -1 = Illegal value was input for cbc
!>
!> @author J. Ator @date 2025-08-25
integer function catch_borts(cbc) result (iret)

  use modv_vars, only: iprt

  use moda_borts

  implicit none

  character, intent(in) :: cbc
  character my_cbc

  iret = 0
  my_cbc = cbc
  call capit(my_cbc)
  if (iprt >= 1) call errwrt('++++++++++++++++++WARNING+++++++++++++++++++')

  if (my_cbc == 'Y') then
    bort_catch = my_cbc
    bort_target_is_unset = .true.
    if (iprt >= 1) call errwrt('BUFRLIB: CATCH_BORTS - ENABLING BORT CATCHING')
  else if (my_cbc == 'N') then
    bort_catch = my_cbc
    bort_target_is_unset = .false.
    if (iprt >= 1) call errwrt('BUFRLIB: CATCH_BORTS - DISABLING BORT CATCHING')
  else
    iret = -1
    if (iprt >= 1) call errwrt('BUFRLIB: CATCH_BORTS - ILLEGAL INPUT VALUE; NO ACTION WAS TAKEN')
  endif

  if (iprt >= 1) call errwrt('++++++++++++++++++WARNING+++++++++++++++++++')

  return
end function catch_borts

!> Sets a new bort target, if bort catching is enabled and such a target doesn't already exist.
!>
!> @returns bort_target_set - Return code:
!>  - 0 = a new bort target was not set during this call, or bort catching is disabled
!>  - 1 = a new bort target was set during this call
!>
!> @author J. Ator @date 2025-11-05
integer function bort_target_set() result (iret)

  use moda_borts

  implicit none

  if (bort_target_is_unset) then
    bort_target_is_unset = .false.
    caught_str_len = 0
    iret = 1
  else
    iret = 0
  endif

  return
end function bort_target_set

!> Clear any existing bort target.
!>
!> @author J. Ator @date 2025-11-05
subroutine bort_target_unset

  use moda_borts

  implicit none

  bort_target_is_unset = .true.

  return
end subroutine bort_target_unset

!> Check whether a bort error occurred during a previous call to an NCEPLIBS-bufr
!> subroutine or function.
!>
!> This subroutine should only be called if a prior call was made to function
!> catch_borts() from an application program with cbc set to 'Y'
!>
!> @param bort_str - Error string, if such a bort error occurred; otherwise empty.
!> @param bort_str_len - Length of bort_str:
!>  - -1 = Subroutine catch_borts() was not previously called
!>  -  0 = No bort error occurred
!>  - >0 = Length of bort_str, up to a maximum of 300 characters
!>
!> @author J. Ator @date 2025-08-25
recursive subroutine check_for_bort(bort_str, bort_str_len)

  use modv_vars, only: iprt, im8b

  use moda_borts

  implicit none

  character*(*), intent(out) :: bort_str

  integer, intent(out) :: bort_str_len

  ! Check for I8 integers
  if(im8b) then
    im8b = .false.
    call check_for_bort(bort_str,bort_str_len)
    call x48(bort_str_len,bort_str_len,1)
    im8b = .true.
    return
  endif

  if (bort_catch == 'N') then
    if (iprt >= 1) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt('BUFRLIB: CHECK_FOR_BORT WAS CALLED WITHOUT HAVING PREVIOUSLY CALLED CATCH_BORTS')
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    endif
    bort_str_len = -1
  else if (caught_str_len == 0) then
    bort_str_len = 0
    bort_str = ' '
  else
    bort_str_len = min(len(bort_str),caught_str_len)
    bort_str = caught_str(1:bort_str_len)
  endif

  return
end subroutine check_for_bort
