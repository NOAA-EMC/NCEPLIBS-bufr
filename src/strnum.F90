!> @file
!> @brief Decode an integer from a character string.
!>
!> @author J. Woollen @date 1994-01-06

!> Decode an integer from a character string.
!>
!> The string may contain leading or trailing blanks, but otherwise should
!> contain only digits and an (optional) leading sign ('+' or '-') character. 
!> If the string is empty or contains all blank characters, then num
!> is returned with a value of 0.
!>
!> @param[in]  str - character*(*): String.
!> @param[out] num - integer: Value decoded from str.
!> @param[out] iret - return code:
!>   0 = success
!>  -1 = string contained one or more illegal characters
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine strnum( str, num, iret )
  use modv_vars, only: im8b

  implicit none

  character*(*), intent(in) :: str

  integer, intent(out) ::  num, iret

  character str2*40

  integer lens, ios

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! Check for I8 integers.

  if (im8b) then
     im8b = .false.

     call strnum ( str, num, iret )
     call x48 ( num, num, 1 )
     call x48 ( iret, iret, 1 )

     im8b = .true.
     return
  end if

! Decode the integer from the string.

  iret = 0
  num = 0

  call strsuc ( str, str2, lens )
  if ( lens .eq. 0 ) return

  read ( str2(1:lens), '(I40)', iostat = ios ) num
  if ( ios .ne. 0 ) iret = -1

  return
end subroutine strnum
