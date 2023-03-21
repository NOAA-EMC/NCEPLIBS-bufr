!> @file
!> @brief Decode an integer from a character string.
!>
!> @author J. Woollen @date 1994-01-06

!> This subroutine decodes an integer from a character string.  The
!> string may contain leading or trailing blanks, but otherwise should
!> contain only digits and an (optional) leading sign ('+' or '-')
!> character. 
!>
!> If the string contains all blank characters, then num is returned
!> with a value of 0.
!>
!> @param[in]  str -- character*(*): String
!> @param[out] num -- integer: Value decoded from str
!> @param[out] iret -- return code:
!>   0 = success
!>  -1 = string contained one or more illegal characters
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine strnum(str,num,iret)
  use modv_im8b

  implicit none

  character*(*), intent(in) :: str

  integer, intent(out) ::  num, iret

  character str2*40, fmt*8

  integer lens

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

  iret = 0

! Check for blank input string.

  if ( str .eq. ' ' ) then
    num = 0
    return
  end if

! Verify that the string contains all legal characters.

  call strsuc ( str, str2, lens )
  if ( verify ( str2(1:lens), "0123456789+-" ) .ne. 0 ) then
    iret = -1
    return
  end if

! Decode the integer from the string.

  write ( fmt, '(''(I'',I2,'')'')' ) lens
  read ( str2(1:lens), fmt ) num

  return
end subroutine strnum
