!> @file
!> @brief Remove leading and trailing blanks from a character string.
!>
!> @author J. Woollen @date 1994-01-06

!> Remove leading and trailing blanks from a character string.
!> The string may not contain any embedded blanks.
!>
!> @param[in]  str1 - String
!> @param[out] str2 - Copy of str1 with leading and trailing blanks removed
!> @param[out] lens - Length of str2
!>
!> @author J. Woollen @date 1994-01-06
subroutine strsuc(str1,str2,lens)
  implicit none

  character*(*), intent(in) :: str1
  character*(*), intent(out) :: str2

  integer, intent(out) :: lens

  str2 = adjustl(str1)
  lens = len_trim(str2)

  return
end subroutine strsuc
