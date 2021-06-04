!> @file
!> @brief Open a Fortran file on the local system

!> @authors Jeff Whitaker
!> @date 2015-08-30
!>
!> @param[in] filename - character*(*): name of the file to be opened
!> @param[in] lunit    - integer: Fortran logical unit number for fortran
!>                       file
!> @param[in] format   - character*(*): format of the fortran file
!> @param[in] position - character*(*): to rewind or continue with open file
!> @param[out] iret    - integer: return code from the fortran open statement

subroutine fortran_open(filename, lunit, format, position, iret)
  implicit none
  character*(*), intent(in) :: filename, format, position
  integer, intent(in)  :: lunit
  integer, intent(out) :: iret
  open(lunit, file=trim(filename), form=trim(format), &
       position=trim(position), iostat=iret)
  return
end
