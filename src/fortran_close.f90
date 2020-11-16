!> @file
!> @authors Jeff Whitaker
!> @date 2015-08-30

!> @brief This subroutine closes an open fortran file
!>
!> @param[in] lunit    - integer: Fortran logical unit number for fortran
!>                       file
!> @param[out] iret    - integer: return code from the fortran open statement

subroutine fortran_close(lunit, iret)
  implicit none
  integer, intent(in)  :: lunit
  integer, intent(out) :: iret
  close(lunit, iostat=iret)
  return
end
