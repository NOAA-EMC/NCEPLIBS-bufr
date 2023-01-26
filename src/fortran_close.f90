!> @file
!> @brief Close a Fortran file on the local system.
!>
!> ### Program History
!> Date | Programmer | Comments 
!> -----|------------|----------
!> 2015-08-30 | J. Whitaker | Original author
!> 2022-09-01 | J. Ator | Added 8-byte wrapper
!>
!> @author Jeff Whitaker @date 2015-08-30

!> Close a Fortran file on the local system.
!>
!> @param[in] lunit - integer: Fortran logical unit number for Fortran file
!> @param[out] iret - integer: return code from the Fortran close statement
!>
!> @author Jeff Whitaker @date 2015-08-30
recursive subroutine fortran_close(lunit, iret)

  use modv_im8b

  implicit none
  integer, intent(in)  :: lunit
  integer, intent(out) :: iret
  integer my_lunit

! check for i8 integers

  if(im8b) then
     im8b=.false.

     call x84(lunit,my_lunit,1)
     call fortran_close(my_lunit,iret)
     call x48(iret,iret,1)

     im8b=.true.
     return
  endif

  close(lunit, iostat=iret)
  return
end
