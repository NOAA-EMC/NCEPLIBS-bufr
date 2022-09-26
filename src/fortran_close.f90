!> @file
!> @brief Close a Fortran file on the local system

!> @authors Jeff Whitaker
!> @date 2015-08-30
!>
!> @param[in] lunit    - integer: Fortran logical unit number for Fortran file
!> @param[out] iret    - integer: return code from the Fortran close statement
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2015-08-30 | J. Whitaker | Original author |
!> | 2022-09-01 | J. Ator | Added 8-byte wrapper |

subroutine fortran_close(lunit, iret)
  use modv_im8b
  implicit none
  integer, intent(in)  :: lunit
  integer, intent(out) :: iret
  integer*8 lunit_8,iret_8

! check for i8 integers

  if(im8b) then
     im8b=.false.

     lunit_8=lunit
     call fortran_close_8(lunit_8,iret_8)
     iret=iret_8

     im8b=.true.
     return
  endif

  close(lunit, iostat=iret)
  return
end

!> This subroutine is an internal wrapper for handling 8-byte integer arguments to subroutine fortran_close().
!>
!> <p>Application programs which use 8-byte integer arguments should never call this subroutine directly; instead, such programs
!> should make an initial call to subroutine setim8b() with int8b=.TRUE. and then call subroutine fortran_close() directly.
!>
!> @param[in] lunit_8  - integer*8: Fortran logical unit number for Fortran file
!> @param[out] iret_8  - integer*8: return code from the Fortran close statement
!>
!> @author J. Ator
!> @date 2022-09-01
!>
!> <b>Program history log:</b>
!> | Date       | Programmer | Comments             |
!> | -----------|------------|----------------------|
!> | 2022-09-01 | J. Ator    | Original author      |

     subroutine fortran_close_8(lunit_8, iret_8)

     integer*8 lunit_8,iret_8

     lunit=lunit_8
     call fortran_close(lunit,iret)
     iret_8=iret

     end subroutine
