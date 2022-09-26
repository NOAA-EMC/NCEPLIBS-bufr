!> @file
!> @brief Open a Fortran file on the local system

!> @authors Jeff Whitaker
!> @date 2015-08-30
!>
!> @param[in] filename - character*(*): name of the file to be opened
!> @param[in] lunit    - integer: Fortran logical unit number for Fortran file
!> @param[in] format   - character*(*): format of the Fortran file
!> @param[in] position - character*(*): to rewind or continue with open file
!> @param[out] iret    - integer: return code from the Fortran open statement
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2015-08-30 | J. Whitaker | Original author |
!> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      subroutine fortran_open(filename, lunit, format, position, iret)
      use modv_im8b
      implicit none
      character*(*), intent(in) :: filename, format, position
      integer, intent(in)  :: lunit
      integer, intent(out) :: iret
      integer*8 lunit_8,iret_8

!     check for i8 integers

      if(im8b) then
         im8b=.false.

         lunit_8=lunit
         call fortran_open_8(filename,lunit_8,format,position,iret_8)
         iret=iret_8

         im8b=.true.
         return
      endif

      open(lunit, file=trim(filename), form=trim(format), &
                  position=trim(position), iostat=iret)

     end subroutine

!> This subroutine is an internal wrapper for handling 8-byte integer arguments to subroutine fortran_open().
!>
!> <p>Application programs which use 8-byte integer arguments should never call this subroutine directly; instead, such programs
!> should make an initial call to subroutine setim8b() with int8b=.TRUE. and then call subroutine fortran_open() directly.
!>
!> @param[in] filename - character*(*): name of the file to be opened
!> @param[in] lunit_8  - integer*8: Fortran logical unit number for Fortran file
!> @param[in] format   - character*(*): format of the Fortran file
!> @param[in] position - character*(*): to rewind or continue with open file
!> @param[out] iret_8  - integer*8: return code from the Fortran open statement
!>
!> @author J. Woollen
!> @date 2022-08-04
!>
!> <b>Program history log:</b>
!> | Date       | Programmer | Comments             |
!> | -----------|------------|----------------------|
!> | 2022-08-04 | J. Woollen | Original author      |

     subroutine fortran_open_8(filename, lunit_8, format, position, iret_8)

     character*(*), intent(in) :: filename, format, position
     integer*8 lunit_8,iret_8

     lunit=lunit_8
     call fortran_open(filename, lunit, format, position, iret)
     iret_8=iret

     end subroutine
