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

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
      subroutine fortran_open_8(filename, lunit_8, format, position, iret_8) 
      integer*8 lunit_8,iret_8
      lunit=lunit_8
      iret=iret_8
      call fortran_open(filename, lunit, format, position, iret)
      iret_8=iret
      end subroutine
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

      subroutine fortran_open(filename, lunit, format, position, iret)
      use moda_im8b
      implicit none
      character*(*), intent(in) :: filename, format, position
      integer, intent(in)  :: lunit
      integer, intent(out) :: iret

!  check for i8 integers
!  ---------------------
      if(im8) then
         im8=.false.
         call fortran_open_8(filename,lunit,format,position,iret)
         im8=.true.
         return
      endif

      open(lunit, file=trim(filename), form=trim(format), &
                  position=trim(position), iostat=iret)

     end subroutine
