!> @file
!> @brief Open a Fortran file on the local system
!>
!> ### Program History
!> Date | Programmer | Comments 
!> -----|------------|----------
!> 2015-08-30 | J. Whitaker | Original author
!> 2022-08-04 | J. Woollen | Added 8-byte wrapper
!>
!> @author Jeff Whitaker @date 2015-08-30

!> Open a Fortran file on the local system.
!>
!> @param[in] filename - character*(*): name of the file to be opened
!> @param[in] lunit    - integer: Fortran logical unit number for Fortran file
!> @param[in] format   - character*(*): format of the Fortran file
!> @param[in] position - character*(*): to rewind or continue with open file
!> @param[out] iret    - integer: return code from the Fortran open statement
!>
!> @author Jeff Whitaker @date 2015-08-30
   recursive subroutine fortran_open(filename, lunit, format, position, iret)

      use modv_im8b

      implicit none
      character*(*), intent(in) :: filename, format, position
      integer, intent(in)  :: lunit
      integer, intent(out) :: iret
      integer my_lunit

!     check for i8 integers

      if(im8b) then
         im8b=.false.

         call x84(lunit,my_lunit,1)
         call fortran_open(filename,my_lunit,format,position,iret)
         call x48(iret,iret,1)

         im8b=.true.
         return
      endif

      open(lunit, file=trim(filename), form=trim(format), &
                  position=trim(position), iostat=iret)

   end subroutine
