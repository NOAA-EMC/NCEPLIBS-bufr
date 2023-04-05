!> @file
!> @brief Define signatures to enable a number of C NCEPLIBS-bufr functions to be called
!>        via wrapper functions from Fortran application programs.
!>
!> @author J. Ator @date 2023-03-22

!> This module contains signatures which wrap C NCEPLIBS-bufr functions so they can be
!> called from within Fortran application programs.
!>
!> @author J. Ator @date 2023-03-22

module bufr_interface

  interface

    !> @fn bufr_interface::cobfl_c::cobfl_c(bfl, io)
    !> Wraps NCEPLIBS-bufr cobfl() function.
    !>
    !> @param bfl - [path]/name of system file to be opened
    !> @param io - Flag indicating how bfl is to be opened
    !>
    !> @author J. Ator @date 2023-03-22
    subroutine cobfl_c( bfl, io ) bind(C, name='cobfl')
      use iso_c_binding
      character(kind=c_char), intent(in) :: bfl(*)
      character(kind=c_char), intent(in), value :: io
    end subroutine cobfl_c

    !> @fn bufr_interface::crbmg_c::crbmg_c(bmg, mxmb, nmb, iret)
    !> Wraps NCEPLIBS-bufr crbmg() function.
    !>
    !> @param mxmb - Dimensioned size of bmg array in the calling program
    !> @param bmg - BUFR message
    !> @param nmb - Size of BUFR message in bmg array
    !> @param iret - Return code 0 indicates success, any other value indicates failure
    !>
    !> @author J. Ator @date 2023-03-22
    subroutine crbmg_c( bmg, mxmb, nmb, iret ) bind(C, name='crbmg')
      use iso_c_binding
      character(kind=c_char), intent(out) :: bmg(*)
      integer(c_int), intent(in), value :: mxmb
      integer(c_int), intent(out) :: nmb, iret
    end subroutine crbmg_c

    !> @fn bufr_interface::cwbmg_c::cwbmg_c(bmg, nmb, iret)
    !> Wraps NCEPLIBS-bufr cwbmg() function.
    !>
    !> @param bmg - BUFR message
    !> @param nmb - Size (in bytes) of BUFR message in bmg
    !> @param iret - Return code 0 indicates success, any other value indicates failure
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine cwbmg_c( bmg, nmb, iret ) bind(C, name='cwbmg')
      use iso_c_binding
      character(kind=c_char), intent(in) :: bmg(*)
      integer(c_int), intent(in), value :: nmb
      integer(c_int), intent(out) :: iret
    end subroutine cwbmg_c

    !> @fn bufr_interface::ccbfl_c::ccbfl_c()
    !> Wraps NCEPLIBS-bufr ccbfl() function.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine ccbfl_c() bind(C, name='ccbfl')
      use iso_c_binding
    end subroutine ccbfl_c

  end interface

end module bufr_interface
