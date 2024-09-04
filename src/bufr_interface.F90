!> @file
!> @brief Enable a number of C NCEPLIBS-bufr functions to be called from within Fortran
!> application programs.
!>
!> @author J. Ator @date 2023-03-22

!> Wrap C NCEPLIBS-bufr functions so they can be called from within Fortran application programs.
!>
!> @author J. Ator @date 2023-03-22

module bufr_interface

  interface

    !> Open a new file for reading or writing BUFR messages via a C language interface.
    !>
    !> Wraps cobfl() function.
    !>
    !> @param bfl - [path]/name of file to be opened
    !> @param io - Flag indicating how bfl is to be opened
    !>
    !> @author J. Ator @date 2023-03-22
    subroutine cobfl_c( bfl, io ) bind(C, name='cobfl')
      use iso_c_binding
      character(kind=c_char), intent(in) :: bfl(*)
      character(kind=c_char), intent(in), value :: io
    end subroutine cobfl_c

    !> Read the next BUFR message from the file that was opened via the most recent call
    !> to subroutine cobfl_c() with io = 'r'.
    !>
    !> Wraps crbmg() function.
    !>
    !> @param mxmb - Number of elements in bmg array; used by the function to
    !> ensure that it doesn't overflow the array.
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

    !> Write a BUFR message to the file that was opened via the most recent call
    !> to subroutine cobfl_c() with io = 'w'.
    !>
    !> Wraps cwbmg() function.
    !>
    !> @param bmg - BUFR message
    !> @param nmb - Size of BUFR message in bmg array
    !> @param iret - Return code 0 indicates success, any other value indicates failure
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine cwbmg_c( bmg, nmb, iret ) bind(C, name='cwbmg')
      use iso_c_binding
      character(kind=c_char), intent(in) :: bmg(*)
      integer(c_int), intent(in), value :: nmb
      integer(c_int), intent(out) :: iret
    end subroutine cwbmg_c

    !> Close all files that were opened via previous calls to subroutine cobfl_c().
    !>
    !> Wraps ccbfl() function.
    !>
    !> @author J. Ator @date 2005-11-29
    subroutine ccbfl_c() bind(C, name='ccbfl')
      use iso_c_binding
    end subroutine ccbfl_c

    !> Free all memory allocated via inittbf_c().
    !>
    !> Wraps dlloctbf() function.
    !>
    !> @author J. Ator @date 2017-11-03
    subroutine dlloctbf_c() bind(C, name='dlloctbf')
      use iso_c_binding
    end subroutine dlloctbf_c

  end interface

end module bufr_interface
