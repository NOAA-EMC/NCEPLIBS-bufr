!> @file
!> @brief Define signatures to enable a number of BUFRLIB functions to be called
!>        via wrapper functions from Fortran application programs.
!>
!> @author J. Ator @date 2023-03-22

module bufr_interface

interface

!> @brief Wraps BUFRLIB cobfl() function.
!>
!> @param bfl - [path]/name of system file to be opened
!> @param io - Flag indicating how bfl is to be opened
!>
!> @author J. Ator @date 2023-03-22
subroutine cobfl_c( bfl, io ) bind(C, name='cobfl')
  use iso_c_binding
  character(len=1), intent(in) :: bfl(*)
  character(len=1), intent(in), value :: io
end subroutine cobfl_c

!> @brief Wraps BUFRLIB crbmg() function.
!>
!> @param mxmb - Dimensioned size of bmg array in the calling program
!> @param bmg - BUFR message
!> @param nmb - Size of BUFR message in bmg array
!> @param iret - Return code 0 indicates success, any other value indicates failure
!>
!> @author J. Ator @date 2023-03-22
subroutine crbmg_c( bmg, mxmb, nmb, iret ) bind(C, name='crbmg')
  use iso_c_binding
  character(len=1), intent(out) :: bmg(*)
  integer(c_int), intent(in), value :: mxmb
  integer(c_int), intent(out) :: nmb, iret
end subroutine crbmg_c

!> @brief Wraps BUFRLIB cwbmg() function.
!>
!> @param bmg - BUFR message
!> @param nmb - Size (in bytes) of BUFR message in bmg
!> @param iret - Return code 0 indicates success, any other value indicates failure
!> 
!> @author J. Ator @date 2005-11-29
subroutine cwbmg_c( bmg, nmb, iret ) bind(C, name='cwbmg')
  use iso_c_binding
  character(len=1), intent(in) :: bmg(*)
  integer(c_int), intent(in), value :: nmb
  integer(c_int), intent(out) :: iret
end subroutine cwbmg_c

!> @brief Wraps BUFRLIB ccbfl() function.
!> 
!> @author J. Ator @date 2005-11-29
subroutine ccbfl_c() bind(C, name='ccbfl')
  use iso_c_binding
end subroutine ccbfl_c

end interface

end module bufr_interface
