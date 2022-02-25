!> @file
!> @brief Enable a number of BUFRLIB subprograms to be called
!>        via module interfaces from Fortran application programs.
!>
!> @author J. Ator
!> @date 2022-02-01

module bufr_procedures

  use subroutine_closbf
  use subroutine_closmg
  use subroutine_openbf
  use subroutine_openmb
  use subroutine_openmg
  use subroutine_ufbint
  use subroutine_ufbrep
  use subroutine_upftbv
  use subroutine_writsa

end module
