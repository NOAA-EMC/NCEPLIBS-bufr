!> @file
!> @brief Declare and initialize IM8B variable.

!> This module declares and initializes the IM8B variable.
!>
!> @author J. Woollen
!> @date 2022-08-04

module MODV_IM8B

!> @var im8b
!> Status indicator to keep track of whether all future calls to BUFRLIB subroutines and functions from a Fortran
!> application program will be made using 8-byte integer arguments.
!>
!> <p>The default value is .false., meaning that all future calls to BUFRLIB subroutines and functions will be
!> made using 4-byte integer arguments.  This value can be changed at any time via a call to subroutine setim8b().

  logical, public :: IM8B = .false.

end module
