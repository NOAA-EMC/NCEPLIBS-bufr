!> @file
!> @brief Declare and initialize MXNAF variable.

!> This module declares and initializes the MXNAF variable.
!>
!> @author J. Ator
!> @date 2021-03-24

module MODV_MXNAF

!> @var mxnaf
!> Maximum number of associated fields that can be in effect at any given time for a Table B descriptor.

  integer, parameter, public :: MXNAF = 4

end module
