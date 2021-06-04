!> @file
!> @brief Declare and initialize BMISS variable.

!> This module declares and initializes the BMISS variable.
!>
!> @author J. Ator
!> @date 2021-03-24

module MODV_BMISS

!> @var bmiss
!> Current placeholder value to represent "missing"
!> data when reading from or writing to BUFR files;
!> this value can be changed at any time via a call
!> to subroutine setbmiss().

  real*8, public :: BMISS = 10E10_8

end module
