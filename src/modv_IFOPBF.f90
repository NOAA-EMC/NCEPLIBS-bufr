!> @file
!> @brief Declare and initialize IFOPBF variable.

!> This module declares and initializes the IFOPBF variable.
!>
!> @author J. Ator
!> @date 2015-03-03

module MODV_IFOPBF

!> @var ifopbf
!> Status indicator to keep track of whether subroutine openbf() has already been called:
!> - 0 = No
!> - 1 = Yes

  integer, public :: IFOPBF = 0

end module
