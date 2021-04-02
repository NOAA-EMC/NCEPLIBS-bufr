!> @file
!> @brief Declare and initialize MXH4WLC variable.

!> This module declares and initializes the MXH4WLC variable.
!>
!> @author J. Ator
!> @date 2014-12-10

module MODV_MXH4WLC

!> @var mxh4wlc
!> Maximum number of long character strings that can be held
!> for writing into an uncompressed BUFR subset by future
!> internal calls to subroutine writlc().

  integer, parameter, public :: MXH4WLC = 10

end module
