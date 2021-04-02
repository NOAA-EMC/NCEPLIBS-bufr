!> @file
!> @brief Declare and initialize MAXNC variable.

!> This module declares and initializes the MAXNC variable.
!>
!> @author J. Ator
!> @date 2021-03-24

module MODV_MAXNC

!> @var maxnc
!> Maximum number of descriptors within Section 3 of
!> a BUFR message. 

  integer, parameter, public :: MAXNC = 600

end MODULE
