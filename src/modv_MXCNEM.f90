!> @file
!> @brief Declare and initialize MXCNEM variable.

!> This module declares and initializes the MXCNEM variable.
!>
!> @author J. Ator
!> @date 2021-03-24

module MODV_MXCNEM

!> @var mxcnem
!> Maximum number of entries in the internal Table A
!> mnemonic cache that is used for Section 3 decoding
!> of BUFR messages.

  integer, parameter, public :: MXCNEM = 450

end module
