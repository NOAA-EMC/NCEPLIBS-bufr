!> @file
!> @brief Declare and initialize NFILES variable.

!> This module declares and initializes the NFILES variable.
!>
!> <p>This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!> within the application program.
!>
!> @author J. Ator
!> @date 2014-12-10

module MODV_NFILES

!>	  @var nfiles
!>        Maximum number of BUFR files that can be connected to the BUFRLIB software (for reading or writing) at any one time.

  integer, public :: NFILES = 32

end module
