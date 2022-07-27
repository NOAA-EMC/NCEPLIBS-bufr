!> @file
!> @brief Declare and initialize MAXCD variable.

!> This module declares and initializes the MAXCD variable.
!>
!> <p>This variable is initialized to a default value which can be overridden by a subsequent call to function isetprm()
!> within the application program.
!>
!> @author J. Ator
!> @date 2014-12-10

module MODV_MAXCD

!> @var maxcd
!> Maximum number of child descriptors that can be included within the sequence definition of a Table D descriptor,
!> not counting the recursive resolution of any child descriptors which may themselves be Table D descriptors.

  integer, public :: MAXCD = 250

end module
