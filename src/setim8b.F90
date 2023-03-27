!> @file
!> @brief Use 8-byte integer arguments to BUFRLIB subprograms from
!> Fortran.
!>
!> @author J. Ator @date 2022-09-01

!> Specify whether all integer arguments to BUFRLIB subprograms from
!> Fortran will use 8-byte integers.
!>
!> The default value is .false., meaning that if this subroutine is
!> never called, then the BUFRLIB will expect that all future calls
!> from the application program will use 4-byte integer arguments.
!> Otherwise, the specification in any call to this subroutine remains
!> in effect unless and until it is overridden by a subsequent future
!> call to this same subroutine.
!>
!> @param[in] INT8B - logical: .true. iff 8-byte integers will be used
!> for all integer arguments passed to and from all future calls to
!> BUFRLIB subprograms from within the application program
!>
!> @author J. Ator @date 2022-09-01
subroutine setim8b ( int8b )

  use modv_im8b

  logical, intent(in) :: int8b

  im8b = int8b

return
end
