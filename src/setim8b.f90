!> @file
!> @brief Specify the use of 8-byte integer arguments during future calls to BUFRLIB subprograms from a Fortran
!>        application program

!> This subroutine allows the user to specify whether all integer arguments passed to and from all future calls to
!> BUFRLIB subprograms from within a Fortran application program will use 8-byte integers.
!>
!> <p>The default value is .false., meaning that if this subroutine is never called, then the BUFRLIB will expect
!> that all future calls from the application program will use 4-byte integer arguments.  Otherwise, the specification
!> in any call to this subroutine remains in effect unless and until it is overridden by a subsequent future call to
!> this same subroutine.
!>
!> @author J. Ator
!> @date 2022-09-01
!>
!> @param[in] INT8B -- logical: .true. iff 8-byte integers will be used for all integer arguments passed to and from
!>                     all future calls to BUFRLIB subprograms from within the application program
!>
!> <b>Program history log:</b>
!> | Date | Programmer | Comments |
!> | -----|------------|----------|
!> | 2022-09-01 | J. Ator | Original author |
!>
subroutine setim8b ( int8b )

  use modv_im8b

  logical, intent(in) :: int8b

  im8b = int8b

return
end
