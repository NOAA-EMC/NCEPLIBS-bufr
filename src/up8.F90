!> @file
!> @brief Decode an 8-byte integer value from an integer array.
!>
!> @author J. Woollen @date 2022-05-06

!> Decode an 8-byte integer value from within a
!> specified number of bits of an integer array, starting at the bit
!> immediately after a specified bit within the array.
!>
!> This subroutine is similar to subroutine upb8(), except that here ibit is
!> both an input and an output argument, and the overall order
!> of the arguments is different.
!>
!> This subroutine is the logical inverse of subroutine pkb8().
!>
!> @param[in] ibay   - integer(*): Array containing encoded value
!> @param[in,out] ibit - integer: Bit pointer within ibay
!>                        - On input, ibit points to the bit within
!>                          ibay after which to begin decoding nval.
!>                        - On output, ibit points to the last bit
!>                          of ibay which contained the decoded nval.
!> @param[in] nbits  - integer: Number of bits to be decoded
!> @param[out] nval  - integer*8: Decoded value
!>
!> @author J. Woollen @date 2022-05-06
subroutine up8(nval,nbits,ibay,ibit)

  implicit none

  integer(4), intent(in) :: nbits, ibay(*)
  integer(4), intent(inout) :: ibit
  integer(8), intent(out) :: nval

!----------------------------------------------------------------------
!----------------------------------------------------------------------

  call upb8(nval,nbits,ibit,ibay)
  ibit = ibit+nbits

end subroutine up8
