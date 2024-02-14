!> @file
!> @brief Encode an 8-byte integer value within an integer array.
!>
!> @author J. Woollen @date 2022-05-06

!> Encode an 8-byte integer value within a specified
!> number of bits of an integer array, starting at the bit
!> immediately after a specified bit within the array.
!>
!> This subroutine will not work properly if nbits is less than 0 or
!> greater than 64.
!>
!> This subroutine is the logical inverse of subroutine up8().
!>
!> @param[in] nval - integer*8: Value to be encoded.
!> @param[in] nbits - integer: Number of bits of ibay within
!> which to encode nval. Must be between 0 and 64.
!> @param[out] ibay - integer(*): Array containing encoded nval.
!> @param[in,out] ibit - integer: Bit pointer within ibay
!> - On input, ibit points to the bit within ibay after which to begin encoding nval.
!> - On output, ibit points to the last bit of ibay which contains the encoded nval.
!>
!> @author J. Woollen @date 2022-05-06
subroutine pkb8(nval,nbits,ibay,ibit)

  use modv_vars, only: nbitw

  implicit none

  integer(8), intent(in) :: nval
  integer(4), intent(in) :: nbits
  integer(4), intent(out) :: ibay(*)
  integer(4), intent(inout) :: ibit

  integer(8) :: nval8
  integer(4) :: nval4
  integer(4) :: nvals(2)

  equivalence (nval8,nvals)

!----------------------------------------------------------------------
!----------------------------------------------------------------------

  if(nbits<0 ) call bort('bufrlib: pkb8 - nbits < zero !!!!!')
  if(nbits>64) call bort('bufrlib: pkb8 - nbits > 64   !!!!!')

  nval8=nval
  nval4=nvals(2)
  call pkb(nval4,max(nbits-nbitw,0),ibay,ibit)
  nval4=nvals(1)
  call pkb(nval4,min(nbits,nbitw  ),ibay,ibit)

end subroutine pkb8
