!> @file
!> @brief Decode an 8-byte integer value from an integer array.
!>
!> @author J. Woollen @date 2022-05-06

!> Decode an 8-byte integer value from within a
!> specified number of bits of an integer array, starting at the bit
!> immediately after a specified bit within the array.
!>
!> This subroutine is similar to subroutine up8(), except that here ibit is
!> only an input argument, and the overall order of the arguments
!> is different.
!>
!> This subroutine will not work properly if nbits is less than 0 or
!> greater than 64.
!>
!> @param[in] ibay - integer(*): Array containing encoded value.
!> @param[in] ibit - integer: Bit within ibay after which to begin decoding nval.
!> @param[in] nbits - integer: Number of bits to be decoded.
!> @param[out] nval - integer*8: Decoded value.
!>
!> @author J. Woollen @date 2022-05-06
subroutine upb8(nval,nbits,ibit,ibay)

  use modv_vars, only: nbitw

  implicit none

  integer(4), intent(in) :: nbits,ibit,ibay(*)
  integer(8), intent(out) :: nval

  integer(4) :: nvals(2), jbit, ival
  integer(8) :: nval8

  equivalence (nval8,nvals)

!----------------------------------------------------------------------
!----------------------------------------------------------------------

  if(nbits<0) then
     call bort('BUFRLIB: UPB8 - nbits < zero !!!!!')
  elseif(nbits<=32) then
     jbit=ibit; ival=0
     call upb(ival,nbits,ibay,jbit)
     nval=ival
  elseif(nbits<=64) then
     jbit=ibit; nvals=0
     call upb(nvals(2),max(nbits-nbitw,0),ibay,jbit)
     call upb(nvals(1),min(nbitw,nbits  ),ibay,jbit)
     nval=nval8
  else
     nval=0
  endif

end subroutine upb8
