!> @file
!> @brief Encode one or more 8-byte integer values as 4-byte integer values.
!>
!> @author J. Woollen @date 2022-10-12

!> Encode one or more 8-byte integer values as 4-byte integer values.
!>
!> @note iin8 and iout4 may be the same array.
!>
!> @param[in] iin8 - integer(*): Value(s) encoded as 8-byte integers
!> @param[out] iout4 - integer(*): Value(s) from iin8 now re-encoded as 4-byte integers
!> @param[in] nval - integer: Number of values in iin8 to be re-encoded
!>
!> @author J. Woollen @date 2022-10-12
subroutine x84(iin8,iout4,nval)
  implicit none

  integer, intent(in) :: iin8(*), nval
  integer, intent(out) :: iout4(*)

  integer k, l, itmp, ilo

! ilo points to the first byte of the low-order (i.e. least significant) 4-byte integer word
! within an 8-byte integer word.

#ifdef BIG_ENDIAN
  ilo=5
#else
  ilo=1
#endif

! Re-encode the 8-byte values as 4-byte values.

  do k = 1, nval
    l=2*k-1
    call mvb(iin8(l),ilo,itmp,1,4)
    iout4(k)=itmp
  enddo

  return
end subroutine x84
