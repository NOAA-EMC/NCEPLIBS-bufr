!> @file
!> @brief Convert between 4-byte integers and 8-byte integers
!>
!> @author J. Woollen @date 2022-10-12

!> Encode one or more 4-byte integer values as 8-byte integer values.
!>
!> @note iin4 and iout8 may be the same array, as long as the underlying
!> memory contains sufficient space for the equivalent number of 8-byte
!> integer values.
!>
!> @param[in] iin4 - integer(*): Value(s) encoded as 4-byte integers
!> @param[out] iout8 - integer(*): Value(s) from iin4 now re-encoded as 8-byte integers
!> @param[in] nval - integer: Number of values in iin4 to be re-encoded
!>
!> @author J. Woollen @date 2022-10-12
subroutine x48(iin4,iout8,nval)

  implicit none

  integer, intent(in) :: iin4(*), nval
  integer, intent(out) :: iout8(*)

  integer, parameter :: zero = 0, ones = -1

  integer k, l, itmp, ilo, ihi

  ! ihi points to the first byte of the high-order (i.e. most significant) 4-byte integer word within an 8-byte integer word.
  ! ilo points to the first byte of the low-order (i.e. least significant) 4-byte integer word within an 8-byte integer word.

#ifdef BIG_ENDIAN
  ihi=1
  ilo=5
#else
  ihi=5
  ilo=1
#endif

  ! Re-encode the 4-byte values as 8-byte values.

  do k = nval,1,-1
    l=2*k-1
    call mvb(iin4(k),1,itmp,ilo,4)
    iout8(l)=itmp
    if(iout8(l)<0) then
      call mvb(ones,1,iout8(l),ihi,4)
    else
      call mvb(zero,1,iout8(l),ihi,4)
    endif
  enddo

  return
end subroutine x48

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

  ! ilo points to the first byte of the low-order (i.e. least significant) 4-byte integer word within an 8-byte integer word.

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
