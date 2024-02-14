!> @file
!> @brief Decode a character string from an integer array.
!>
!> @author J. Woollen @date 1994-01-06

!> Decode a character string from within a specified number of bytes of an integer
!> array, starting at the bit immediately after a specified bit within the array.
!>
!> @remarks
!> - This subroutine is the logical inverse of subroutine pkc().
!> - On input, there is no requirement that ibit must point to the first
!>   bit of a byte within ibay.  In other words, the nchr characters to
!>   be decoded do not necessarily need to be aligned on byte boundaries
!>   within ibay.
!>
!> @param[out] chr - character*(*): Decoded string.
!> @param[in] nchr - integer: Number of bytes of ibay from within
!> which to decode chr (i.e. the number of characters in chr).
!> @param[in] ibay - integer(*): Array from which to decode chr.
!> @param[inout] ibit - integer: Bit pointer within ibay
!> - On input, ibit points to the bit within ibay after which to begin
!> decoding chr.
!> - On output, ibit points to the last bit of ibay which was decoded.
!> @param[in] cnvnull - logical: .true. if null characters in ibay
!> should be converted to blanks within chr; .false. otherwise
!>
!> @author J. Woollen @date 1994-01-06
subroutine upc(chr,nchr,ibay,ibit,cnvnull)

  use modv_vars, only: nbytw, iordle, iordbe

  implicit none

  character*(*), intent(out) :: chr

  integer, intent(in) :: nchr, ibay(*)

  integer, intent(inout) :: ibit

  logical, intent(in) :: cnvnull

  integer ival(2), lb, i, numchr

  character*1 cval(8)

  equivalence (cval,ival)

!----------------------------------------------------------------------
!----------------------------------------------------------------------

! Set lb to point to the "low-order" (i.e. least significant) byte within a machine word.

#ifdef BIG_ENDIAN
  lb = iordbe(nbytw)
#else
  lb = iordle(nbytw)
#endif

  cval = ' '

  numchr = min(nchr,len(chr))
  do i=1,numchr
    call upb(ival(1),8,ibay,ibit)
    if((ival(1).eq.0).and.(cnvnull)) then
      chr(i:i) = ' '
    else
      chr(i:i) = cval(lb)
    endif
  enddo

  return
end subroutine upc
