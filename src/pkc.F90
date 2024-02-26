!> @file
!> @brief Encode a character string within an integer array.
!>
!> @author J. Woollen @date 1994-01-06

!> Encode a character string within a specified number of bytes of an integer array,
!> starting at the bit immediately after a specified bit within the array.
!>
!> @remarks
!> - This subroutine is the logical inverse of subroutine upc().
!> - On input, there is no requirement that ibit must point to the first
!>   bit of a byte within ibay.  Correspondingly, on output there is no
!>   guarantee that the nchr characters of chr will be aligned on byte
!>   boundaries when encoded within ibay.
!>
!> @param[in] chr - character*(*): String to be encoded.
!> @param[in] nchr - integer: Number of bytes of ibay within
!> which to encode chr (i.e. the number of characters in chr).
!> @param[out] ibay - integer(*): Array containing encoded chr.
!> @param[in,out] ibit - integer: Bit pointer within ibay
!>  - On input, ibit points to the bit within ibay after which
!> to begin encoding chr.
!>  - On output, ibit points to the last bit of ibay which
!> contains the encoded chr.
!>
!> @author J. Woollen @date 1994-01-06
subroutine pkc(chr,nchr,ibay,ibit)

  use modv_vars, only: nbitw, nbytw, iordle, iordbe

  implicit none

  character*(*), intent(in) :: chr

  integer, intent(in) :: nchr

  integer, intent(out) :: ibay(*)

  integer, intent(inout) :: ibit

  integer ival(2), lb, i, nwd, nbt, nbit, int, msk, irev

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

  ival(1) = 0
  nbit = 8

  do i=1,nchr
    if(i.le.len(chr)) then
      cval(lb) = chr(i:i)
    else
      cval(lb) = ' '
    endif

    nwd  = ibit/nbitw + 1
    nbt  = mod(ibit,nbitw)
    int = ishft(ival(1),nbitw-nbit)
    int = ishft(int,-nbt)
    msk = ishft(  -1,nbitw-nbit)
    msk = ishft(msk,-nbt)
    ibay(nwd) = irev(ior(iand(irev(ibay(nwd)),not(msk)),int))
    if(nbt+nbit.gt.nbitw) then

!     This character will not fit within the current word (i.e.
!     array member) of ibay, because there are less than 8 bits of
!     space left.  Store as many bits as will fit within the current
!     word and then store the remaining bits within the next word.

      int = ishft(ival(1),2*nbitw-(nbt+nbit))
      msk = ishft(  -1,2*nbitw-(nbt+nbit))
      ibay(nwd+1) = irev(ior(iand(irev(ibay(nwd+1)),not(msk)),int))
    endif
    ibit = ibit + nbit
  enddo

  return
end subroutine pkc
