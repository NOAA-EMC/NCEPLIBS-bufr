!> @file
!> @brief Decode character strings and integer values.
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
!> @param[out] chr - Decoded string
!> @param[in] nchr - Number of bytes of ibay from within which to decode chr (i.e. the number of characters in chr)
!> @param[in] ibay - Array from which to decode chr
!> @param[inout] ibit - Bit pointer within ibay
!> - On input, ibit points to the bit within ibay after which to begin decoding chr
!> - On output, ibit points to the last bit of ibay which was decoded
!> @param[in] cnvnull - .true. if null characters in ibay should be converted to blanks within chr; .false. otherwise
!>
!> @author J. Woollen @date 1994-01-06
subroutine upc(chr,nchr,ibay,ibit,cnvnull)

  use modv_vars, only: nbytw, iordle, iordbe

  implicit none

  character*(*), intent(out) :: chr
  character*1 cval(8)

  integer, intent(in) :: nchr, ibay(*)
  integer, intent(inout) :: ibit
  integer ival(2), lb, i, numchr

  logical, intent(in) :: cnvnull

  equivalence (cval,ival)

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

!> Decode an 8-byte integer value from within a specified number of bits of an integer array, starting at the bit
!> immediately after a specified bit within the array.
!>
!> This subroutine is similar to subroutine up8(), except that here ibit is only an input argument, and the overall
!> order of the arguments is different.
!>
!> This subroutine will not work properly if nbits is greater than 64.
!>
!> @param[in] ibay - Array containing encoded value
!> @param[in] ibit - Bit within ibay after which to begin decoding nval
!> @param[in] nbits - Number of bits to be decoded
!> @param[out] nval - Decoded value
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

  return
end subroutine upb8

!> Decode an 8-byte integer value from within a specified number of bits of an integer array, starting at the bit
!> immediately after a specified bit within the array.
!>
!> This subroutine is similar to subroutine upb8(), except that here ibit is both an input and an output argument,
!> and the overall order of the arguments is different.
!>
!> This subroutine is the logical inverse of subroutine pkb8().
!>
!> @param[in] ibay - Array containing encoded value
!> @param[in,out] ibit - Bit pointer within ibay
!>   - On input, ibit points to the bit within ibay after which to begin decoding nval
!>   - On output, ibit points to the last bit of ibay which contained the decoded nval
!> @param[in] nbits - Number of bits to be decoded
!> @param[out] nval - Decoded value
!>
!> @author J. Woollen @date 2022-05-06
subroutine up8(nval,nbits,ibay,ibit)

  implicit none

  integer(4), intent(in) :: nbits, ibay(*)
  integer(4), intent(inout) :: ibit
  integer(8), intent(out) :: nval

  call upb8(nval,nbits,ibit,ibay)
  ibit = ibit+nbits

  return
end subroutine up8

!> Decode an integer value from within a specified number of bits of an integer array, starting at the bit
!> immediately after a specified bit within the array.
!>
!> This subroutine is similar to subroutine upb(), except that here ibit is only an input argument, and the overall
!> order of the arguments is different.
!>
!> @param[in] ibay - Array containing encoded value
!> @param[in] ibit - Bit within ibay after which to begin decoding nval
!> @param[in] nbits - Number of bits to be decoded
!> @param[out] nval - Decoded value
!>
!> @author J. Woollen @date 1994-01-06
subroutine upbb(nval,nbits,ibit,ibay)

  use modv_vars, only: nbitw

  implicit none

  integer, intent(in) :: ibay(*), ibit, nbits
  integer, intent(out) :: nval
  integer nwd, nbt, int, jnt, irev, lbt

  !  If nbits=0, then just set nval=0 and return

  if(nbits.eq.0) then
    nval=0
    return
  endif

  nwd = ibit/nbitw + 1
  nbt = mod(ibit,nbitw)
  int = ishft(irev(ibay(nwd)),nbt)
  int = ishft(int,nbits-nbitw)
  lbt = nbt+nbits
  if(lbt.gt.nbitw) then
    jnt = irev(ibay(nwd+1))
    int = ior(int,ishft(jnt,lbt-2*nbitw))
  endif
  nval = int

  return
end subroutine upbb

!> Decode an integer value from within a specified number of bits of an integer array, starting at the bit
!> immediately after a specified bit within the array.
!>
!> This subroutine is similar to subroutine upbb(), except that here ibit is both an input and an output argument,
!> and the overall order of the arguments is different.
!>
!> @param[in] ibay - Array containing encoded value
!> @param[in,out] ibit - Bit pointer within ibay
!>    - On input, ibit points to the bit within ibay after which to begin decoding nval
!>    - On output, ibit points to the last bit of ibay which contained the decoded nval
!> @param[in] nbits - Number of bits to be decoded
!> @param[out] nval - Decoded value
!>
!> @remarks
!> - This subroutine is the logical inverse of subroutine pkb().
!>
!> @author J. Woollen @date 1994-01-06
subroutine upb(nval,nbits,ibay,ibit)

  implicit none

  integer, intent(in) :: ibay(*), nbits
  integer, intent(out) :: nval
  integer, intent(inout) :: ibit

  call upbb(nval,nbits,ibit,ibay)
  ibit = ibit+nbits

  return
end subroutine upb

!> Decode an integer value from within a specified number of bits of an integer array, starting with the first
!> bit of a specified byte of the array.
!>
!> @param[in] mbay - Array containing encoded value
!> @param[in] nbyt - Byte within mbay at whose first bit to begin decoding
!> @param[in] nbit - Number of bits to be decoded, up to a maximum of 32
!>
!> @returns iupb - Decoded value
!>
!> @author J. Woollen @date 1994-01-06
recursive integer function iupb(mbay,nbyt,nbit) result(iret)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: mbay(*), nbit, nbyt
  integer my_nbit, my_nbyt, mbit

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(nbyt,my_nbyt,1)
    call x84(nbit,my_nbit,1)
    iret = iupb(mbay,my_nbyt,my_nbit)

    im8b=.true.
    return
  endif

  mbit = (nbyt-1)*8
  call upb(iret,nbit,mbay,mbit)

  return
end function iupb

!> Decode an integer value from within a specified number of bits of a character string, starting
!> with the first bit of the first byte of the string.
!>
!> This function is the logical inverse of subroutine ipkm().
!>
!> @param[in] cbay - String
!> @param[in] nbits - Number of bits from cbay to be decoded, up to a maximum of 32
!>
!> @returns iupm - Decoded value
!>
!> @author J. Woollen @date 1994-01-06
recursive integer function iupm(cbay,nbits) result(iret)

  use modv_vars, only: im8b, nbitw

  implicit none

  character*4, intent(in) :: cbay
  character*4 cint
  character*128 bort_str

  integer, intent(in) :: nbits
  integer my_nbits, int, irev

  equivalence (cint,int)

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(nbits,my_nbits,1)
    iret = iupm(cbay,my_nbits)

    im8b=.true.
    return
  endif

  iret = 0
  if(nbits.gt.nbitw) then
    write(bort_str,'("BUFRLIB: IUPM - NUMBER OF BITS BEING UNPACKED'// &
      ', NBITS (",I4,"), IS > THE INTEGER WORD LENGTH ON THIS MACHINE, NBITW (",I3,")")') nbits,nbitw
    call bort(bort_str)
  endif
  cint = cbay
  int = irev(int)
  iret = ishft(int,nbits-nbitw)

  return
end function iupm
