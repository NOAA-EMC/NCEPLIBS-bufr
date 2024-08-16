!> @file
!> @brief Encode character strings and integer values.
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
!> @param chr - String to be encoded
!> @param nchr - Number of bytes of ibay within which to encode chr (i.e. the number of characters in chr)
!> @param ibay - Array containing encoded chr
!> @param ibit - Bit pointer within ibay
!>  - On input, ibit points to the bit within ibay after which to begin encoding chr
!>  - On output, ibit points to the last bit of ibay which contains the encoded chr
!>
!> @author J. Woollen @date 1994-01-06
subroutine pkc(chr,nchr,ibay,ibit)

  use modv_vars, only: nbitw, nbytw, iordle, iordbe

  implicit none

  character*(*), intent(in) :: chr
  character*1 cval(8)

  integer, intent(in) :: nchr
  integer, intent(out) :: ibay(*)
  integer, intent(inout) :: ibit
  integer ival(2), lb, i, nwd, nbt, nbit, int, msk, irev

  equivalence (cval,ival)

  ! Set lb to point to the "low-order" (i.e. least significant) byte within a machine word.

#ifdef BIG_ENDIAN
  lb = iordbe(nbytw)
#else
  lb = iordle(nbytw)
#endif

  ival(1) = 0
  nbit = 8

  do i=1,nchr
    if(i<=len(chr)) then
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
    if(nbt+nbit>nbitw) then

      ! This character will not fit within the current word (i.e. array member) of ibay, because there
      ! are less than 8 bits of space left.  Store as many bits as will fit within the current
      ! word and then store the remaining bits within the next word.

      int = ishft(ival(1),2*nbitw-(nbt+nbit))
      msk = ishft(  -1,2*nbitw-(nbt+nbit))
      ibay(nwd+1) = irev(ior(iand(irev(ibay(nwd+1)),not(msk)),int))
    endif
    ibit = ibit + nbit
  enddo

  return
end subroutine pkc

!> Encode an 8-byte integer value within a specified number of bits of an integer array,
!> starting at the bit immediately after a specified bit within the array.
!>
!> This subroutine will not work properly if nbits is greater than 64.
!>
!> This subroutine is the logical inverse of subroutine up8().
!>
!> @param nval - Value to be encoded
!> @param nbits - Number of bits of ibay (up to a maximum of 64) within which to encode nval
!> @param ibay - Array containing encoded nval
!> @param ibit - Bit pointer within ibay
!> - On input, ibit points to the bit within ibay after which to begin encoding nval
!> - On output, ibit points to the last bit of ibay which contains the encoded nval
!>
!> @author J. Woollen @date 2022-05-06
subroutine pkb8(nval,nbits,ibay,ibit)

  use modv_vars, only: nbitw

  implicit none

  integer*8, intent(in) :: nval
  integer, intent(in) :: nbits
  integer, intent(out) :: ibay(*)
  integer, intent(inout) :: ibit

  integer*8 :: nval8
  integer :: nval4, nvals(2)

  equivalence (nval8,nvals)

  if(nbits<0) call bort('bufrlib: pkb8 - nbits < zero !!!!!')
  if(nbits>64) call bort('bufrlib: pkb8 - nbits > 64   !!!!!')

  nval8=nval
  nval4=nvals(2)
  call pkb(nval4,max(nbits-nbitw,0),ibay,ibit)
  nval4=nvals(1)
  call pkb(nval4,min(nbits,nbitw),ibay,ibit)

  return
end subroutine pkb8

!> Encode an integer value within a specified number of bits of an integer array,
!> starting at the bit immediately after a specified bit within the array.
!>
!> @param nval  - Value to be encoded
!> @param nbits - Number of bits of ibay within which to encode nval
!> @param ibay - Array containing encoded nval
!> @param ibit - Bit pointer within ibay
!>  - On input, ibit points to the bit within ibay after which to begin encoding nval
!>  - On output, ibit points to the last bit of ibay which contains the encoded nval
!>
!> @remarks
!> - This subroutine is the logical inverse of subroutine upb().
!> - This subroutine will not work properly if nbits is greater than 32.
!>
!> @author J. Woollen @date 1994-01-06
subroutine pkb(nval,nbits,ibay,ibit)

  use modv_vars, only: nbitw

  implicit none

  integer, intent(in) :: nval, nbits
  integer, intent(out) :: ibay(*)
  integer, intent(inout) :: ibit
  integer nwd, nbt, ival, int, msk, irev

  character*156 bort_str

  if(nbits>nbitw) then
    write(bort_str,'("BUFRLIB: PKB - NUMBER OF BITS BEING PACKED '// &
      ', NBITS (",I4,"), IS > THE INTEGER WORD LENGTH ON THIS MACHINE, NBITW (",I3,")")') nbits,nbitw
    call bort(bort_str)
  endif

  nwd  = ibit/nbitw + 1
  nbt  = mod(ibit,nbitw)
  ival = nval
  if(ishft(ival,-nbits)>0) ival = -1
  int = ishft(ival,nbitw-nbits)
  int = ishft(int,-nbt)
  msk = ishft(-1,nbitw-nbits)
  msk = ishft(msk,-nbt)
  ibay(nwd) = irev(ior(iand(irev(ibay(nwd)),not(msk)),int))
  if(nbt+nbits>nbitw) then

    ! There are less than nbits bits remaining within the current word (i.e. array member) of ibay,
    ! so store as many bits as will fit within the current word and then store the remaining bits
    ! within the next word.

    int = ishft(ival,2*nbitw-(nbt+nbits))
    msk = ishft(  -1,2*nbitw-(nbt+nbits))
    ibay(nwd+1) = irev(ior(iand(irev(ibay(nwd+1)),not(msk)),int))
  endif

  ibit = ibit + nbits

  return
end subroutine pkb

!> Encode an integer value within a specified number of bytes of a character string,
!> up to a maximum of 4 bytes.
!>
!> This subroutine is the logical inverse of subroutine iupm().
!>
!> @param n - Value to be encoded
!> @param nbyt - Number of bytes of cbay (up to a maximum of 4) within which to encode n
!> @param cbay - String of length nbyt bytes containing encoded integer n
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ipkm(cbay,nbyt,n)

  use modv_vars, only: im8b, nbytw

  implicit none

  integer, intent(in) :: n, nbyt
  integer my_n, my_nbyt, int, irev, i

  character*(*), intent(out) :: cbay
  character*128 bort_str
  character*4 cint

  equivalence (cint,int)

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(n,my_n,1)
    call x84(nbyt,my_nbyt,1)
    call ipkm(cbay,my_nbyt,my_n)

    im8b=.true.
    return
  endif

  if(nbyt>nbytw) then
    write(bort_str,'("BUFRLIB: IPKM - NUMBER OF BYTES BEING PACKED '// &
      ', NBYT (",I4,"), IS > THE INTEGER WORD LENGTH ON THIS MACHINE, NBYTW (",I3,")")') nbyt,nbytw
    call bort(bort_str)
  endif

  int = irev(ishft(n,(nbytw-nbyt)*8))
  do i=1,nbyt
    cbay(i:i) = cint(i:i)
  enddo

  return
end subroutine ipkm

!> Pack a real*8 value into an integer by applying the proper scale and reference values.
!>
!> Normally the scale and reference values are obtained from index
!> node of the internal jump/link table arrays isc(*) and irf(*);
!> however, the reference value in irf(*) will be overridden if a
!> 2-03 operator is in effect for this node.
!>
!> @param val - User value
!> @param node - Index into internal jump/link tables
!>
!> @return ipks - Packed BUFR integer
!>
!> @remarks
!> - This function is the logical inverse of function ups().
!>
!> @author J. Ator @date 2012-03-02
integer*8 function ipks(val,node) result(i8ret)

  use moda_tables
  use moda_nrv203

  implicit none

  integer*8 imask
  integer, intent(in) :: node
  integer jj

  real*8, parameter :: ten = 10.
  real*8, intent(in) :: val

  i8ret = nint(val * ten**isc(node),8) - irf(node)

  if ( nnrv > 0 ) then
    ! There are redefined reference values in the jump/link table, so we need to check if this node is affected by any of them.
    do jj = 1, nnrv
      if ( node == inodnrv(jj) ) then
        ! This node contains a redefined reference value. Per the rules of BUFR, negative values should be encoded as positive
        ! integers with the left-most bit set to 1.
        nrv(jj) = nint(val)
        if ( nrv(jj) < 0 ) then
          imask = 2_8**(ibt(node)-1)
          i8ret = ior(abs(nrv(jj)),imask)
        else
          i8ret = nrv(jj)
        end if
        return
      else if ( ( tag(node)(1:8) == tagnrv(jj) ) .and. ( node >= isnrv(jj) ) .and. ( node <= ienrv(jj) ) ) then
        ! The corresponding redefinded reference value needs to be used when encoding this value.
        i8ret = nint(val * ten**isc(node),8) - nrv(jj)
        return
      end if
    end do
  end if

  return
end function ipks
