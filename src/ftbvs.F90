!> @file
!> @brief Convert between flag table bit settings and numerical data values.
!>
!> @author J. Ator @date 2005-11-29

!> Compute the numerical value equivalent to the setting of bit #(ibit) within a flag table of nbits bits.
!>
!> If the computation fails for any reason, then the function returns the current placeholder value for "missing" data.
!>
!> @param[in] nbits - integer: Total number of bits in flag table
!> @param[in] ibit  - integer: Number of bit to be set
!>
!> @returns pkftbv  - real*8: Value equivalent to the setting of bit #(ibit) within a flag table of nbits bits
!>
!> @remarks
!> - This function is the logical inverse of subroutine upftbv().
!> - According to the WMO standard, bits within a bit field are numbered from left to right, so bit #1 is always the
!> high-order (i.e. most significant) bit in any bit field.
!>
!> @author J. Ator @date 2005-11-29
recursive real*8 function pkftbv(nbits,ibit) result(r8val)

  use modv_vars, only: im8b, bmiss

  implicit none

  integer, intent(in) :: nbits, ibit
  integer my_nbits, my_ibit

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(nbits,my_nbits,1)
    call x84(ibit,my_ibit,1)
    r8val=pkftbv(my_nbits,my_ibit)

    im8b=.true.
    return
  endif

  if((nbits.le.0).or.(ibit.le.0).or.(ibit.gt.nbits)) then
    r8val = bmiss
  else
    r8val = (2.)**(nbits-ibit)
  endif

  return
end function pkftbv

!> Given a Table B mnemonic with flag table units and a corresponding numerical data value, this subroutine determines
!> the bit settings equivalent to that numerical value.
!>
!> This subroutine is the logical inverse of function pkftbv().
!>
!> According to the WMO standard, bits within a bit field are numbered from left to right, so bit #1 is always the
!> high-order i.e. most significant) bit in any bit field.
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!> @param[in] nemo - character*(*): Table B mnemonic with flag table units
!> @param[in] val - real*8: Value corresponding to nemo
!> @param[in] mxib - integer: Dimensioned size (in integers) of ibit in the calling program; used by the subroutine
!> to ensure that it doesn't overflow the ibit array
!> @param[out] ibit - integer: Bit numbers which were set to "On" (i.e. set to "1") in val
!> @param[out] nib - integer: Number of bit numbers returned in ibit
!>
!> @author J. Ator @date 2005-11-29
recursive subroutine upftbv(lunit,nemo,val,mxib,ibit,nib)

  use modv_vars, only: im8b

  use moda_tababd

  implicit none

  integer, intent(in) :: lunit, mxib
  integer, intent(out) :: ibit(*), nib(*)
  integer my_lunit, my_mxib, lun, il, im, idn, i, n, nbits, iersn

  character*(*), intent(in) :: nemo
  character*128 bort_str
  character*1 tab

  real*8, intent(in) :: val
  real*8 r8val, r82i

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call x84(mxib,my_mxib,1)
    call upftbv( my_lunit, nemo, val, my_mxib*2, ibit, nib )
    call x48(ibit,ibit,nib(1))
    call x48(nib,nib,1)

    im8b=.true.
    return
  endif

  ! Perform some sanity checks.

  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: UPFTBV - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')

  call nemtab(lun,nemo,idn,tab,n)
  if(n.eq.0) then
    write(bort_str,'("BUFRLIB: UPFTBV - MNEMONIC ",A," NOT FOUND IN TABLE B")') nemo
    call bort(bort_str)
  endif
  if(tabb(n,lun)(71:74).ne.'FLAG') then
    write(bort_str,'("BUFRLIB: UPFTBV - MNEMONIC ",A," IS NOT A FLAG TABLE")') nemo
    call bort(bort_str)
  endif

  ! Figure out which bits are set.

  nib(1) = 0
  r8val = val
  call strnum(tabb(n,lun)(110:112),nbits,iersn)
  do i=(nbits-1),0,-1
    r82i = (2.)**i
    if(abs(r8val-r82i).lt.(0.005)) then
      nib(1) = nib(1) + 1
      if(nib(1).gt.mxib) call bort('BUFRLIB: UPFTBV - IBIT ARRAY OVERFLOW')
      ibit(nib(1)) = nbits-i
      return
    elseif(r82i.lt.r8val) then
      nib(1) = nib(1) + 1
      if(nib(1).gt.mxib) call bort('BUFRLIB: UPFTBV - IBIT ARRAY OVERFLOW')
      ibit(nib(1)) = nbits-i
      r8val = r8val - r82i
    endif
  enddo

  return
end subroutine upftbv
