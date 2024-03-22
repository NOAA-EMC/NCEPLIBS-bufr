!> @file
!> @brief Read and write tank receipt times within BUFR messages
!> @author J. Ator @date 2009-03-23

!> Read an input message and output an equivalent BUFR message with a tank receipt time added to Section 1.
!>
!> The tank receipt time to be added must have been specified via a previous call to subroutine strcpt(). This subroutine
!> performs the same function as subroutine strcpt() when the latter is called with CF = 'Y', except that the latter
!> subroutine operates on BUFR messages internally within the software, whereas this subroutine operates on a single BUFR
!> message passed in via a memory array.
!>
!> @remarks
!> - msgin and msgot must be separate arrays
!> - msgot will be longer in length than msgin, so the user must allow for extra space when allocating msgot within the
!> application program
!>
!> @param[in] msgin - integer(*): BUFR message
!> @param[in] lmsgot - integer: Dimensioned size (in integers) of msgot; used by the subroutine to ensure that
!> it doesn't overflow the msgot array
!> @param[out] msgot - integer(*): Copy of msgin with a tank receipt time added to Section 1
!>
!> @author J. Ator @date 2009-03-23
recursive subroutine atrcpt(msgin,lmsgot,msgot)

  use modv_vars, only: im8b, nbytw

  use moda_tnkrcp

  implicit none

  integer, intent(in) :: msgin(*), lmsgot
  integer, intent(out) :: msgot(*)
  integer my_lmsgot, len0, len1, l2, l3, l4, l5, iad1, iad2, lenm, lenmot, len1ot, ibit, iupbs01

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84 ( lmsgot, my_lmsgot, 1 )
    call atrcpt ( msgin, my_lmsgot*2, msgot )

    im8b=.true.
    return
  endif

  ! Get some section lengths and addresses from the input message.

  call getlens(msgin,1,len0,len1,l2,l3,l4,l5)

  iad1 = len0
  iad2 = iad1 + len1

  lenm = iupbs01(msgin,'LENM')

  ! Check for overflow of the output array.  Note that the new message will be 6 bytes longer than the input message.

  lenmot = lenm + 6
  if(lenmot.gt.(lmsgot*nbytw)) &
    call bort('BUFRLIB: ATRCPT - OVERFLOW OF OUTPUT MESSAGE ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY')

  len1ot = len1 + 6

  ! Write Section 0 of the new message into the output array.

  call mvb ( msgin, 1, msgot, 1, 4 )
  ibit = 32
  call pkb ( lenmot, 24, msgot, ibit )
  call mvb ( msgin, 8, msgot, 8, 1 )

  ! Store the length of the new Section 1.

  ibit = iad1*8
  call pkb ( len1ot, 24, msgot, ibit )

  ! Copy the remainder of Section 1 from the input array to the output array.

  call mvb ( msgin, iad1+4, msgot, (ibit/8)+1, len1-3 )

  ! Append the tank receipt time data to the new Section 1.

  ibit = iad2*8
  call pkb ( itryr, 16, msgot, ibit )
  call pkb ( itrmo,  8, msgot, ibit )
  call pkb ( itrdy,  8, msgot, ibit )
  call pkb ( itrhr,  8, msgot, ibit )
  call pkb ( itrmi,  8, msgot, ibit )

  ! Copy Sections 2, 3, 4 and 5 from the input array to the output array.

  call mvb ( msgin, iad2+1, msgot, (ibit/8)+1, lenm-iad2 )

  return
end subroutine atrcpt

!> Read the tank receipt time (if one exists) from Section 1 of a BUFR message.
!>
!> This subroutine is similar to subroutine rtrcpt(), except that it operates on a BUFR message passed in via a memory array,
!> whereas rtrcpt() operates on the BUFR message that was read into internal arrays via the most recent call to any of the
!> other [message-reading subroutines](@ref hierarchy) for a specified Fortran logical unit.
!>
!> @param[in]  mbay - integer(*): BUFR message
!> @param[out] iyr  - integer: Tank receipt year
!> @param[out] imo  - integer: Tank receipt month
!> @param[out] idy  - integer: Tank receipt day
!> @param[out] ihr  - integer: Tank receipt hour
!> @param[out] imi  - integer: Tank receipt minute
!> @param[out] iret - integer: return code:
!> - 0 = normal return
!> - -1 = no tank receipt time exists within mbay
!>
!> @author J. Ator @date 2013-10-07
recursive subroutine rtrcptb(mbay,iyr,imo,idy,ihr,imi,iret)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: mbay(*)
  integer, intent(out) :: iyr, imo, idy, ihr, imi, iret
  integer is1byt, imgbyt, iupbs01, iupb

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call rtrcptb(mbay,iyr,imo,idy,ihr,imi,iret)
    call x48(iyr,iyr,1)
    call x48(imo,imo,1)
    call x48(idy,idy,1)
    call x48(ihr,ihr,1)
    call x48(imi,imi,1)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  iret = -1

  ! Check whether the message contains a tank receipt time.

  if(iupbs01(mbay,'BEN').eq.4) then
    is1byt = 23
  else
    is1byt = 19
  endif
  if( (is1byt+5) .gt. iupbs01(mbay,'LEN1') ) return

  ! Unpack the tank receipt time.

  ! Note that is1byt is a starting byte number relative to the beginning of Section 1, so we still need to account for
  ! Section 0 when specifying the actual byte numbers to unpack within the overall message.

  imgbyt = is1byt + iupbs01(mbay,'LEN0')

  iyr = iupb(mbay,imgbyt,16)
  imo = iupb(mbay,imgbyt+2,8)
  idy = iupb(mbay,imgbyt+3,8)
  ihr = iupb(mbay,imgbyt+4,8)
  imi = iupb(mbay,imgbyt+5,8)

  iret = 0

  return
end subroutine rtrcptb

!> Read the tank receipt time (if one exists) from Section 1 of a BUFR message.
!>
!> This subroutine is similar to subroutine rtrcptb(), except that rtrcptb() operates on a BUFR message passed in via a
!> memory array, whereas this subroutine operates on the BUFR message that was read into internal arrays via the most
!> recent call to any of the other [message-reading subroutines](@ref hierarchy) for a specified Fortran logical unit.
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!> @param[out] iyr  - integer: Tank receipt year
!> @param[out] imo  - integer: Tank receipt month
!> @param[out] idy  - integer: Tank receipt day
!> @param[out] ihr  - integer: Tank receipt hour
!> @param[out] imi  - integer: Tank receipt minute
!> @param[out] iret - integer: return code
!>  - 0 = normal return
!>  - -1 = no tank receipt time exists within the BUFR message currently open for input within internal arrays
!>
!> @author J. Ator @date 2009-03-23
recursive subroutine rtrcpt(lunit,iyr,imo,idy,ihr,imi,iret)

  use modv_vars, only: im8b

  use moda_bitbuf

  implicit none

  integer, intent(in) :: lunit
  integer, intent(out) :: iyr, imo, idy, ihr, imi, iret
  integer my_lunit, lun, il, im

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call rtrcpt(my_lunit,iyr,imo,idy,ihr,imi,iret)
    call x48(iyr,iyr,1)
    call x48(imo,imo,1)
    call x48(idy,idy,1)
    call x48(ihr,ihr,1)
    call x48(imi,imi,1)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  ! Check the file status.

  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: RTRCPT - INPUT BUFR FILE IS CLOSED; IT MUST BE OPEN FOR INPUT')
  if(il.gt.0) call bort('BUFRLIB: RTRCPT - INPUT BUFR FILE IS OPEN FOR OUTPUT; IT MUST BE OPEN FOR INPUT')
  if(im.eq.0) call bort('BUFRLIB: RTRCPT - A MESSAGE MUST BE OPEN IN INPUT BUFR FILE; NONE ARE')

  ! Unpack the tank receipt time.

  call rtrcptb(mbay(1,lun),iyr,imo,idy,ihr,imi,iret)

  return
end subroutine rtrcpt

!> Specify a tank receipt time to be included within Section 1 of all BUFR messages output by future calls
!> to [message-writing subroutines](@ref hierarchy) and [subset-writing subroutines](@ref hierarchy).
!>
!> This subroutine can be called at any time after the first call to subroutine openbf(), and the specified value for cf will
!> remain in effect for all future calls to [message-writing subroutines](@ref hierarchy) and
!> [subset-writing subroutines](@ref hierarchy), for all Fortran logical units that are open for output within the application
!> program, unless a subsequent call is made to this subroutine to reset the value of cf again.  If this subroutine is never
!> called, a default value of 'N' is used for cf, as set within module @ref moda_tnkrcp.
!>
!> Whenever this subroutine is called with cf = 'N', the values in iyr, imo, idy, ihr, and imi are ignored.
!>
!> @remarks
!> - Tank receipt time is an NCEP extension to Section 1 of the [official WMO BUFR regulations](@ref manual).
!> However, it's encoded by the NCEPLIBS-bufr software in such a way that its inclusion within an output BUFR message is
!> still fully compliant with the regulations.
!>
!> @param[in] cf - character*1: Flag indicating whether future BUFR output messages should include the tank receipt
!> time defined by iyr, imo, idy, ihr, and imi:
!> - 'N' = No (the default)
!> - 'Y' = Yes
!> @param[in] iyr - integer: Tank receipt year
!> @param[in] imo - integer: Tank receipt month
!> @param[in] idy - integer: Tank receipt day
!> @param[in] ihr - integer: Tank receipt hour
!> @param[in] imi - integer: Tank receipt minute
!>
!> @author J. Ator @date 2009-03-23
recursive subroutine strcpt(cf,iyr,imo,idy,ihr,imi)

  use modv_vars, only: im8b

  use moda_tnkrcp

  implicit none

  character*1, intent(in) :: cf
  character*128 bort_str

  integer, intent(in) :: iyr, imo, idy, ihr, imi
  integer my_iyr, my_imo, my_idy, my_ihr, my_imi

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(iyr,my_iyr,1)
    call x84(imo,my_imo,1)
    call x84(idy,my_idy,1)
    call x84(ihr,my_ihr,1)
    call x84(imi,my_imi,1)
    call strcpt(cf,my_iyr,my_imo,my_idy,my_ihr,my_imi)

    im8b=.true.
    return
  endif

  call capit(cf)
  if(cf.ne.'Y' .and. cf.ne.'N') then
    write(bort_str,'("BUFRLIB: STRCPT - INPUT ARGUMENT IS ",A1,", IT MUST BE EITHER Y OR N")') cf
    call bort(bort_str)
  endif

  ctrt = cf
  if(ctrt.eq.'Y') then
    itryr = iyr
    itrmo = imo
    itrdy = idy
    itrhr = ihr
    itrmi = imi
  endif

  return
end subroutine strcpt
