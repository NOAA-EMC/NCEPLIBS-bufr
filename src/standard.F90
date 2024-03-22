!> @file
!> @brief Write WMO-standard BUFR messages
!>
!> @author J. Ator @date 2004-08-18

!> Specify whether BUFR messages output by
!> future calls to [message-writing subroutines](@ref hierarchy) and
!> [subset-writing subroutines](@ref hierarchy) should be internally
!> reformatted to remove all NCEPLIBS-bufr software extensions to the
!> WMO standard, prior to actually writing each message.
!>
!> It is strongly recommended to use this subroutine (or,
!> alternatively, subroutine stndrd() for messages which already exist
!> in memory arrays) whenever BUFR messages are being written that will
!> potentially be read using software other than the NCEPLIBS-bufr software.
!> Otherwise, by default the output messages will contain a number of
!> extensions to allow for faster reading and more efficient storage,
!> but which will be encoded using non-standard descriptors in
!> Section 3, and therefore likely be unrecognizable to other software
!> packages.
!>
!> This subroutine can be called at any time after the first call
!> to subroutine openbf(), and the specified value for cf will remain
!> in effect for all future calls to
!> [message-writing subroutines](@ref hierarchy) and
!> [subset-writing subroutines](@ref hierarchy) for all Fortran logical
!> units that are open for output within the application program,
!> unless a subsequent call is made to this subroutine to reset the
!> value of cf again.  If this subroutine is never called, a default
!> value of 'N' is used for cf, as set within subroutine bfrini().
!>
!> @param[in] cf - character*1: Flag indicating whether future BUFR output messages are to be standardized:
!>  - 'N' = No (the default)
!>  - 'Y' = Yes
!>
!> @author J. Ator @date 2004-08-18
subroutine stdmsg(cf)

  use moda_msgstd

  implicit none

  character*1, intent(in) :: cf
  character*128 bort_str

  call capit(cf)
  if(cf.ne.'Y'.and. cf.ne.'N') then
    write(bort_str,'("BUFRLIB: STDMSG - INPUT ARGUMENT IS ",A1,", IT MUST BE EITHER Y OR N")') cf
    call bort(bort_str)
  endif
  csmf = cf

  return
end subroutine stdmsg

!> Standardize a BUFR message.
!>
!> This subroutine performs the same function as subroutine stdmsg(), except that it operates on a BUFR message passed in via
!> a memory array and returns its output via a separate memory array, whereas stdmsg() operates on BUFR messages stored
!> internally within the software.
!>
!> @remarks
!> - msgin and msgot must be separate arrays
!> - Standardized messages are usually longer in length than their non-standard counterparts, so it's a good idea to allow
!> for extra space when allocating msgot within the application program
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!> @param[in] msgin - integer(*): BUFR message
!> @param[in] lmsgot - integer: Dimensioned size (in integers) of msgot; used by the subroutine to ensure that it doesn't
!> overflow the msgot array
!> @param[out] msgot - integer(*): Standardized copy of msgin
!>
!> @author J. Ator @date 2004-08-18
recursive subroutine stndrd(lunit,msgin,lmsgot,msgot)

  use bufrlib

  use modv_vars, only: im8b, nbytw

  use moda_s3list

  implicit none

  integer, intent(in) :: msgin(*), lunit, lmsgot
  integer, intent(out) :: msgot(*)
  integer my_lunit, my_lmsgot, lun, il, im, len0, len1, len2, len3, len4, len5
  integer iad3, iad4, lenn, lenm, iupbs01, iupbs3, iupb, mxbyto, lbyto, ii, isub, itab, mtyp, msbt, inod
  integer istdesc, ncd, iben, ibit, jbit, kbit, mbit, nad4, lsub, nsub, islen, kval, nval, i, k, l, n

  character*128 bort_str
  character*8 subset
  character*4 sevn
  character*1 tab
  character*(*), parameter :: bort_arrayoverflow = &
    'BUFRLIB: STNDRD - OVERFLOW OF OUTPUT (STANDARD) MESSAGE ARRAY; TRY A LARGER DIMENSION FOR THIS ARRAY'

  logical found

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84 ( lunit, my_lunit, 1 )
    call x84 ( lmsgot, my_lmsgot, 1 )
    call stndrd ( my_lunit, msgin, my_lmsgot*2, msgot )

    im8b=.true.
    return
  endif

  ! lunit must point to an open bufr file.

  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: STNDRD - BUFR FILE IS CLOSED, IT MUST BE OPEN')

  ! Identify the section lengths and addresses in msgin.

  call getlens(msgin,5,len0,len1,len2,len3,len4,len5)

  iad3 = len0+len1+len2
  iad4 = iad3+len3

  lenn = len0+len1+len2+len3+len4+len5

  lenm = iupbs01(msgin,'LENM')

  if(lenn.ne.lenm) then
    write(bort_str,'("BUFRLIB: STNDRD - INPUT MESSAGE LENGTH FROM SECTION 0",I6," DOES NOT EQUAL SUM OF ALL INDIVIDUAL '// &
      'SECTION LENGTHS (",I6,")")') lenm,lenn
    call bort(bort_str)
  endif

  mbit = (lenn-4)*8
  call upc(sevn,4,msgin,mbit,.true.)
  if(sevn.ne.'7777') then
    write(bort_str,'("BUFRLIB: STNDRD - INPUT MESSAGE DOES NOT END WITH ""7777"" (ENDS WITH ",A)') sevn
    call bort(bort_str)
  endif

  ! Copy Sections 0 through part of Section 3 into msgot.

  mxbyto = (lmsgot*nbytw) - 8

  lbyto = iad3+7
  if(lbyto.gt.mxbyto) call bort(bort_arrayoverflow)
  call mvb(msgin,1,msgot,1,lbyto)

  ! Rewrite new Section 3 in a standard form.  First, locate the top-level Table A descriptor.

  found = .false.
  ii = 10
  do while ((.not.found).and.(ii.ge.8))
    isub = iupb(msgin,iad3+ii,16)
    call numtab(lun,isub,subset,tab,itab)
    if((itab.ne.0).and.(tab.eq.'D')) then
      call nemtbax(lun,subset,mtyp,msbt,inod)
      if(inod.ne.0) found = .true.
    endif
    ii = ii - 2
  enddo
  if(.not.found) call bort('BUFRLIB: STNDRD - TABLE A SUBSET DESCRIPTOR NOT FOUND')

  if (istdesc(isub).eq.0) then
    ! isub is a non-standard Table A descriptor and needs to be expanded into an equivalent standard sequence
    call restd_c(lun,isub,ncd,ids3)
  else
    ! isub is already a standard descriptor, so just copy it "as is" into the new Section 3 (i.e. no expansion is necessary)
    ncd = 1
    ids3(ncd) = isub
  endif

  ! Use the edition number to determine the length of the new Section 3.

  len3 = 7+(ncd*2)
  iben = iupbs01(msgin,'BEN')
  if(iben.lt.4) then
    len3 = len3+1
  endif
  lbyto = lbyto + len3 - 7
  if(lbyto.gt.mxbyto) call bort(bort_arrayoverflow)

  ! Store the descriptors into the new Section 3.

  ibit = (iad3+7)*8
  do n=1,ncd
    call pkb(ids3(n),16,msgot,ibit)
  enddo

  ! Depending on the edition number, pad out the new Section 3 with an additional zeroed-out byte to ensure an even byte count.

  if(iben.lt.4) then
    call pkb(0,8,msgot,ibit)
  endif

  ! Store the length of the new Section 3.

  ibit = iad3*8
  call pkb(len3,24,msgot,ibit)

  ! Now the tricky part - new Section 4.

  if(iupbs3(msgin,'ICMP').eq.1) then

!   The data in Section 4 is compressed and is therefore already standardized, so copy it "as is" into the new Section 4.

    if((lbyto+len4+4).gt.mxbyto) call bort(bort_arrayoverflow)

    call mvb(msgin,iad4+1,msgot,lbyto+1,len4)
    jbit = (lbyto+len4)*8

  else

    nad4 = iad3+len3

    ibit = (iad4+4)*8
    jbit = (nad4+4)*8

    lbyto = lbyto + 4

    ! Copy the subsets, minus the byte counters and bit pads, into the new Section 4.

    nsub = iupbs3(msgin,'NSUB')

    subset_copy: do i=1,nsub
      call upb(lsub,16,msgin,ibit)
      if(nsub.gt.1) then
        ! Use the byte counter to copy this subset.
        islen = lsub-2
      else
        ! This is the only subset in the message, and it could possibly be an overlarge (> 65530 bytes) subset, in
        ! which case we can't rely on the value stored in the byte counter.  either way, we don't really need it.
        islen = iad4+len4-(ibit/8)
        if (mod(len4,2).eq.0) islen = islen - 1
      endif
      do l=1,islen
        call upb(nval,8,msgin,ibit)
        lbyto = lbyto + 1
        if(lbyto.gt.mxbyto) call bort(bort_arrayoverflow)
        call pkb(nval,8,msgot,jbit)
      enddo
      do k=1,8
        kbit = ibit-k-8
        call upb(kval,8,msgin,kbit)
        if(kval.eq.k) then
          jbit = jbit-k-8
          cycle subset_copy
        endif
      enddo
      call bort('BUFRLIB: STNDRD - BIT MISMATCH COPYING SECTION 4 FROM INPUT TO OUTPUT (STANDARD) MESSAGE')
    enddo subset_copy

    ! From this point on, we will need (at most) 6 more bytes of space within msgot in order to be able to store the entire
    ! standardized message (i.e. we will need (at most) 2 more zeroed-out bytes in Section 4, plus the 4 bytes '7777' in
    ! Section 5), so do a final msgot overflow check now.

    if(lbyto+6.gt.mxbyto) call bort(bort_arrayoverflow)

    ! Pad the new Section 4 with zeroes up to the next whole byte boundary.

    do while(.not.(mod(jbit,8).eq.0))
     call pkb(0,1,msgot,jbit)
    enddo

    ! Depending on the edition number, we may need to further pad the new Section 4 with an additional zeroed-out byte in
    ! order to ensure that the padding is up to an even byte boundary.

    if( (iben.lt.4) .and. (mod(jbit/8,2).ne.0) ) then
      call pkb(0,8,msgot,jbit)
    endif

    ibit = nad4*8
    len4 = jbit/8 - nad4
    call pkb(len4,24,msgot,ibit)
    call pkb(0,8,msgot,ibit)
  endif

  ! Finish the new message with an updated section 0 byte count.

  ibit = 32
  lenn = len0+len1+len2+len3+len4+len5
  call pkb(lenn,24,msgot,ibit)

  call pkc('7777',4,msgot,jbit)

  return
end subroutine stndrd

!> Given the WMO bit-wise representation of an FXY value for a descriptor, check whether the descriptor is WMO-standard.
!>
!> If a descriptor is not WMO-standard, then by definition it is a local descriptor.
!>
!> @param[in] idn - integer: WMO bit-wise representation of FXY value for descriptor
!> @returns istdesc - integer: Flag indicating whether idn is a WMO-standard descriptor:
!> - 0 = No
!> - 1 = Yes
!>
!> @author J. Ator @date 2004-08-18
integer function istdesc( idn ) result( iret )

  implicit none

  integer, intent(in) :: idn
  integer if, ix, iy, iokoper

  character*6 adsc, adn30

  adsc = adn30( idn, 6 )

  read(adsc,'(I1,I2,I3)') if,ix,iy
  if ( if .eq. 1 ) then
    ! adsc is a replication descriptor and therefore standard by default
    iret = 1
  else if ( if .eq. 2 ) then
    ! adsc is an operator descriptor
    iret = iokoper( adsc )
  else if ( ( ix .lt. 48 ) .and. ( iy .lt. 192 ) ) then
    iret = 1
  else
    iret = 0
  end if

  return
end function istdesc
