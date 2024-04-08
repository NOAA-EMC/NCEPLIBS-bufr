!> @file
!> @brief Read or write a BUFR data subset.
!>
!> @author J. Woollen @date 1994-01-06

!> Read the next data subset from a BUFR message.
!>
!> Logical unit lunit should have already been opened for
!> input operations via a previous call to subroutine openbf(), and a
!> BUFR message should have already been read into internal arrays via
!> a previous call to one of the
!> [message-reading subroutines](@ref hierarchy).
!>
!> Whenever this subroutine returns with iret = 0, this indicates that a
!> new BUFR data subset (i.e. report) was successfully read into internal
!> arrays within the NCEPLIBS-bufr software, and from where it can be
!> manipulated or further parsed via calls to any of the [values-reading
!> subroutines](@ref hierarchy).
!>
!> If the subroutine returns with iret = -1 there are no more data
!> subsets available within the current message; a new call needs to
!> be made to one of the [message-reading subroutines](@ref hierarchy)
!> in order to read in the next message from logical unit lunit.
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!> @param[out] iret - integer: return code:
!> - 0 new BUFR data subset was successfully read into internal arrays
!> - -1 there are no more BUFR data subsets in the BUFR message
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine readsb(lunit,iret)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_unptyp
  use moda_bitbuf
  use moda_bitmaps
  use moda_stcode

  implicit none

  integer, intent(in) :: lunit
  integer, intent(out) :: iret
  integer my_lunit, lun, il, im, ier, nbyt

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call readsb(my_lunit,iret)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  iret = -1

  ! Check the file status

  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: READSB - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il.gt.0) call bort('BUFRLIB: READSB - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(im.eq.0) return

  ! See if there is another subset in the message

  if(nsub(lun).eq.msub(lun)) return
  nsub(lun) = nsub(lun) + 1

  ! Read the next subset and reset the pointers

  nbtm = 0
  lstnod = 0
  lstnodct = 0
  iscodes(lun) = 0
  linbtm = .false.

  if(msgunp(lun).eq.0) then
    ibit = mbyt(lun)*8
    call upb(nbyt,16,mbay(1,lun),ibit)
    call rdtree(lun,ier)
    if(ier.ne.0) return
    mbyt(lun) = mbyt(lun) + nbyt
  elseif(msgunp(lun).eq.1) then
    ! message with "standard" Section 3
    ibit = mbyt(lun)
    call rdtree(lun,ier)
    if(ier.ne.0) return
    mbyt(lun) = ibit
  else
    ! compressed message
    call rdcmps(lun)
    if (iscodes(lun) .ne. 0) return
  endif

  iret = 0

  return
end subroutine readsb

!> Call subroutine readsb() and pass back its return code as the function value.
!>
!> The use of this function allows the return code from readsb() to be
!> used as the target variable within an iterative program loop.
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!> @returns ireadsb - integer:
!> - 0 = new BUFR data subset was successfully read into internal arrays
!> - -1 = there are no more BUFR data subsets in the BUFR message
!>
!> @author J. Woollen @date 1994-01-06
recursive integer function ireadsb(lunit) result(iret)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: lunit
  integer my_lunit

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    iret=ireadsb(my_lunit)

    im8b=.true.
    return
  endif

  call readsb(lunit,iret)

  return
end function ireadsb

!> Read the next data subset from a BUFR file.
!>
!> This subroutine provides a handy way to combine the functionality of subroutines readmg() and readsb() within a single
!> subroutine call.
!>
!> Logical unit lunit should have already been opened for
!> input operations via a previous call to subroutine openbf().
!> But once that is done, the application program can immediately call
!> this subroutine to read each new data subset from the
!> associated BUFR file, and the subroutine will automatically open
!> and close each new BUFR message internally as needed, so that
!> subsequent calls can immediately be made to any of the various
!> [values-reading subroutines](@ref hierarchy).
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!> @param[out] subset - character*8: Table A mnemonic for type of data subset that was read
!>  (see [DX BUFR Tables](@ref dfbftab) for further information about Table A mnemonics)
!> @param[out] jdate - integer: Date-time stored within Section 1 of
!>  BUFR message containing data subset that was read, in format of either YYMMDDHH or YYYYMMDDHH, depending on the most
!>  recent call to subroutine datelen()
!> @param[out] iret - integer: return code
!>  - 0 = new BUFR data subset was successfully read into internal arrays
!>  - -1 = there are no more BUFR data subsets in the file connected to logical unit lunit
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine readns(lunit,subset,jdate,iret)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: lunit
  integer, intent(out) :: jdate, iret
  integer my_lunit, lun, il, im

  character*8, intent(out) :: subset

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call readns(my_lunit,subset,jdate,iret)
    call x48(jdate,jdate,1)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  ! Refresh the subset and jdate parameters

  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: READNS - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR INPUT')
  if(il.gt.0) call bort('BUFRLIB: READNS - INPUT BUFR FILE IS OPEN FOR OUTPUT, IT MUST BE OPEN FOR INPUT')
  if(inode(lun).eq.0) then
    subset = '        '
  else
    subset = tag(inode(lun))(1:8)
  endif
  jdate = idate(lun)

  ! Read the next subset in the BUFR file

  do while (.true.)
    call readsb(lunit,iret)
    if (iret.eq.0) exit
    call readmg(lunit,subset,jdate,iret)
    if (iret.ne.0) exit
  enddo

  return
end subroutine readns

!> Call subroutine readns() and pass back its return code as the function value.
!>
!> The use of this function allows the return code from readns() to be
!> used as the target variable within an iterative program loop.
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!> @param[out] subset - character*8: Table A mnemonic for type of
!> data subset that was read (see [DX BUFR Tables](@ref dfbftab) for
!> further information about Table A mnemonics)
!> @param[out] idate - integer: Date-time stored within Section 1 of
!> BUFR message containing data subset that was read, in format of
!> either YYMMDDHH or YYYYMMDDHH, depending on the most
!> recent call to subroutine datelen()
!> @returns ireadns - integer:
!> - 0 = new BUFR data subset was successfully read into internal arrays
!> - -1 = there are no more BUFR data subsets in the file connected to logical unit lunit
!>
!> @author J. Woollen @date 1994-01-06
recursive integer function ireadns(lunit,subset,idate) result(iret)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: lunit
  integer, intent(out) :: idate
  integer my_lunit

  character*8, intent(out) :: subset

  ! Check for I8 integers.

  if(im8b) then
     im8b=.false.

     call x84(lunit,my_lunit,1)
     iret=ireadns(my_lunit,subset,idate)
     call x48(idate,idate,1)

     im8b=.true.
     return
  endif

  call readns(lunit,subset,idate,iret)

  return
end function ireadns

!> Write a complete data subset into a BUFR message, for eventual output to logical unit lunit.
!>
!> This subroutine is called when all necessary values for a data subset
!> (i.e. report) have been written, and the subset is ready to be encoded
!> and packed into the current message for the BUFR file associated with
!> logical unit lunit. Logical unit lunit should have already been
!> opened for output operations via a previous call to subroutine
!> openbf(), and a BUFR message should already be open for output within
!> internal arrays via a previous call to one of the NCEPLIBS-bufr
!> [message-writing subroutines](@ref hierarchy). All of the values for
!> the data subset should have already been written into internal arrays
!> via calls to any of the NCEPLIBS-bufr
!> [values-writing subroutines](@ref hierarchy).
!>
!> There is a maximum size for any BUFR message that can be written
!> by the NCEPLIBS-bufr software. This maximum message size is initially set
!> to an internal default value within subroutine bfrini(), but it can
!> be changed to a different value via a separate prior call to
!> subroutine maxout().
!>
!> This subroutine will always check to ensure that the data subset,
!> when encoded and packed, will fit into the current BUFR message that
!> is already open within the internal arrays associated with logical
!> unit lunit. If adding the data subset to the current message would
!> cause the maximum message size to be exceeded, then the subroutine will
!> automatically flush the current message to logical unit lunit, then
!> open and initialize a new internal message using the same subset and
!> jdate values that were specified in the most recent call to one of
!> the [message-writing subroutines](@ref hierarchy) for lunit, then
!> encode and pack the data subset into that new message.
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine writsb(lunit)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: lunit
  integer my_lunit, lun, il, im

  character*1 ccmf

  common /msgcmp/ ccmf

! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84 ( lunit, my_lunit, 1 )
    call writsb ( my_lunit )

    im8b=.true.
    return
  endif

! Check the file status

  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: WRITSB - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il.lt.0) call bort('BUFRLIB: WRITSB - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
  if(im.eq.0) call bort('BUFRLIB: WRITSB - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')

! Pack up the subset and put it into the message

  call wrtree(lun)
  if( ccmf.eq.'Y' ) then
    call wrcmps(lunit)
  else
    call msgupd(lunit,lun)
  endif

  return
end subroutine writsb

!> Write a complete data subset into a BUFR message, and return each completed message within a memory array.
!>
!> This subroutine is similar to subroutine writsb(), except that in
!> addition to writing each completed message to a specified Fortran
!> logical unit, it also returns a copy of each completed message to
!> the application program within a memory array.
!>
!> This subroutine is called to indicate that
!> all necessary values for a data subset (i.e. report) have been written,
!> and the subset is ready to be encoded and packed into the
!> current message for the BUFR file associated with logical unit
!> abs(lunxx). Logical unit abs(lunxx) should have already been opened
!> for output operations via a previous call to subroutine openbf(),
!> and a BUFR message should already be open for output within internal
!> arrays via a previous call to one of the NCEPLIBS-bufr
!> [message-writing subroutines](@ref hierarchy).
!> All of the values for the data subset should have
!> already been written into internal arrays via calls to any of the
!> NCEPLIBS-bufr [values-writing subroutines](@ref hierarchy)
!>
!> This subroutine returns a copy of each completed
!> BUFR message to the application program within a memory array.
!> When using this subroutine, it is important to note that the NCEPLIBS-bufr
!> software is designed to pack as many data subsets as possible into
!> each message for output, and therefore not every call to this
!> subroutine will result in a message being returned in msgt. In
!> such cases, msgl will contain the value 0, indicating that no
!> message was returned.
!>
!> Only when msgl contains a value
!> greater than 0 is there an actual BUFR message within msgt; otherwise,
!> the message into which the data subset was packed remains internally
!> within NCEPLIBS-bufr so that future data subsets can be packed into it as
!> well, and the message will eventually be returned during some other
!> future call to this subroutine. For this reason, there is a way to
!> force the subroutine to return any message contained within the
!> internal NCEPLIBS-bufr arrays, such as when there are no more data subsets
!> to be encoded and we're ready to exit the application program. In
!> this case, the application program should make one final call to
!> this subroutine, but with lunxx set to a negative value;
!> specifically, to the additive inverse of the Fortran logical unit
!> number of the BUFR file. This signals to the subroutine that there
!> are no more data subsets to be packed into the current message for
!> logical unit abs(lunxx), and that the existing message should
!> instead be immediately flushed to output and returned in msgt.
!>
!> @remarks
!> - There is a maximum size for any BUFR message that can be written
!> by the NCEPLIBS-bufr software. This maximum message size is initially set
!> to an internal default value within subroutine bfrini(), but it can
!> be changed to a different value via a separate prior call to
!> subroutine maxout().
!> - As is the case for subroutine writsb(), this subroutine will also
!> check to ensure that the data subset, when encoded and packed, will
!> fit into the current BUFR message that is open within the internal
!> arrays associated with logical unit abs(lunxx).  If adding the data
!> subset to the current message would cause the maximum message size
!> to be exceeded, then the subroutine will automatically flush the
!> current message to logical unit abs(lunxx) and to array msgt, then
!> open and initialize a new internal message using the same subset and
!> jdate values that were specified in the most recent call to one of
!> the [message-writing subroutines](@ref hierarchy) for abs(lunxx),
!> then encode and pack the data subset into that new message.
!> - If the user would prefer that output messages only be returned
!> to the calling program via the msgt memory array and not also
!> written to Fortran logical unit abs(lunxx), then this can be
!> accomplished by setting io = 'NUL' when calling subroutine openbf()
!> for abs(lunxx). In such cases, the logical unit number abs(lunxx)
!> does not even need to be associated with an actual file on the
!> local system.
!>
!> @param[in] lunxx - integer: Absolute value is Fortran logical unit number for BUFR file
!> @param[in] lmsgt - integer: Dimensioned size (in integers) of msgt;
!> used by the subroutine to ensure that it doesn't overflow the msgt array
!> @param[out] msgt - integer: BUFR message
!> @param[out] msgl - integer: Size (in integers) of BUFR message in msgt (0 for no message)
!>
!> @author J. Woollen @author J. Ator @date 1994-01-06
recursive subroutine writsa(lunxx,lmsgt,msgt,msgl)

  use modv_vars, only: im8b

  use moda_bufrmg

  implicit none

  integer, intent(in) :: lunxx, lmsgt
  integer, intent(out) :: msgt(*), msgl
  integer my_lunxx, my_lmsgt, lunit, lun, il, im, n

  character*1 ccmf

  common /msgcmp/ ccmf

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84 ( lunxx, my_lunxx, 1 )
    call x84 ( lmsgt, my_lmsgt, 1 )
    call writsa ( my_lunxx, my_lmsgt*2, msgt, msgl )
    msgl = msgl/2
    call x48 ( msgl, msgl, 1 )

    im8b=.true.
    return
  endif

  lunit = abs(lunxx)

  ! Check the file status

  call status(lunit,lun,il,im)
  if(il.eq.0) call bort('BUFRLIB: WRITSA - OUTPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il.lt.0) call bort('BUFRLIB: WRITSA - OUTPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')
  if(im.eq.0) call bort('BUFRLIB: WRITSA - A MESSAGE MUST BE OPEN IN OUTPUT BUFR FILE, NONE ARE')

  ! If lunxx < 0, force memory msg to be written (w/o any current subset)

  if(lunxx.lt.0) call closmg(lunit)

  ! Is there a completed BUFR message to be returned?

  if(msglen(lun).gt.0) then
    if(msglen(lun).gt.lmsgt) call bort('BUFRLIB: WRITSA - OVERFLOW OF OUTPUT BUFR MESSAGE ARRAY; TRY A LARGER '// &
      'DIMENSION FOR THIS ARRAY')
    msgl = msglen(lun)
    do n=1,msgl
      msgt(n) = msgtxt(n,lun)
    enddo
    msglen(lun) = 0
  else
    msgl = 0
  endif

  if(lunxx.lt.0) return

  ! Pack up the subset and put it into the message

  call wrtree(lun)
  if( ccmf.eq.'Y' ) then
    call wrcmps(lunit)
  else
    call msgupd(lunit,lun)
  endif

  ! If the just-completed call to wrcmps() or msgupd() for this subset caused a message to be flushed to abs(lunxx), then
  ! attempt to retrieve and return that message now.  Otherwise, we run the risk that the next call to openmb() or openmg()
  ! might cause another message to be flushed, and thus overwrite the current message within array msgtxt before we
  ! had the chance to retrieve it during the next call to writsa().

  ! Also note that, in rare instances (e.g. if the byte count of the most recent subset is > 65530), we could end up with
  ! two BUFR messages available to be returned from this one call to writsa().  If sufficient space is available in the
  ! msgt array, then go ahead and return both messages now.

  if( (msglen(lun).gt.0) .and. (msgl+msglen(lun).le.lmsgt) ) then
    do n = 1,msglen(lun)
      msgt(msgl+n) = msgtxt(n,lun)
    enddo
    msgl = msgl+msglen(lun)
    msglen(lun) = 0
  endif

  return
end subroutine writsa

!> Read a specified data subset from a BUFR file.
!>
!> This subroutine combines the functionality of openbf(), readmg(), and readsb().
!>
!> Logical unit lunit should not have already been opened via a
!> previous call to subroutine openbf(). This subroutine will
!> open the file for input.
!>
!> Whenever this subroutine returns successfully, this indicates
!> that a new data subset was successfully read into internal arrays
!> within the NCEPLIBS-bufr software, and that subsequent calls can
!> immediately be made to any of the various
!> [values-reading subroutines](@ref hierarchy).
!>
!> Note that the value specified for imsg should not include any
!> DX BUFR table messages which may be present in the file.
!> In other words, a value of 12 for imsg means to read the 12th
!> message which contains actual report data.
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file
!> @param[in] imsg - integer: Number of BUFR message to be read from the
!> BUFR file, counting from the beginning of the file, but <b>not</b>
!> counting any DX BUFR table messages which may be present in the file
!> @param[in] isub - integer: Number of data subset to be read from the
!> (imsg)th BUFR message, counting from the beginning of the message
!>
!> @author J. Woollen @date 2003-11-04
recursive subroutine rdmgsb(lunit,imsg,isub)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_bitbuf

  implicit none

  integer, intent(in) :: lunit, imsg, isub
  integer my_lunit, my_imsg, my_isub, lun, il, im, i, jdate, iret

  character*128 bort_str
  character*8 subset

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call x84(imsg,my_imsg,1)
    call x84(isub,my_isub,1)
    call rdmgsb(my_lunit,my_imsg,my_isub)

    im8b=.true.
    return
  endif

  ! Open the file and skip to message #imsg

  call openbf(lunit,'IN',lunit)
  call status(lunit,lun,il,im)

  ! Note that we need to use subroutine readmg() to actually read in all of the messages (including the
  ! first (imsg-1) messages!), just in case there are any embedded dictionary messages in the file.

  do i=1,imsg
    call readmg(lunit,subset,jdate,iret)
    if(iret.lt.0) then
      write(bort_str,'("BUFRLIB: RDMGSB - HIT END OF FILE BEFORE READING REQUESTED MESSAGE NO.",I5," IN '//&
        'BUFR FILE CONNECTED TO UNIT",I4)') imsg,lunit
      call bort(bort_str)
    endif
  enddo

  ! Position at subset #isub

  do i=1,isub
    call readsb(lunit,iret)
    if(iret.lt.0) then
      write(bort_str,'("BUFRLIB: RDMGSB - ALL SUBSETS READ BEFORE READING REQ. SUBSET NO.",I3," IN '// &
        'REQ. MSG NO.",I5," IN BUFR FILE CONNECTED TO UNIT",I4)') isub,imsg,lunit
      call bort(bort_str)
    endif
  enddo

  return
end subroutine rdmgsb
