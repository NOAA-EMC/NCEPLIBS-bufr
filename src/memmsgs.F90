!> @file
!> @brief Read and process BUFR messages within internal memory arrays.
!>
!> @author J. Woollen @date 1994-01-06

!> Connect a new file to the NCEPLIBS-bufr software
!> for input operations, then read the entire file contents into
!> internal arrays so that any of the individual BUFR messages can
!> later be accessed from memory, instead of having to read them one
!> at a time sequentially from the file.
!>
!> Any embedded DX BUFR tables contained within the file are also
!> read and processed into separate internal arrays for later use.
!>
!> Logical unit number lunit must already be associated with an
!> actual filename on the local system, typically via a Fortran "OPEN"
!> statement.
!>
!> When inew = 0, the output value iunit will be set equal to the
!> input value lunit.  Otherwise, the output value iunit will be set to
!> the value of lunit that was input when this subroutine was previously
!> called with inew = 0, and the system file connected to lunit will be
!> closed via an internal call to subroutine closbf() before exiting
!> this subroutine.  In either case, iunit can now be used to access
!> all BUFR messages that were read and stored by all previous calls
!> to this subroutine.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param inew - Processing option:
!>   - 0 = Initialize the internal arrays, then read all BUFR messages from lunit into internal arrays
!>   - Otherwise, read all BUFR messages from lunit and append them to the existing messages within the internal arrays
!> @param iret - Number of BUFR messages that were read from lunit and stored into internal arrays
!> @param iunit - File status:
!>   - 0 = lunit was empty, so no messages were read
!>   - Otherwise, the Fortran logical unit number to use for later access to any of the messages from the internal arrays
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbmem(lunit,inew,iret,iunit)

  use bufrlib

  use modv_vars, only: im8b, maxmem, maxmsg

  use moda_mgwa
  use moda_msgmem

  implicit none

  integer, intent(in) :: lunit, inew
  integer, intent(out) :: iret, iunit
  integer iprt, my_lunit, my_inew, iflg, itim, lun, il, im, itemp, ier, nmsg, lmem, i, mlast0, idxmsg, nmwrd

  character*128 bort_str, errstr

  common /quiet/ iprt

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call x84(inew,my_inew,1)
    call ufbmem(my_lunit,my_inew,iret,iunit)
    call x48(iret,iret,1)
    call x48(iunit,iunit,1)

    im8b=.true.
    return
  endif

  ! Try to open BUFR file and set to initialize or concatenate

  call openbf(lunit,'IN',lunit)

  if(inew.eq.0) then
    msgp(0) = 0
    munit = 0
    mlast = 0
    ndxts = 0
    ldxts = 0
    ndxm = 0
    ldxm = 0
  endif

  nmsg = msgp(0)
  iret = 0
  iflg = 0
  itim = 0

  ! Copy any BUFR dictionary table messages from the beginning of lunit into @ref moda_msgmem for possible later use.
  ! Note that such a table (if one exists) is already now in scope due to the prior call to subroutine openbf(), which
  ! in turn would have automatically called subroutines readdx(), rdbfdx() and makestab() for this table.

  itemp = ndxts
  call status(lunit,lun,il,im)
  call cewind_c(lun)
  call cpdxmm(lunit)

  ! If a table was indeed present at the beginning of the file, then set the flag to indicate that this table is now in scope.

  if ((itemp+1).eq.ndxts) ldxts = ndxts

  ! Transfer messages from file to memory and set message pointers

  do while (.true.)
    call rdmsgw(lunit,mgwa,ier)
    if(ier.eq.-1) exit
    if(ier.eq.-2) then
      write(bort_str,'("BUFRLIB: UFBMEM - ERROR READING MESSAGE NUMBER",I5," INTO MEMORY FROM UNIT",I3)') nmsg+1,lunit
      call bort(bort_str)
    endif

    if(idxmsg(mgwa).eq.1) then
      ! New "embedded" BUFR dictionary table messages have been found in this file.  Copy them into @ref moda_msgmem
      ! for later use.
      call backbufr_c(lun)  ! Backspace lunit
      call cpdxmm(lunit)
      cycle
    endif

    nmsg = nmsg+1
    if(nmsg.gt.maxmsg) iflg = 1
    lmem = nmwrd(mgwa)
    if(lmem+mlast.gt.maxmem) iflg = 2

    if(iflg.eq.0) then
      iret = iret+1
      do i=1,lmem
        msgs(mlast+i) = mgwa(i)
      enddo
      msgp(0) = nmsg
      msgp(nmsg) = mlast+1
    else
      if(itim.eq.0) then
        mlast0 = mlast
        itim=1
      endif
    endif
    mlast = mlast+lmem
  enddo

  if(iflg.eq.1) then
    ! Emergency room treatment for maxmsg array overflow
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      write ( unit=errstr, fmt='(A,A,I8,A)' ) 'BUFRLIB: UFBMEM - THE NO. OF MESSAGES REQUIRED TO STORE ', &
        'ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', maxmsg, ') - INCOMPLETE READ'
      call errwrt(errstr)
      write ( unit=errstr, fmt='(A,I8,A,I8,A)' ) '>>>UFBMEM STORED ', msgp(0), ' MESSAGES OUT OF ', nmsg, '<<<'
      call errwrt(errstr)
      write ( unit=errstr, fmt='(A,I8,A,I8,A)' ) '>>>UFBMEM STORED ', mlast0, ' BYTES OUT OF ', mlast, '<<<'
      call errwrt(errstr)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    mlast=mlast0
  endif

  if(iflg.eq.2) then
    ! Emergency room treatment for maxmem array overflow
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      write ( unit=errstr, fmt='(A,A,I8,A)' ) 'BUFRLIB: UFBMEM - THE NO. OF BYTES REQUIRED TO STORE ', &
        'ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', maxmem, ') - INCOMPLETE READ'
      call errwrt(errstr)
      write ( unit=errstr, fmt='(A,I8,A,I8,A)' ) '>>>UFBMEM STORED ', mlast0, ' BYTES OUT OF ', mlast, '<<<'
      call errwrt(errstr)
      write ( unit=errstr, fmt='(A,I8,A,I8,A)' ) '>>>UFBMEM STORED ', msgp(0), ' MESSAGES OUT OF ', nmsg, '<<<'
      call errwrt(errstr)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    mlast=mlast0
  endif

  if(iret.eq.0) then
    call closbf(lunit)
  else
    if(munit.ne.0) call closbf(lunit)
    if(munit.eq.0) munit = lunit
  endif
  iunit = munit

  return
end subroutine ufbmem

!> Connect a new file to the NCEPLIBS-bufr software
!> for input operations, then read the entire file contents into
!> internal arrays so that any of the individual BUFR messages can
!> later be accessed from memory, instead of having to read them one
!> at a time sequentially from the file.
!>
!> This subroutine is similar to subroutine ufbmem(), except that
!> instead of a file status it returns an array of message types that
!> were read in.  Furthermore, this subroutine doesn't process any
!> embedded DX BUFR tables contained within the file; instead,
!> it provides an additional call argument lundx to allow
!> for specification of the necessary DX BUFR table information
!> associated with the messages in the file.
!>
!> Logical unit numbers lunit and lundx must already be associated
!> with actual filenames on the local system, typically via a Fortran
!> "OPEN" statement.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param lundx - Fortran logical unit number containing DX BUFR table information associated with
!> BUFR messages in lunit
!> @param inew - Processing option:
!>  - 0 = Initialize the internal arrays, then read all BUFR messages from lunit into internal arrays
!>  - Otherwise, read all BUFR messages from lunit and append them to the existing messages within the internal arrays
!> @param iret - Number of BUFR messages that were read from lunit and stored into internal arrays
!> @param mesg - Types of BUFR messages that were read from lunit and stored into internal arrays
!>
!> @author J. Woollen @date 2012-01-26
recursive subroutine ufbmex(lunit,lundx,inew,iret,mesg)

  use modv_vars, only: im8b, maxmem, maxmsg

  use moda_mgwa
  use moda_msgmem

  implicit none

  character*128 bort_str, errstr

  integer, intent(in) :: lunit(*), lundx(*), inew(*)
  integer, intent(out) :: mesg(*), iret(*)
  integer iprt, my_lunit(1), my_lundx(1), my_inew(1), nmesg, iflg, itim, ier, nmsg, lmem, i, mlast0, iupbs01, nmwrd

  common /quiet/ iprt

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit(1),my_lunit(1),1)
    call x84(lundx(1),my_lundx(1),1)
    call x84(inew(1),my_inew(1),1)
    if (my_inew(1).eq.0) then
      nmesg = 0
    else
      nmesg = msgp(0)
      call x84(mesg(1),mesg(1),nmesg)
    endif
    call ufbmex(my_lunit(1),my_lundx(1),my_inew(1),iret(1),mesg(1))
    call x48(mesg(1),mesg(1),nmesg+iret(1))
    call x48(iret(1),iret(1),1)

    im8b=.true.
    return
  endif

  ! Try to open BUFR file and set to initialize or concatenate

  call openbf(lunit(1),'IN',lundx(1))

  if(inew(1).eq.0) then
    msgp(0) = 0
    munit = 0
    mlast = 0
    ndxts = 0
    ldxts = 0
    ndxm = 0
    ldxm = 0
  endif

  nmsg = msgp(0)
  iret(1) = 0
  iflg = 0
  itim = 0

  ! Set some flags so that subsequent calls to the message reading routines will know there is a BUFR table in scope.

  ndxts = 1
  ldxts = 1
  ipmsgs(1) = 1

  ! Transfer messages from file to memory and set message pointers.

  do while (.true.)
    call rdmsgw(lunit(1),mgwa,ier)
    if(ier.eq.-1) exit
    if(ier.eq.-2) then
      write(bort_str,'("BUFRLIB: UFBMEX - ERROR READING MESSAGE NUMBER",I5," INTO MEMORY FROM UNIT",I3)') nmsg+1,lunit(1)
      call bort(bort_str)
    endif

    nmsg = nmsg+1
    mesg(nmsg) = iupbs01(mgwa,'MTYP')
    if(nmsg.gt.maxmsg) iflg = 1
    lmem = nmwrd(mgwa)
    if(lmem+mlast.gt.maxmem) iflg = 2

    if(iflg.eq.0) then
      iret(1) = iret(1)+1
      do i=1,lmem
        msgs(mlast+i) = mgwa(i)
      enddo
      msgp(0) = nmsg
      msgp(nmsg) = mlast+1
    else
      if(itim.eq.0) then
        mlast0 = mlast
        itim=1
      endif
    endif
    mlast = mlast+lmem
  enddo

  if(iflg.eq.1) then
    ! Emergency room treatment for maxmsg array overflow
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      write ( unit=errstr, fmt='(A,A,I8,A)' ) 'BUFRLIB: UFBMEX - THE NO. OF MESSAGES REQUIRED TO STORE ', &
        'ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', maxmsg, ') - INCOMPLETE READ'
      call errwrt(errstr)
      write ( unit=errstr, fmt='(A,I8,A,I8,A)' ) '>>>UFBMEX STORED ', msgp(0), ' MESSAGES OUT OF ', nmsg, '<<<'
      call errwrt(errstr)
      write ( unit=errstr, fmt='(A,I8,A,I8,A)' ) '>>>UFBMEX STORED ', mlast0, ' BYTES OUT OF ', mlast, '<<<'
      call errwrt(errstr)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    mlast=mlast0
  endif

  if(iflg.eq.2) then
    ! Emergency room treatment for maxmem array overflow
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      write ( unit=errstr, fmt='(A,A,I8,A)' ) 'BUFRLIB: UFBMEX - THE NO. OF BYTES REQUIRED TO STORE ', &
        'ALL MESSAGES INTERNALLY EXCEEDS MAXIMUM (', maxmem, ') - INCOMPLETE READ'
      call errwrt(errstr)
      write ( unit=errstr, fmt='(A,I8,A,I8,A)' ) '>>>UFBMEX STORED ', mlast0, ' BYTES OUT OF ', mlast, '<<<'
      call errwrt(errstr)
      write ( unit=errstr, fmt='(A,I8,A,I8,A)' ) '>>>UFBMEX STORED ', msgp(0), ' MESSAGES OUT OF ', nmsg, '<<<'
      call errwrt(errstr)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    mlast=mlast0
  endif

  if(iret(1).eq.0) then
    call closbf(lunit(1))
  else
    if(munit.ne.0) call closbf(lunit(1))
    if(munit.eq.0) munit = lunit(1)
  endif

  return
end subroutine ufbmex

!> Read a specified BUFR message from internal arrays in memory, so that it is now in scope for processing
!> via a subsequent call to subroutine rdmems().
!>
!> BUFR messages should already be stored within internal
!> arrays in memory via one or more previous calls to
!> subroutine ufbmem().
!>
!> This subroutine is similar to subroutine rdmemm(), except that
!> this subroutine increments the value of imsg prior to returning to
!> the calling program, which in turn allows it to be easily called
!> within an iterative program loop.
!>
!> @param imsg - Message pointer within internal arrays:
!>   - On input, imsg is the number of the BUFR message to be read into scope for further
!>     processing, counting from the beginning of the internal arrays in memory
!>   - On output, imsg is incremented by one from its input value
!> @param subset - Table A mnemonic for type of BUFR message that was read into scope
!> (see [DX BUFR Tables](@ref dfbftab) for further information about Table A mnemonics)
!> @param jdate - Date-time stored within Section 1 of BUFR message that was read into scope,
!> in format of either YYMMDDHH or YYYYMMDDHH, depending on the most recent call to subroutine datelen()
!> @param iret - return code:
!>   - 0 = requested message was successfully read into scope
!>   - -1 = requested message number could not be found in internal arrays
!>
!> @author J. Woollen @date 1999-11-18
recursive subroutine readmm(imsg,subset,jdate,iret)

  use modv_vars, only: im8b

  implicit none

  integer, intent(inout) :: imsg
  integer, intent(out) :: jdate, iret

  character*8, intent(out) :: subset

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.

    call x84(imsg,imsg,1)
    call readmm(imsg,subset,jdate,iret)
    call x48(imsg,imsg,1)
    call x48(jdate,jdate,1)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  call rdmemm(imsg,subset,jdate,iret)

  imsg = imsg+1

  return
end subroutine readmm

!> Call subroutine readmm() and pass back its return code as the function value.
!>
!> The use of this function allows the return code from readmm() to be
!> used as the target variable within an iterative program loop.
!>
!> @param imsg - Message pointer within internal arrays:
!> - On input, imsg is the number of the BUFR message to be read into scope for further processing, counting from the
!> beginning of the internal arrays in memory
!>  - On output, imsg is incremented by one from its input value
!> @param subset - Table A mnemonic for type of BUFR message that was read into scope
!> (see [DX BUFR Tables](@ref dfbftab) for further information about Table A mnemonics)
!> @param idate - Date-time stored within Section 1 of BUFR message that was read into scope,
!> in format of either YYMMDDHH or YYYYMMDDHH, depending on the most recent call to subroutine datelen()
!>
!> @returns ireadmm - return code:
!> - 0 new BUFR message was successfully read into scope
!> - -1 requested message number could not be found in internal arrays
!>
!> @author J. Woollen @date 1999-11-18
recursive integer function ireadmm(imsg,subset,idate) result(iret)

  use modv_vars, only: im8b

  implicit none

  integer, intent(inout) :: imsg
  integer, intent(out) :: idate

  character*8, intent(out) :: subset

  ! Check for I8 integers.

  if(im8b) then
     im8b=.false.

     call x84(imsg,imsg,1)
     iret=ireadmm(imsg,subset,idate)
     call x48(imsg,imsg,1)
     call x48(idate,idate,1)

     im8b=.true.
     return
  endif

  call readmm(imsg,subset,idate,iret)

  return
end function ireadmm

!> Read a specified BUFR message from internal arrays in memory, so that it is now in scope for processing
!> via a subsequent call to subroutine rdmems().
!>
!> BUFR messages should already be stored within internal
!> arrays in memory via one or more previous calls to
!> subroutine ufbmem().
!>
!> This subroutine is similar to subroutine readmm(), except that
!> readmm() also increments the value of imsg prior to returning to
!> the calling program, which in turn allows it to be easily called
!> within an iterative program loop.
!>
!> @param imsg - Number of BUFR message to be read into scope for further processing,
!> counting from the beginning of the internal arrays in memory
!> @param subset - Table A mnemonic for type of BUFR message that was read into scope
!> (see [DX BUFR Tables](@ref dfbftab) for further information about Table A mnemonics)
!> @param jdate - Date-time stored within Section 1 of BUFR message that was read into scope,
!> in format of either YYMMDDHH or YYYYMMDDHH, depending on the most recent call to subroutine datelen()
!> @param iret - return code:
!>  - 0 = requested message was successfully read into scope
!>  - -1 = requested message number could not be found in internal arrays
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine rdmemm(imsg,subset,jdate,iret)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_bitbuf
  use moda_mgwa
  use moda_msgmem

  implicit none

  integer, intent(in) :: imsg
  integer, intent(out) :: jdate, iret
  integer iprt, my_imsg, lun, il, im, ii, jj, kk, nwrd, iptr, lptr, ier

  character*128 bort_str, errstr
  character*8, intent(out) :: subset

  logical known

  common /quiet/ iprt

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(imsg,my_imsg,1)
    call rdmemm(my_imsg,subset,jdate,iret)
    call x48(jdate,jdate,1)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  ! Check the message request and file status

  call status(munit,lun,il,im)
  call wtstat(munit,lun,il,1)
  iret = 0

  if(imsg.eq.0 .or.imsg.gt.msgp(0)) then
    call wtstat(munit,lun,il,0)
    if(iprt.ge.1) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      if(imsg.eq.0) then
        errstr = 'BUFRLIB: RDMEMM - REQUESTED MEMORY MESSAGE NUMBER {FIRST (INPUT) ARGUMENT} IS 0, RETURN WITH IRET = -1'
      else
        write ( unit=errstr, fmt='(A,I6,A,I6,A)' ) 'BUFRLIB: RDMEMM - REQ. MEMORY MESSAGE #', imsg, &
          ' {= 1ST (INPUT) ARG.} > # OF MESSAGES IN MEMORY (', msgp(0), '), RETURN WITH IRET = -1'
      endif
      call errwrt(errstr)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    iret = -1
    return
  endif

  ! Determine which table applies to this message.

  known = .false.
  jj = ndxts
  do while ((.not.known).and.(jj.ge.1))
    if (ipmsgs(jj).le.imsg) then
      known = .true.
    else
      jj = jj - 1
    endif
  enddo
  if (.not.known) then
    write(bort_str,'("BUFRLIB: RDMEMM - UNKNOWN DX TABLE FOR REQUESTED MESSAGE #",I5)') imsg
    call bort(bort_str)
  endif

  ! Is this table the one that is currently in scope?

  if (jj.ne.ldxts) then

    ! No, so reset the software to use the proper table.

    if(iprt.ge.2) then
      call errwrt('+++++++++++++++++++++++++++++++++++++++++++++')
      write ( unit=errstr, fmt='(A,I3,A,I3,A,I6)' ) 'BUFRLIB: RDMEMM - RESETTING TO USE DX TABLE #', jj, &
        ' INSTEAD OF DX TABLE #', ldxts, ' FOR REQUESTED MESSAGE #', imsg
      call errwrt(errstr)
      call errwrt('+++++++++++++++++++++++++++++++++++++++++++++')
      call errwrt(' ')
    endif
    call dxinit(lun,0)

    ! Store each of the DX dictionary messages which constitute this table.

    do ii = ifdxts(jj), (ifdxts(jj)+icdxts(jj)-1)
      if (ii.eq.ndxm) then
        nwrd = ldxm - ipdxm(ii) + 1
      else
        nwrd = ipdxm(ii+1) - ipdxm(II)
      endif
      do kk = 1, nwrd
        mgwa(kk) = mdx(ipdxm(ii)+kk-1)
      enddo
      call stbfdx(lun,mgwa)
    enddo

    ! Rebuild the internal jump/link table.

    call makestab
    ldxts = jj
  endif

  ! Read memory message number imsg into a message buffer.

  iptr = msgp(imsg)
  if(imsg.lt.msgp(0)) lptr = msgp(imsg+1)-iptr
  if(imsg.eq.msgp(0)) lptr = mlast-iptr+1
  iptr = iptr-1

  do ii=1,lptr
    mbay(ii,lun) = msgs(iptr+ii)
  enddo

  ! Parse the message section contents.

  call cktaba(lun,subset,jdate,ier)
  nmsg(lun) = imsg

  return
end subroutine rdmemm

!> Read a specified data subset from the BUFR message that was most recently read via a call to subroutine rdmemm()
!> or readmm().
!>
!> Whenever this subroutine returns with iret = 0, this indicates
!> that a new BUFR data subset (i.e. report) was successfully read into
!> internal arrays within the NCEPLIBS-bufr software, and from where it can
!> now be easily manipulated or further parsed via calls to any of the
!> [values-reading subroutines](@ref hierarchy) using the Fortran
!> logical unit number IUNIT that was returned from the most recent
!> call to subroutine ufbmem().
!>
!> @param isub - Number of data subset to be read from BUFR message, counting from the beginning of the message
!> @param iret - return code:
!>  - 0 = requested data subset was successfully read
!>  - -1 = requested subset number could not be found in the message
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine rdmems(isub,iret)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_unptyp
  use moda_bitbuf
  use moda_msgmem

  implicit none

  integer, intent(in) :: isub
  integer, intent(out) :: iret
  integer my_isub, iprt, lun, il, im, mbym, nbyt, i, iupb

  character*128 bort_str, errstr

  common /quiet/ iprt

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(isub,my_isub,1)
    call rdmems(my_isub,iret)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  ! Check the message request and file status

  call status(munit,lun,il,im)
  if(im.eq.0) call bort('BUFRLIB: RDMEMS - A MEMORY MESSAGE MUST BE OPEN IN INPUT BUFR FILE, NONE ARE')
  if(nsub(lun).ne.0) then
    write(bort_str,'("BUFRLIB: RDMEMS - UPON ENTRY, SUBSET POINTER IN MEMORY MESSAGE IS NOT AT BEGINNING (",I3," '// &
      'SUBSETS HAVE BEEN READ, SHOULD BE 0)")') nsub(lun)
    call bort(bort_str)
  endif

  if(isub.gt.msub(lun)) then
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      write ( unit=errstr, fmt='(A,I5,A,A,I5,A)' ) 'BUFRLIB: RDMEMS - REQ. SUBSET #', isub, ' (= 1st INPUT ', &
        'ARG.) > # OF SUBSETS IN MEMORY MESSAGE (', msub(lun), ')'
      call errwrt(errstr)
      call errwrt('RETURN WITH IRET = -1')
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    iret = -1
    return
  endif

  mbym = mbyt(lun)
  nbyt = 0

  ! Position to subset number isub in memory message

  if(msgunp(lun).eq.0) then
    nsub(lun) = isub-1
    do i=1,isub-1
      mbyt(lun) = mbyt(lun) + iupb(mbay(1,lun),mbyt(lun)+1,16)
    enddo
  elseif(msgunp(lun).eq.1) then
    ! message with "standard" Section 3
    do i=1,isub-1
      call readsb(munit,iret)
    enddo
  else
    ! compressed message
    nsub(lun) = isub-1
  endif

  ! Now read subset number isub from memory message

  call readsb(munit,iret)
  if(iret.ne.0) call bort('BUFRLIB: RDMEMS - CALL TO ROUTINE READSB RETURNED WITH IRET = -1 (EITHER MEMORY MESSAGE '// &
     'NOT OPEN OR ALL SUBSETS IN MESSAGE READ')

  ! Reset subset pointer back to zero (beginning of message) and return

  mbyt(lun) = mbym
  nsub(lun) = 0

  return
end subroutine rdmems

!> Read an entire DX BUFR table from a specified file into internal memory arrays.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!>
!> @author J. Ator @date 2009-03-23
subroutine cpdxmm( lunit )

  use bufrlib

  use modv_vars, only: mxdxts

  use moda_mgwa
  use moda_msgmem

  implicit none

  integer, intent(in) :: lunit
  integer iprt, ict, lun, il, im, ier, j, lmem, idxmsg, iupbs3, nmwrd

  character*128 errstr

  logical done

  common /quiet/ iprt

  if ( ndxts .ge. mxdxts ) call bort('BUFRLIB: CPDXMM - MXDXTS OVERFLOW')

  ict = 0
  done = .false.
  call status(lunit,lun,il,im)

  ! Read a complete dictionary table from lunit, as a set of one or more DX dictionary messages.

  do while ( .not. done )
    call rdmsgw ( lunit, mgwa, ier )
    if ( ier .eq. -2 ) call bort('BUFRLIB: CPDXMM - UNEXPECTED READ ERROR')
    if ( ier .eq. -1 ) then

      ! Don't abort for an end-of-file condition, since it may be possible for a file to end with dictionary messages.
      ! Instead, backspace the file pointer and let the calling routine diagnose the end-of-file condition and deal with
      ! it as it sees fit.

      call backbufr_c(lun)
      done = .true.
    else if ( idxmsg(mgwa) .ne. 1 ) then

      ! This is a non-DX dictionary message.  Assume we've reached the end of the dictionary table, and backspace lunit so
      ! that the next read (e.g. in the calling routine) will get this same message.

      call backbufr_c(lun)
      done = .true.
    else if ( iupbs3(mgwa,'nsub') .eq. 0 ) then

      ! This is a DX dictionary message, but it doesn't contain any actual dictionary information.  Assume we've reached the
      ! end of the dictionary table.

      done = .true.
    else

      ! Store this message into @ref moda_msgmem.

      ict = ict + 1
      if ( ( ndxm + ict ) .gt. mxdxm ) call bort('BUFRLIB: CPDXMM - MXDXM OVERFLOW')
      ipdxm(ndxm+ict) = ldxm + 1
      lmem = nmwrd(mgwa)
      if ( ( ldxm + lmem ) .gt. mxdxw ) call bort('BUFRLIB: CPDXMM - MXDXW OVERFLOW')
      do j = 1, lmem
        mdx(ldxm+j) = mgwa(j)
      enddo
      ldxm = ldxm + lmem
    endif
  enddo

  ! Update the table information within @ref moda_msgmem.

  if ( ict .gt. 0 ) then
    ifdxts(ndxts+1) = ndxm + 1
    icdxts(ndxts+1) = ict
    ipmsgs(ndxts+1) = msgp(0) + 1
    ndxm = ndxm + ict
    ndxts = ndxts + 1
    if ( iprt .ge. 2 ) then
      call errwrt('+++++++++++++++++++++++++++++++++++++++++++++')
      write ( unit=errstr, fmt='(A,I3,A,I3,A)') 'BUFRLIB: CPDXMM - STORED NEW DX TABLE #', ndxts, &
        ' CONSISTING OF ', ict, ' MESSAGES'
      call errwrt(errstr)
      call errwrt('+++++++++++++++++++++++++++++++++++++++++++++')
      call errwrt(' ')
    endif
  endif

  return
end subroutine cpdxmm

!> Read a specified data subset from internal arrays.
!>
!> This subroutine provides a handy way to combine the functionality
!> of subroutines rdmemm() and rdmems() within a single subroutine call.
!>
!> Whenever this subroutine returns successfully, the requested data
!> subset can now be easily manipulated or further parsed via calls to
!> any of the [values-reading subroutines](@ref hierarchy) using the
!> Fortran logical unit number iunit that was returned from the most
!> recent call to subroutine ufbmem().
!>
!> @param imsg - Number of BUFR message to be read into scope for further processing, counting from the
!> beginning of the internal arrays in memory
!> @param isub - Number of data subset to be read from the (imsg)th BUFR message, counting from the
!> beginning of the message
!> @param subset - Table A mnemonic for type of (imsg)th BUFR message
!> (see [DX BUFR Tables](@ref dfbftab) for further information about Table A mnemonics)
!> @param jdate - Date-time stored within Section 1 of (imsg)th BUFR message, in format of either
!> YYMMDDHH or YYYYMMDDHH, depending on the most recent call to subroutine datelen()
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbmms(imsg,isub,subset,jdate)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_msgmem

  implicit none

  integer, intent(in) :: imsg, isub
  integer, intent(out) :: jdate
  integer my_imsg, my_isub, lun, il, im, iret

  character*8, intent(out) :: subset

  character*128 bort_str

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(imsg,my_imsg,1)
    call x84(isub,my_isub,1)
    call ufbmms(my_imsg,my_isub,subset,jdate)
    call x48(jdate,jdate,1)

    im8b=.true.
    return
  endif

  ! Read subset #isub from memory message #imsg

  call rdmemm(imsg,subset,jdate,iret)
  if(iret.lt.0) then
    if(imsg.gt.0)  then
      write(bort_str,'("BUFRLIB: UFBMMS - REQUESTED MEMORY MESSAGE NUMBER TO READ IN (",I5,") EXCEEDS THE NUMBER OF '// &
        'MESSAGES IN MEMORY (",I5,")")') imsg,msgp(0)
    else
      write(bort_str,'("BUFRLIB: UFBMMS - REQUESTED MEMORY MESSAGE NUMBER TO READ IN IS ZERO - THIS IS NOT VALID")')
    endif
    call bort(bort_str)
  endif
  call rdmems(isub,iret)
  if(iret.ne.0) then
    call status(munit,lun,il,im)
    write(bort_str,'("BUFRLIB: UFBMMS - REQ. SUBSET NUMBER TO READ IN (",I3,") EXCEEDS THE NUMBER OF SUBSETS (",I3,") '// &
      'IN THE REG. MEMORY MESSAGE (",I5,")")') isub,msub(lun),imsg
    call bort(bort_str)
  endif

  return
end subroutine ufbmms

!> Read a specified data subset from internal
!> arrays in memory, so that it is now in scope for processing
!> via calls to any of the [values-reading subroutines](@ref hierarchy)
!> using the Fortran logical unit number iunit that was returned from
!> the most recent call to subroutine ufbmem().
!>
!> This subroutine does not return any information about which
!> BUFR message within the internal arrays contained the specified data subset.
!>
!> @param irep - Number of data subset to be read into scope for further processing,
!> counting from the beginning of the internal arrays in memory
!> @param subset - Table A mnemonic for type of BUFR message that was read into scope
!> (see [DX BUFR Tables](@ref dfbftab) for further information about Table A mnemonics)
!> @param idate - Date-time stored within Section 1 of BUFR message that was read into scope,
!> in format of either YYMMDDHH or YYYYMMDDHH, depending on the most recent call to subroutine datelen()
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbmns(irep,subset,idate)

  use modv_vars, only: im8b

  use moda_msgmem

  implicit none

  integer, intent(in) :: irep
  integer, intent(out) :: idate
  integer my_irep, imsg, jrep, iret, ireadmm, nmsub

  character*8, intent(out) :: subset

  character*128 bort_str

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(irep,my_irep,1)
    call ufbmns(my_irep,subset,idate)
    call x48(idate,idate,1)

    im8b=.true.
    return
  endif

  jrep = 0
  imsg = 1

  ! Read subset #irep

  do while(ireadmm(imsg,subset,idate).eq.0)
    if(jrep+nmsub(munit).ge.irep) then
       call rdmems(irep-jrep,iret)
       return
    endif
    jrep = jrep+nmsub(munit)
  enddo

  write(bort_str,'("BUFRLIB: UFBMNS - REQ. SUBSET NO. TO READ IN (",I5,") EXCEEDS TOTAL NO. OF SUBSETS IN THE COLLECTION '// &
    'OF MEMORY MESSAGES (",I5,")")') irep,jrep
  call bort(bort_str)
end subroutine ufbmns

!> Read one or more data values from a data subset in internal arrays.
!>
!> This subroutine provides a handy way to combine the functionality
!> of subroutines rdmemm(), rdmems() and ufbint() within a single
!> subroutine call.
!>
!> @param imsg - Number of BUFR message to be read into scope for further processing,
!> counting from the beginning of the internal arrays in memory
!> @param isub - Number of data subset to be read from the (imsg)th BUFR message,
!> counting from the beginning of the message
!> @param usr - Data values
!> @param i1 - First dimension of usr as allocated within the calling program
!> @param i2 - Second dimension of usr as allocated within the calling program
!> @param iret - Number of replications of str that were read from the data subset
!> @param str - String of blank-separated Table B mnemonics in one-to-one correspondence with the number of
!> data values that will be read from the data subset within the first dimension of usr (see [DX BUFR Tables](@ref dfbftab)
!> for further information about Table B mnemonics)
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbrms(imsg,isub,usr,i1,i2,iret,str)

  use modv_vars, only: im8b

  use moda_msgcwd
  use moda_msgmem

  implicit none

  integer, intent(in) :: imsg, isub, i1, i2
  integer, intent(out) :: iret
  integer iprt, my_imsg, my_isub, my_i1, my_i2, jdate, lun, il, im

  real*8, intent(out) :: usr(i1,i2)

  character*(*), intent(in) :: str
  character*128 bort_str, errstr
  character*8 subset

  common /quiet/ iprt

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(imsg,my_imsg,1)
    call x84(isub,my_isub,1)
    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call ufbrms(my_imsg,my_isub,usr,my_i1,my_i2,iret,str)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  iret = 0
  if(i1.le.0) then
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBRMS - 4th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 6th ARG. (IRET) = 0; 7th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  elseif(i2.le.0) then
    if(iprt.ge.0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      errstr = 'BUFRLIB: UFBRMS - 5th ARG. (INPUT) IS .LE. 0, SO RETURN WITH 6th ARG. (IRET) = 0; 7th ARG. (STR) ='
      call errwrt(errstr)
      call errwrt(str)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
    return
  endif

  ! Read requested values from subset #isub within memory message #imsg

  call rdmemm(imsg,subset,jdate,iret)
  if(iret.lt.0) then
    if(imsg.gt.0)  then
      write(bort_str,'("BUFRLIB: UFBRMS - REQUESTED MEMORY MESSAGE NUMBER TO READ IN (",I5,") EXCEEDS THE NUMBER OF '// &
        'MESSAGES IN MEMORY (",I5,")")') imsg,msgp(0)
    else
      write(bort_str,'("BUFRLIB: UFBRMS - REQUESTED MEMORY MESSAGE NUMBER TO READ IN IS ZERO - THIS IS NOT VALID")')
    endif
    call bort(bort_str)
  endif
  call rdmems(isub,iret)
  if(iret.ne.0) then
    call status(munit,lun,il,im)
    write(bort_str,'("BUFRLIB: UFBRMS - REQ. SUBSET NUMBER TO READ IN (",I3,") EXCEEDS THE NUMBER OF SUBSETS (",I3,") '// &
      'IN THE REG. MEMORY MESSAGE (",I5,")")') isub,msub(lun),imsg
    call bort(bort_str)
  endif

  call ufbint(munit,usr,i1,i2,iret,str)

  return
end subroutine ufbrms

!> Read through every data subset in internal arrays and return one or more specified data values from each subset.
!>
!> This provides a useful way to scan the ranges of one or more
!> specified data values across all of the data subsets in the
!> internal arrays.  It is similar to subroutine ufbtab(), except
!> that ufbtab() works on data subsets in a BUFR file.
!>
!> It is the user's responsibility to ensure that TAB is dimensioned
!> sufficiently large enough to accommodate the number of data values
!> that are to be read from the internal arrays.  Specifically, each row of
!> TAB will contain the data values read from a different data subset,
!> so the value I2 must be at least as large as the total number of data
!> subsets in the internal arrays.
!>
!> The internal arrays must have already been populated via a previous
!> call to subroutine ufbmem().
!>
!> There are a few additional special mnemonics that can be
!> included within str when calling this subroutine, and which in turn
!> will result in special information being returned within the
!> corresponding location in tab:
!> - IREC - returns the number of the BUFR message within the
!>   internal arrays (counting from the beginning of the
!>   internal arrays) in which the current data subset resides.
!> - ISUB - returns the number of the current data subset within
!>   the BUFR message pointed to by IREC, counting from
!>   the beginning of the message.
!> - ITBL - returns the number of the DX BUFR table that is
!>   in scope for the current data subset.
!>
!> This subroutine will not work on compressed data subsets.
!>
!> @param tab - Data values
!> @param i1 - First dimension of tab as allocated within the calling program
!> @param i2 - Second dimension of tab as allocated within the calling program
!> @param iret - Number of data subsets in internal arrays
!> @param str - String of blank-separated Table B mnemonics, in one-to-one correspondence with the number
!> of data values that will be read from each data subset within the first dimension of tab
!> (see [DX BUFR Tables](@ref dfbftab) for further information about Table B mnemonics)
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbtam(tab,i1,i2,iret,str)

  use modv_vars, only: im8b, bmiss

  use moda_usrint
  use moda_msgcwd
  use moda_bitbuf
  use moda_msgmem
  use moda_tables

  implicit none

  character*(*), intent(in) :: str
  character*128 bort_str, errstr
  character*10 tgs(100)
  character*8 subset, cval

  integer*8 mps, ival
  integer, intent(in) :: i1, i2
  integer, intent(out) :: iret
  integer iprt, maxtg, nnod, ncon, nods, nodc, ivls, kons, my_i1, my_i2, i, j, irec, isub, itbl, lun, il, im, jdate, mret, &
    kbit, mbit, nbit, n, node, imsg, kmsg, nrep, ntg, nbyt, nbmp, nmsub

  real*8, intent(out) :: tab(i1,i2)
  real*8 rval, ups

  common /usrstr/ nnod,ncon,nods(20),nodc(10),ivls(10),kons(10)
  common /quiet/ iprt

  equivalence (cval,rval)

  data maxtg /100/

  ! Statement function
  mps(node) = 2_8**(ibt(node))-1

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call ufbtam(tab,my_i1,my_i2,iret,str)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  iret = 0

  if(msgp(0).eq.0) return

  do j=1,i2
    do i=1,i1
      tab(i,j) = bmiss
    enddo
  enddo

  ! Check for special tags in string

  call parstr(str,tgs,maxtg,ntg,' ',.true.)
  irec = 0
  isub = 0
  itbl = 0
  do i=1,ntg
    if(tgs(i).eq.'IREC') irec = i
    if(tgs(i).eq.'ISUB') isub = i
    if(tgs(i).eq.'ITBL') itbl = i
  enddo

  call status(munit,lun,il,im)

  ! Cycle through all of the memory messages in the internal arrays

  outer: do imsg=1,msgp(0)
    call rdmemm(imsg,subset,jdate,mret)
    if(mret.lt.0) then
      write(bort_str,'("BUFRLIB: UFBTAM - HIT END-OF-FILE READING MESSAGE NUMBER",I5," IN INTERNAL MEMORY")') imsg
      call bort(bort_str)
    endif

    call string(str,lun,i1,0)
    if(irec.gt.0) nods(irec) = 0
    if(isub.gt.0) nods(isub) = 0
    if(itbl.gt.0) nods(itbl) = 0

    ! Process all the subsets in the memory message

    do while (nsub(lun).lt.msub(lun))
      if(iret+1.gt.i2) then
        ! Emergency room treatment for array overflow
        call rdmemm(0,subset,jdate,mret)
        nrep = 0
        do kmsg=1,msgp(0)
          call rdmemm(kmsg,subset,jdate,mret)
          if(mret.lt.0) then
            write(bort_str,'("BUFRLIB: UFBTAM - HIT END-OF-FILE READING MESSAGE NUMBER",I5," IN INTERNAL MEMORY")') kmsg
            call bort(bort_str)
          endif
          nrep = nrep+nmsub(munit)
        enddo
        if(iprt.ge.0) then
          call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
          write ( unit=errstr, fmt='(A,A,I8,A,A)' ) 'BUFRLIB: UFBTAM - THE NO. OF DATA SUBSETS IN MEMORY ', &
            'IS .GT. LIMIT OF ', I2, ' IN THE 3RD ARG. (INPUT) - INCOMPLETE READ'
          call errwrt(errstr)
          write ( unit=errstr, fmt='(A,I8,A,I8,A)' ) '>>>UFBTAM STORED ', iret, ' REPORTS OUT OF ', nrep, '<<<'
          call errwrt(errstr)
          call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
          call errwrt(' ')
        endif
        exit outer
      endif

      iret = iret+1

      do i=1,nnod
        nods(i) = abs(nods(i))
      enddo

      call usrtpl(lun,1,1)
      mbit = mbyt(lun)*8+16
      nbit = 0
      n = 1

      inner: do while(n+1.le.nval(lun))
        n = n+1
        node = inv(n,lun)
        mbit = mbit+nbit
        nbit = ibt(node)
        if(itp(node).eq.1) then
          call upb8(ival,nbit,mbit,mbay(1,lun))
          nbmp=int(ival)
          call usrtpl(lun,n,nbmp)
        endif
        do i=1,nnod
          if(nods(i).eq.node) then
            if(itp(node).eq.1) then
              call upb8(ival,nbit,mbit,mbay(1,lun))
              tab(i,iret) = ival
            elseif(itp(node).eq.2) then
              call upb8(ival,nbit,mbit,mbay(1,lun))
              if(ival.lt.mps(node)) tab(i,iret) = ups(ival,node)
            elseif(itp(node).eq.3) then
              cval = ' '
              kbit = mbit
              call upc(cval,nbit/8,mbay(1,lun),kbit,.true.)
              tab(i,iret) = rval
            endif
            nods(i) = -nods(I)
            cycle inner
          endif
        enddo
        do i=1,nnod
          if(nods(i).gt.0) cycle inner
        enddo
      enddo inner

      ! Update the subset pointers before next read

      ibit = mbyt(lun)*8
      call upb(nbyt,16,mbay(1,lun),ibit)
      mbyt(lun) = mbyt(lun) + nbyt
      nsub(lun) = nsub(lun) + 1
      if(irec.gt.0) tab(irec,iret) = nmsg(lun)
      if(isub.gt.0) tab(isub,iret) = nsub(lun)
      if(itbl.gt.0) tab(itbl,iret) = ldxts
    enddo

  enddo outer

  ! Reset the memory file
  call rdmemm(0,subset,jdate,mret)

  return
end subroutine ufbtam
