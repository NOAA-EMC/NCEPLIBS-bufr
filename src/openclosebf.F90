!> @file
!> @brief Open or close a file to the library
!>
!> @authors J. Woollen, J. Ator, D. Keyser @date 1994-01-06

!> Open a Fortran file on the local system.
!>
!> @param filename - name of the file to be opened
!> @param lunit    - Fortran logical unit number for Fortran file
!> @param format   - format of the Fortran file
!> @param position - to rewind or continue with open file
!> @param iret    - return code from the Fortran open statement
!>
!> @author Jeff Whitaker @date 2015-08-30
recursive subroutine fortran_open(filename, lunit, format, position, iret)

  use modv_vars, only: im8b

  implicit none
  character*(*), intent(in) :: filename, format, position
  integer, intent(in)  :: lunit
  integer, intent(out) :: iret
  integer my_lunit

  ! check for i8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call fortran_open(filename,my_lunit,format,position,iret)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  open(lunit, file=trim(filename), form=trim(format), position=trim(position), iostat=iret)
  return
end subroutine fortran_open

!> Close a Fortran file on the local system.
!>
!> @param lunit - Fortran logical unit number for Fortran file
!> @param iret - return code from the Fortran close statement
!>
!> @author Jeff Whitaker @date 2015-08-30
recursive subroutine fortran_close(lunit, iret)

  use modv_vars, only: im8b

  implicit none
  integer, intent(in)  :: lunit
  integer, intent(out) :: iret
  integer my_lunit

  ! check for i8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call fortran_close(my_lunit,iret)
    call x48(iret,iret,1)

    im8b=.true.
    return
  endif

  close(lunit, iostat=iret)
  return
end subroutine fortran_close

!> Connect a new file to the NCEPLIBS-bufr software for
!> input or output operations, or initialize the library without
!> connecting to a file, or change the verbosity of the library for an
!> already-connected BUFR file.
!>
!> The logical unit numbers lunit and lundx must already be associated
!> with actual filenames on the local system, typically via a Fortran "OPEN"
!> statement. Multiple logical units can be connected to the NCEPLIBS-bufr software
!> at any one time.
!>
!> The argument io is a character string describing how the file connected to
!> lunit will be used, e.g. 'IN' is used to access an existing file of BUFR
!> messages for input (i.e. reading/decoding BUFR), and 'OUT' is used to access
!> a new file for output (i.e. writing/encoding BUFR). An option 'APX' is also
!> available which behaves like 'OUT', except that output is then appended to
!> an existing BUFR file rather than creating a new one from scratch, and there
!> are also some additional options 'NUL' and 'NODX' which can likewise be used
!> instead of 'OUT' for some very special cases as needed. There's also an
!> option 'SEC3' which can be used in place of 'IN' for certain cases when the
!> user is attempting to read BUFR messages whose content and descriptor layout
!> are unknown in advance. However, all of these additional options are
!> basically just variations of 'IN' or 'OUT', again depending on whether the
!> intent is to read or write BUFR messages from the file connected to lunit.
!> The only exceptions are when io = 'FIRST' or 'QUIET'.  When io = 'FIRST',
!> the subroutine simply checks whether it has already been called from within
!> the application program and, if not, goes ahead and initializes the library
!> without actually connecting any files in lunit or lundx.
!>
!> Alternatively, when io = 'QUIET', the subroutine simply sets or resets the
!> internal print verbosity switch to the value of input argument lundx,
!> overriding its previous value and/or its internal default value of 0.
!>
!> The third and final call argument lundx identifies the logical unit which
!> contains the definition of the DX BUFR tables to be associated with unit
!> lunit.  Except when io = 'SEC3', every BUFR file that is linked to the NCEPLIBS-bufr
!> software must have a DX BUFR tables file associated with it, and these tables
!> may be defined within a separate ASCII text file
!> (see [Description and Format of DX BUFR Tables](@ref dfbftab) for more info.)
!> or, in the case of an existing BUFR file, may be embedded within the first few
!> BUFR messages of the file itself, and in which case the user can denote this
!> to the subroutine by setting lundx to the same value as lunit.
!>
!> @remarks
!> - When an existing BUFR file is accessed for input (i.e. reading/decoding BUFR),
!> the associated DX BUFR tables defined by lundx are stored internally within
!> the NCEPLIBS-bufr software and are referenced during all subsequent processing of
!> the file. Likewise, when a file is accessed for output (i.e. writing/encoding
!> BUFR), the associated DX BUFR tables are still stored internally for subsequent
!> reference; however, the output file itself is also initialized by writing the
!> BUFR table information (as one or more BUFR messages) to the beginning of the
!> file, except when io = 'NODX', and in which case the writing of these
!> additional messages is suppressed.
!> - As noted above, 'SEC3' is the only value of io (other than 'FIRST' or 'QUIET') where it's
!> not necessary to provide pre-defined DX BUFR tables via lundx.  Instead, this
!> option instructs the NCEPLIBS-bufr software to unpack the data description section
!> (Section 3) from each BUFR message it reads and then decode the contents
!> accordingly. In this case, it's necessary to provide a set of BUFR master
!> tables containing listings of all possible BUFR descriptors
!> (see [Description and Format of master BUFR Tables](@ref dfbfmstab) for more
!> info.), but otherwise no prior knowledge is required of the contents of the
!> messages to be decoded.
!>
!>
!> @param lunit - Fortran logical unit number for BUFR file (unless io is set to 'FIRST' or 'QUIET',
!> in which case this is a dummy argument)
!> @param io - flag indicating how lunit is to be used by the software:
!>   - 'IN' = input operations with table processing
!>   - 'INX' = input operations w/o table processing
!>   - 'OUX' = output operations w/o table processing
!>   - 'OUT' = output operations with table processing
!>   - 'SEC3' = same as 'IN', except use Section 3 of input messages for decoding rather than DX BUFR table information
!>   from lundx; in this case lundx is ignored, and the user must provide appropriate [master BUFR tables](@ref dfbfmstab)
!>   within the directory specified by a subsequent call to subroutine mtinfo()
!>   - 'NODX' = same as 'OUT', except don't write DX BUFR table messages to lunit
!>   - 'APN' = same as 'NODX', except begin writing at end of file ("append")
!>   - 'APX' = same as 'APN', except backspace before appending
!>   - 'NUL' = same as 'OUT', except don't write any messages whatsoever to lunit (e.g. when subroutine writsa() is to be used)
!>   - 'INUL' = same as 'IN', except don't read any messages whatsoever from lunit (e.g. when subroutine readerme() is to be used)
!>   - 'QUIET' = lunit is ignored; this is an indicator that the value for iprt in common block /quiet/ is being reset to the
!>   value in lundx
!>   - 'FIRST' = lunit and lundx are ignored; this is an indicator to initialize the NCEPLIBS-bufr software, in case this
!>   subroutine was never previously called
!> @param lundx - Fortran logical unit number containing DX BUFR table information, except as noted below:
!>   - If io is not set to 'FIRST' or 'QUIET' = Fortran logical unit number containing DX BUFR table information to be used in
!>   reading/writing from/to lunit (depending on the case); this value may be set equal to lunit if DX BUFR table information is
!>   already embedded in lunit
!>   - If io is set to 'QUIET' = indicator for degree of printout:
!>      - -1 = no printout except for ABORT messages
!>      -  0 = limited printout (default)
!>      -  1 = all warning messages are printed
!>      -  2 = all warning and info messages are printed
!>      -  3 = all warning, info and debug messages are printed
!>
!> @authors J. Woollen, J. Ator,  D. Keyser @date 1994-01-06
recursive subroutine openbf(lunit,io,lundx)

  use bufrlib

  use modv_vars, only: im8b, ifopbf, nfiles

  use moda_msgcwd
  use moda_stbfr
  use moda_sc3bfr
  use moda_lushr
  use moda_nulbfr
  use moda_stcode

  implicit none

  integer, intent(in) :: lunit, lundx
  integer my_lunit, my_lundx, iprt, iprtprv, lun, il, im

  character*(*), intent(in) :: io
  character*255 filename, fileacc
  character*128 bort_str, errstr
  character*28 cprint(0:4)

  common /quiet/ iprt

  data cprint/ &
    ' (only ABORTs)              ', &
    ' (limited -default)         ', &
    ' (all warnings)             ', &
    ' (all warnings+infos)       ', &
    ' (all warnings+infos+debugs)'/

  ! Check for i8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call x84(lundx,my_lundx,1)
    call openbf(my_lunit,io,my_lundx)

    im8b=.true.
    return
  endif

  ! If this is the first call to this subroutine, initialize iprt in /quiet/ as 0

  if(ifopbf.eq.0) iprt = 0

  if(io.eq.'QUIET') then
    ! override previous iprt value (printout indicator)
    iprtprv = iprt
    iprt = lundx
    if(iprt.lt.-1) iprt = -1
    if(iprt.gt.3) iprt =  3
    if(iprt.ge.0) then
      call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      write ( unit=errstr, FMT='(A,I3,A,A,I3,A)' ) 'BUFRLIB: OPENBF - DEGREE OF MESSAGE PRINT INDICATOR CHNGED FROM', &
        iprtprv,cprint(iprtprv+1),' TO',iprt,cprint(iprt+1)
      call errwrt(errstr)
      call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      call errwrt(' ')
    endif
  endif

  if(ifopbf.eq.0) then
    ! This is the first call to this subroutine, so take care of some initial housekeeping tasks.
    ! Note that arallocf and arallocc_c must be called before calling bfrini.

    ! Allocate internal arrays.
    call arallocf
    call arallocc_c

    ! Initialize some global variables.
    call bfrini

    ifopbf = 1
  endif
  if( (io.eq.'FIRST') .or. (io.eq.'QUIET') ) return

  ! See if a file can be opened

  call status(lunit,lun,il,im)
  if(lun.eq.0) then
    write(bort_str,'("BUFRLIB: OPENBF - THERE ARE ALREADY",I3," BUFR FILES OPENED, CANNOT OPEN FILE CONNECTED TO UNIT",I4)') &
      nfiles,lunit
    call bort(bort_str)
  endif
  if(il.ne.0) then
    write(bort_str,'("BUFRLIB: OPENBF - THE FILE CONNECTED TO UNIT",I5," IS ALREADY OPEN")') lunit
    call bort(bort_str)
  endif
  null(lun) = 0
  isc3(lun) = 0
  iscodes(lun) = 0
  lus(lun) = 0

  ! Use inquire to obtain the filename associated with unit lunit

  if (io.ne.'NUL' .and. io.ne.'INUL') then
    inquire(lunit,access=fileacc)
    if(fileacc=='UNDEFINED') open(lunit)
    inquire(lunit,name=filename)
    filename=trim(filename)//char(0)
  ENDIF

  nmsg (lun) = 0
  nsub (lun) = 0
  msub (lun) = 0
  inode(lun) = 0
  idate(lun) = 0

  ! Decide how to open the file and setup the dictionary

  if(io.eq.'IN') then
    call openrb_c(lun,filename)
    call wtstat(lunit,lun,-1,0)
    call readdx(lunit,lun,lundx)
  else if(io.eq.'INUL') then
    call wtstat(lunit,lun,-1,0)
    if(lunit.ne.lundx) call readdx(lunit,lun,lundx)
    null(lun) = 1
  else if(io.eq.'NUL') then
    call wtstat(lunit,lun,1,0)
    if(lunit.ne.lundx) call readdx(lunit,lun,lundx)
    null(lun) = 1
  else if(io.eq.'INX') then
    call openrb_c(lun,filename)
    call wtstat(lunit,lun,-1,0)
    null(lun) = 1
  else if(io.eq.'OUX') then
    call openwb_c(lun,filename)
    call wtstat(lunit,lun,1,0)
  else if(io.eq.'SEC3') then
    call openrb_c(lun,filename)
    call wtstat(lunit,lun,-1,0)
    isc3(lun) = 1
  else if(io.eq.'OUT') then
    call openwb_c(lun,filename)
    call wtstat(lunit,lun,1,0)
    call writdx(lunit,lun,lundx)
  else if(io.eq.'NODX') then
    call openwb_c(lun,filename)
    call wtstat(lunit,lun,1,0)
    call readdx(lunit,lun,lundx)
  else if(io.eq.'APN' .or. io.eq.'APX') then
    call openab_c(lun,filename)
    call wtstat(lunit,lun,1,0)
    if(lunit.ne.lundx) call readdx(lunit,lun,lundx)
    call posapx(lunit)
  else
    call bort('BUFRLIB: OPENBF - ILLEGAL SECOND (INPUT) ARGUMENT')
  endif

  return
end subroutine openbf

!> Close the connection between logical unit lunit and the NCEPLIBS-bufr software.
!>
!> @remarks
!> - This subroutine will execute a Fortran "CLOSE" on logical unit lunit, even though subroutine openbf() didn't
!> previously handle the corresponding Fortran "OPEN" of the same file.
!> - It's a good idea to call this subroutine for every lunit that was opened to the software via openbf(); however, it's
!> especially important to do so when writing/encoding a BUFR file, in order to ensure that all output is properly flushed
!> to lunit.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!>
!> @author J. Woollen, J. Ator @date 1994-01-06
recursive subroutine closbf(lunit)

  use bufrlib

  use modv_vars, only: im8b

  use moda_nulbfr

  implicit none

  character*128 errstr

  integer, intent(in) :: lunit
  integer my_lunit, lun, il, im

  ! Check for i8 integers

  if(im8b) then
     im8b=.false.

     call x84(lunit,my_lunit,1)
     call closbf(my_lunit)

     im8b=.true.
     return
  endif

  if ( .not. allocated(null) ) then
    call errwrt('++++++++++++++++++++WARNING++++++++++++++++++++++')
    errstr = 'BUFRLIB: CLOSBF WAS CALLED WITHOUT HAVING PREVIOUSLY CALLED OPENBF'
    call errwrt(errstr)
    call errwrt('++++++++++++++++++++WARNING++++++++++++++++++++++')
    return
  ENDIF

  call status(lunit,lun,il,im)
  if(il.gt.0 .and. im.ne.0) call closmg(lunit)
  if(il.ne.0 .and. null(lun).eq.0) call closfb_c(lun)
  call wtstat(lunit,lun,0,0)

  ! Close Fortran unit if null(lun) = 0

  if(null(lun).eq.0) close(lunit)

  return
end subroutine closbf

!> Check whether a specified Fortran logical unit number is currently connected to the NCEPLIBS-bufr software.
!>
!> If the unit number is already connected, then the subroutine returns information about the associated file.
!> Otherwise, it returns the next available file ID that could be used to connect the associated file to the
!> software via a subsequent call to subroutine wtstat().
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param lun - File ID associated with lunit
!> - 0 = lunit is not already connected to the software, <b>and</b> there is no remaining internal space available that
!> could be used to connect it
!> @param il - File status:
!> - 0 = lunit is not already connected to the software, but lun contains a file ID that could be used to connect it via
!> a subsequent call to subroutine wtstat()
!> - 1 = lunit is already connected to the software for output operations (i.e. writing/encoding BUFR)
!> - -1 = lunit is already connected to the software for input operations (i.e. reading/decoding BUFR)
!> @param im - Message status, indicating whether there is already a message open within internal arrays
!> for lunit:
!> - 0 = No
!> - 1 = Yes
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine status(lunit,lun,il,im)

  use modv_vars, only: im8b, nfiles

  use moda_stbfr

  implicit none

  integer, intent(in) :: lunit
  integer, intent(out) :: lun, il, im
  integer my_lunit, i

  character*128 bort_str, errstr

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.

    call x84(lunit,my_lunit,1)
    call status(my_lunit,lun,il,im)
    call x48(lun,lun,1)
    call x48(il,il,1)
    call x48(im,im,1)

    im8b=.true.
    return
  endif

  if(lunit.le.0 .or. lunit.gt.99) then
    write(bort_str,'("BUFRLIB: STATUS - INPUT UNIT NUMBER (",I3,") OUTSIDE LEGAL RANGE OF 1-99")') lunit
    call bort(bort_str)
  endif

  ! Clear the status indicators

  lun = 0
  il  = 0
  im  = 0

  ! See if the unit is already connected to the library

  if ( .not. allocated(iolun) ) then
    call errwrt('++++++++++++++++++++WARNING++++++++++++++++++++++')
    errstr = 'BUFRLIB: STATUS WAS CALLED WITHOUT HAVING PREVIOUSLY CALLED OPENBF'
    call errwrt(errstr)
    call errwrt('++++++++++++++++++++WARNING++++++++++++++++++++++')
    return
  endif

  do i=1,nfiles
    if(abs(iolun(i)).eq.lunit) lun = i
  enddo

  ! If not, try to define it so as to connect it to the library

  if(lun.eq.0) then
    do i=1,nfiles
      if(iolun(i).eq.0) then
        ! File space is available, return with lun > 0, il and im remain 0
        lun = i
        return
      endif
    enddo
    ! File space is NOT available, return with lun, il and im all 0
    return
  endif

  ! If the unit was already connected to the library prior to this call, then return statuses

  il = sign(1,iolun(lun))
  im = iomsg(lun)

  return
end subroutine status

!> Update file status in library internals.
!>
!> This subroutine can be used to connect or disconnect a specified
!> Fortran logical unit number to/from the NCEPLIBS-bufr software, and it
!> can also be used to set or reset the internal message status
!> associated with that logical unit number.
!>
!> @note Before this subroutine is called to connect any lunit to the
!> software, a previous call should have been made to subroutine
!> status() to confirm that internal space is available to connect
!> the associated file, as well as to obtain an lun value to use
!> in connecting it. Once a file is connected, the corresponding
!> lunit and lun values remain linked to each other for as
!> long as the file is connected to the software.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param lun - File ID associated with lunit
!> @param il - File status update option:
!>  - 0 Disconnect lunit from the software
!>  - 1 Connect lunit to the software for output operations (i.e. writing/encoding BUFR), if not already connected
!>  - -1 Connect lunit to the software for input operations (i.e. reading/decoding BUFR), if not already connected
!> @param im - Message status update option, indicating whether a message is currently open within the internal
!> arrays for lunit:
!>  - 0 No
!>  - 1 Yes
!>
!> @author J. Woollen @date 1994-01-06
subroutine wtstat(lunit,lun,il,im)

  use moda_stbfr

  implicit none

  integer, intent(in) :: lunit, lun, il, im

  character*128 bort_str

  ! Check on the arguments

  if(lunit.le.0) then
    write(bort_str,'("BUFRLIB: WTSTAT - INVALID UNIT NUMBER PASSED INTO FIRST ARGUMENT (INPUT) (=",I3,")")') lunit
    call bort(bort_str)
  endif
  if(lun.le.0) then
    write(bort_str,'("BUFRLIB: WTSTAT - INVALID FILE ID PASSED INTO SECOND ARGUMENT (INPUT) (=",I3,")")') lun
    call bort(bort_str)
  endif
  if(il.lt.-1 .or. il.gt.1) then
    write(bort_str,'("BUFRLIB: WTSTAT - INVALID LOGICAL UNIT STATUS INDICATOR PASSED INTO THIRD ARGUMENT '// &
      '(INPUT) (=",I4,")")') il
    call bort(bort_str)
  endif
  if(im.lt. 0 .or. im.gt.1) then
    write(bort_str,'("BUFRLIB: WTSTAT - INVALID BUFR MESSAGE STATUS INDICATOR PASSED INTO FOURTH ARGUMENT '// &
      '(INPUT) (=",I4,")")') im
    call bort(bort_str)
  endif

  ! Check on lunit-lun combination

  if(abs(iolun(lun)).ne.lunit .and. (iolun(lun).ne.0)) then
    write(bort_str,'("BUFRLIB: WTSTAT - ATTEMPTING TO REDEFINE EXISTING FILE UNIT (LOGICAL UNIT '// &
      'NUMBER ",I3,")")') iolun(lun)
    call bort(bort_str)
  endif

  ! Reset the file statuses

  if(il.ne.0) then
    iolun(lun) = sign(lunit,il)
    iomsg(lun) = im
  else
    iolun(lun) = 0
    iomsg(lun) = 0
  endif

  return
end subroutine wtstat
