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

  if(ifopbf==0) iprt = 0

  if(io=='QUIET') then
    ! override previous iprt value (printout indicator)
    iprtprv = iprt
    iprt = lundx
    if(iprt<-1) iprt = -1
    if(iprt>3) iprt =  3
    if(iprt>=0) then
      call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      write ( unit=errstr, FMT='(A,I3,A,A,I3,A)' ) 'BUFRLIB: OPENBF - DEGREE OF MESSAGE PRINT INDICATOR CHNGED FROM', &
        iprtprv,cprint(iprtprv+1),' TO',iprt,cprint(iprt+1)
      call errwrt(errstr)
      call errwrt('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
      call errwrt(' ')
    endif
  endif

  if(ifopbf==0) then
    ! This is the first call to this subroutine, so take care of some initial housekeeping tasks.
    ! Note that arallocf and arallocc_c must be called before calling bfrini.

    ! Allocate internal arrays.
    call arallocf
    call arallocc_c

    ! Initialize some global variables.
    call bfrini

    ifopbf = 1
  endif
  if( (io=='FIRST') .or. (io=='QUIET') ) return

  ! See if a file can be opened

  call status(lunit,lun,il,im)
  if(lun==0) then
    write(bort_str,'("BUFRLIB: OPENBF - THERE ARE ALREADY",I3," BUFR FILES OPENED, CANNOT OPEN FILE CONNECTED TO UNIT",I4)') &
      nfiles,lunit
    call bort(bort_str)
  endif
  if(il/=0) then
    write(bort_str,'("BUFRLIB: OPENBF - THE FILE CONNECTED TO UNIT",I5," IS ALREADY OPEN")') lunit
    call bort(bort_str)
  endif
  null(lun) = 0
  isc3(lun) = 0
  iscodes(lun) = 0
  lus(lun) = 0

  ! Use inquire to obtain the filename associated with unit lunit

  if (io/='NUL' .and. io/='INUL') then
    inquire(lunit,access=fileacc)
    if(fileacc=='UNDEFINED') open(lunit)
    inquire(lunit,name=filename)
    filename=trim(filename)//char(0)
  endif

  nmsg (lun) = 0
  nsub (lun) = 0
  msub (lun) = 0
  inode(lun) = 0
  idate(lun) = 0

  ! Decide how to open the file and setup the dictionary

  if(io=='IN') then
    call openrb_c(lun,filename)
    call wtstat(lunit,lun,-1,0)
    call readdx(lunit,lun,lundx)
  else if(io=='INUL') then
    call wtstat(lunit,lun,-1,0)
    if(lunit/=lundx) call readdx(lunit,lun,lundx)
    null(lun) = 1
  else if(io=='NUL') then
    call wtstat(lunit,lun,1,0)
    if(lunit/=lundx) call readdx(lunit,lun,lundx)
    null(lun) = 1
  else if(io=='INX') then
    call openrb_c(lun,filename)
    call wtstat(lunit,lun,-1,0)
    null(lun) = 1
  else if(io=='OUX') then
    call openwb_c(lun,filename)
    call wtstat(lunit,lun,1,0)
  else if(io=='SEC3') then
    call openrb_c(lun,filename)
    call wtstat(lunit,lun,-1,0)
    isc3(lun) = 1
  else if(io=='OUT') then
    call openwb_c(lun,filename)
    call wtstat(lunit,lun,1,0)
    call writdx(lunit,lun,lundx)
  else if(io=='NODX') then
    call openwb_c(lun,filename)
    call wtstat(lunit,lun,1,0)
    call readdx(lunit,lun,lundx)
  else if(io=='APN' .or. io=='APX') then
    call openab_c(lun,filename)
    call wtstat(lunit,lun,1,0)
    if(lunit/=lundx) call readdx(lunit,lun,lundx)
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
  endif

  call status(lunit,lun,il,im)
  if(il>0 .and. im/=0) call closmg(lunit)
  if(il/=0 .and. null(lun)==0) call closfb_c(lun)
  call wtstat(lunit,lun,0,0)

  ! Close Fortran unit if null(lun) = 0

  if(null(lun)==0) close(lunit)

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

  if(lunit<=0 .or. lunit>99) then
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
    if(abs(iolun(i))==lunit) lun = i
  enddo

  ! If not, try to define it so as to connect it to the library

  if(lun==0) then
    do i=1,nfiles
      if(iolun(i)==0) then
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

  if(lunit<=0) then
    write(bort_str,'("BUFRLIB: WTSTAT - INVALID UNIT NUMBER PASSED INTO FIRST ARGUMENT (INPUT) (=",I3,")")') lunit
    call bort(bort_str)
  endif
  if(lun<=0) then
    write(bort_str,'("BUFRLIB: WTSTAT - INVALID FILE ID PASSED INTO SECOND ARGUMENT (INPUT) (=",I3,")")') lun
    call bort(bort_str)
  endif
  if(il<-1 .or. il>1) then
    write(bort_str,'("BUFRLIB: WTSTAT - INVALID LOGICAL UNIT STATUS INDICATOR PASSED INTO THIRD ARGUMENT '// &
      '(INPUT) (=",I4,")")') il
    call bort(bort_str)
  endif
  if(im< 0 .or. im>1) then
    write(bort_str,'("BUFRLIB: WTSTAT - INVALID BUFR MESSAGE STATUS INDICATOR PASSED INTO FOURTH ARGUMENT '// &
      '(INPUT) (=",I4,")")') im
    call bort(bort_str)
  endif

  ! Check on lunit-lun combination

  if(abs(iolun(lun))/=lunit .and. (iolun(lun)/=0)) then
    write(bort_str,'("BUFRLIB: WTSTAT - ATTEMPTING TO REDEFINE EXISTING FILE UNIT (LOGICAL UNIT '// &
      'NUMBER ",I3,")")') iolun(lun)
    call bort(bort_str)
  endif

  ! Reset the file statuses

  if(il/=0) then
    iolun(lun) = sign(lunit,il)
    iomsg(lun) = im
  else
    iolun(lun) = 0
    iomsg(lun) = 0
  endif

  return
end subroutine wtstat

!> Get the current location of the file pointer within a BUFR file, in terms of a message number counting from the
!> beginning of the file, and a data subset number counting from the beginning of that message.
!>
!> @remarks
!> - Logical unit lunit should have already been opened via a previous
!> call to subroutine openbf(). If lunit was opened for input
!> operations, then kmsg is incremented with each call to any of the
!> [message-reading subroutines](@ref hierarchy), and ksub is
!> incremented with each call to any of the
!> [subset-reading subroutines](@ref hierarchy) for that message.
!> Otherwise, if lunit was opened for output operations, then kmsg is
!> incremented with each call to any of the
!> [message-writing subroutines](@ref hierarchy), and ksub is
!> incremented with each call to any of the
!> [subset-writing subroutines](@ref hierarchy) for that message.
!> - The value returned for kmsg does <b>not</b> include any messages
!> which contain DX BUFR tables information.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param kmsg - Ordinal number of current message, counting from the beginning of the BUFR file, but
!> not counting any messages which contain DX BUFR tables information
!> @param ksub - Ordinal number of current data subset within (kmsg)th message, counting from the
!> beginning of the message
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbcnt(lunit,kmsg,ksub)

  use modv_vars, only: im8b

  use moda_msgcwd

  implicit none

  integer, intent(in) :: lunit
  integer, intent(out) :: kmsg, ksub
  integer my_lunit, lun, il, im

  ! Check for I8 integers

  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call ufbcnt(my_lunit,kmsg,ksub)
    call x48(kmsg,kmsg,1)
    call x48(ksub,ksub,1)
    im8b=.true.
    return
  endif

  ! Check the file status - return the message and subset counters

  call status(lunit,lun,il,im)
  if(il==0) call bort('BUFRLIB: UFBCNT - BUFR FILE IS CLOSED, IT MUST BE OPEN FOR EITHER INPUT OR OUTPUT')
  kmsg = nmsg(lun)
  ksub = nsub(lun)

  return
end subroutine ufbcnt

!> Position an output BUFR file for appending.
!>
!> Read to the end of the file pointed to by abs(lunxx) and position it for appending. The file must have
!> already been opened for output operations.
!>
!< If lunxx > 0, then the file is backspaced before being positioned for append.
!> If lunxx < 0, then the file is not backspaced before being positioned for append.
!>
!> @param lunxx - Absolute value is Fortran logical unit number for BUFR file
!>
!> @author Woollen @date 1994-01-06
subroutine posapx(lunxx)

  use bufrlib

  use moda_mgwa

  implicit none

  integer, intent(in) :: lunxx
  integer lunit, lun, il, im, ier, idxmsg

  lunit = abs(lunxx)

  call status(lunit,lun,il,im)
  if(il==0) call bort('BUFRLIB: POSAPX - INPUT BUFR FILE IS CLOSED, IT MUST BE OPEN FOR OUTPUT')
  if(il<0) call bort('BUFRLIB: POSAPX - INPUT BUFR FILE IS OPEN FOR INPUT, IT MUST BE OPEN FOR OUTPUT')

  ! Try to read to the end of the file

  do while (.true.)
    call rdmsgw(lunit,mgwa,ier)
    if(ier<0) return
    if(idxmsg(mgwa)==1) then
      ! This is an internal dictionary message that was generated by the NCEPLIBS-bufr software.  Backspace the file pointer
      ! and then read and store all such dictionary messages (they should be stored consecutively!) and reset the internal tables.
      call backbufr_c(lun)
      call rdbfdx(lunit,lun)
    endif
  enddo

end subroutine posapx

!> Store or restore parameters associated with a BUFR file.
!>
!> Depending on the value of isr, either:
!> - Store the current parameters associated with a BUFR file
!> connected to lunit (read/write pointers, etc.), set the file status
!> to read, then rewind the BUFR file and position it such that the
!> next BUFR message read will be the first message in the file
!> containing actual subsets with data; or
!> - Restore the BUFR file connected to lunit to the parameters
!> it had prior to the previous call, and using the information that
!> was saved previously
!>
!> This allows information to be extracted from a particular subset in
!> a BUFR file which is in the midst of being read from or written to
!> by an application program.  Note that, for any given BUFR file, a call
!> to this subroutine with isr = 0 must precede a call to this same
!> subroutine with isr = 1.  An application program might first
!> call this subroutine with isr = 0, then call either
!> subroutine rdmgsb() or ufbinx() to get information from a subset, then
!> call this routine again with isr = 1 to restore the pointers in the
!> BUFR file to their original location.  For example, this subroutine is
!> called internally by subroutine ufbtab() whenever
!> the BUFR file it is acting upon is already open for input or output.
!>
!> @param lunit - Fortran logical unit number for BUFR file.
!> @param isr - Switch:
!> - 0 = Store current parameters associated with BUFR file, set file status to read, and rewind
!> file such that next message read is first message containing subset data
!> - 1 = Restore BUFR file with parameters saved from the previous call to this routine with isr = 0
!>
!> @author Woollen @date 2003-11-04
subroutine rewnbf(lunit,isr)

  use bufrlib

  use moda_msgcwd
  use moda_bitbuf
  use moda_bufrsr

  implicit none

  integer, intent(in) :: lunit, isr
  integer lun, il, im, i, kdate, ier

  character*128 bort_str
  character*8 subset

  ! Try to trap bad call problems
  if(isr==0) then
    call status(lunit,lun,il,im)
    if(jsr(lun)/=0) then
      write(bort_str,'("BUFRLIB: REWNBF - ATTEMPING TO SAVE '// &
        'PARAMETERS FOR FILE FOR WHICH THEY HAVE ALREADY BEEN SAVED (AND NOT YET RESTORED) (UNIT",I3,")")') lunit
      call bort(bort_str)
    endif
    if(il==0) then
      write(bort_str,'("BUFRLIB: REWNBF - ATTEMPING TO SAVE '// &
        'PARAMETERS FOR BUFR FILE WHICH IS NOT OPENED FOR EITHER INPUT OR OUTPUT) (UNIT",I3,")")') lunit
      call bort(bort_str)
    endif
  elseif(isr==1) then
    if(junn==0 .or. jsr(junn)/=1) then
      write(bort_str,'("BUFRLIB: REWNBF - ATTEMPING TO RESTORE '// &
        'PARAMETERS TO BUFR FILE WHICH WERE NEVER SAVED (UNIT",I3,")")') lunit
      call bort(bort_str)
    endif
    lun = junn
  else
     write(bort_str,'("BUFRLIB: REWNBF - SAVE/RESTORE SWITCH (INPUT '// &
      'ARGUMENT ISR) IS NOT ZERO OR ONE (HERE =",I4,") (UNIT",I3,")")') isr, lunit
     call bort(bort_str)
  endif

  if(isr==0) then
    ! Store the existing file parameters
    jmsg = nmsg(lun)
    jsub = nsub(lun)
    if ( il > 0 ) then
      ! The file is open for writing
      jbit = ibit
      jbyt = mbyt(lun)
      do i=1,jbyt
        jbay(i) = mbay(i,lun)
      enddo
    endif
    junn = lun
    jill = il
    jimm = im
    ! Reset the file for reading
    call wtstat(lunit,lun,-1,0)
  endif

  ! Rewind the file
  call cewind_c(lun)

  if(isr==1) then
    ! Restore the previous file parameters. Note that we already restored the previous value of lun earlier in this routine.

    ! Reset nmsg(lun) to 0, so that the below calls to readmg() will internally restore nmsg(lun) to the correct value.
    nmsg(lun) = 0

    ! Note that the below calls to readmg() are valid even if the file was previously open for writing, because we haven't yet
    ! called wtstat() to restore the file to its previous I/O status.  So until then we can still read from it as though it
    ! was an input file.
    do i=1,jmsg
      call readmg(lunit,subset,kdate,ier)
      if(ier<0) then
        write(bort_str,'("BUFRLIB: REWNBF - HIT END OF FILE BEFORE '// &
          'REPOSITIONING BUFR FILE IN UNIT",I3," TO ORIGINAL MESSAGE NO.",I5)') lunit, jmsg
        call bort(bort_str)
      endif
    enddo

    if ( jill < 0 ) then
      ! The file was previously open for reading
      do i=1,jsub
        call readsb(lunit,ier)
      enddo
    else
      ! The file was previously open for writing
      do i=1,jbyt
        mbay(i,lun) = jbay(i)
      enddo
      nsub(lun) = jsub
      mbyt(lun) = jbyt
      ibit = jbit
    endif

    ! Now restore the file to its previous I/O status
    il = jill
    im = jimm
    call wtstat(lunit,lun,il,im)
  endif

  ! Toggle the stack status indicator
  jsr(lun) = mod(jsr(lun)+1,2)

  return
end subroutine rewnbf

!> Read through every data subset in a BUFR file
!> and return one or more specified data values from each subset.
!>
!> This provides a useful way to scan the ranges of one or more
!> specified data values across all of the data subsets within an
!> entire BUFR file.  It is similar to subroutine ufbtam(), except
!> that ufbtam() works on data subsets within internal arrays.
!>
!> It is the user's responsibility to ensure that tab is dimensioned
!> sufficiently large enough to accommodate the number of data values
!> that are to be read from the BUFR file.  Specifically, each row of
!> tab will contain the data values read from a different data subset,
!> so the value i2 must be at least as large as the total number of data
!> subsets in the BUFR file.
!>
!> If logical unit abs(lunin) has already been opened
!> via a previous call to subroutine openbf(), then this subroutine
!> will save the current file position, rewind the file to the
!> beginning, read through the entire file, and then restore it to its
!> previous file position.  Otherwise, if logical unit abs(lunin) has
!> not already been opened via a previous call to subroutine openbf(),
!> then this subroutine will open it via an internal call to
!> subroutine openbf(), read through the entire file, and then close
!> it via an internal call to subroutine closbf().
!>
!> @remarks
!> - If lunin < 0, the number of data subsets in the BUFR file will
!> still be returned in iret; however, str will be ignored,
!> and all of the values returned in tab will contain the current
!> placeholder value for "missing" data.
!> - If any of the Table B mnemonics in str are replicated within the
!> data subset definition for the BUFR file, then this subroutine will
!> only return the value corresponding to the first occurrence of each
!> such mnemonic (counting from the beginning of the data subset
!> definition) within the corresponding row of tab.
!> - There are a few additional special mnemonics that can be
!> included within str when calling this subroutine, and which in turn
!> will result in special information being returned within the
!> corresponding location in tab:
!>      - IREC - returns the number of the BUFR message within the file pointed to by abs(lunin) (counting from the
!>        beginning of the file) in which the current data subset resides
!>      - ISUB - returns the number of the current data subset within the BUFR message pointed to by IREC, counting from
!>        the beginning of the message
!>
!> @param lunin - Absolute value is Fortran logical unit number for BUFR file
!> @param tab - Data values
!> @param i1 - First dimension of tab as allocated within the calling program
!> @param i2 - Second dimension of tab as allocated within the calling program
!> @param iret - Number of data subsets in BUFR file
!> @param str - String of blank-separated Table B mnemonics, in one-to-one correspondence with the number of data values
!> that will be read from each data subset within the first dimension of tab (see [DX BUFR Tables](@ref dfbftab) for
!> further information about Table B mnemonics)
!>
!> @author J. Woollen @date 1994-01-06
recursive subroutine ufbtab(lunin,tab,i1,i2,iret,str)

  use modv_vars, only: im8b, bmiss, iac

  use moda_usrint
  use moda_msgcwd
  use moda_unptyp
  use moda_bitbuf
  use moda_tables

  implicit none

  integer*8 ival, lref, ninc, mps, lps
  integer, intent(in) :: lunin, i1, i2
  integer, intent(out) :: iret
  integer, parameter :: maxtg = 100
  integer nnod, ncon, nods, nodc, ivls, kons, iprt, my_lunin, my_i1, my_i2, lunit, lun, il, im, irec, isub, i, j, n, ntg, &
    jdate, jbit, kbit, lbit, mbit, nbit, nibit, nbyt, nsb, node, nbmp, nrep, lret, linc, iac_prev, ityp, &
    ireadmg, ireadsb, nmsub

  character*(*), intent(in) :: str
  character*128 errstr
  character*40 cref
  character*10 tgs(maxtg)
  character*8 subset, cval

  logical openit, overflow, just_count, need_node

  real*8, intent(out) :: tab(i1,i2)
  real*8 rval, ups

  common /usrstr/ nnod, ncon, nods(20), nodc(10), ivls(10), kons(10)
  common /quiet/ iprt

  equivalence (cval,rval)

  ! Statement functions
  mps(node) = 2_8**(ibt(node))-1
  lps(lbit) = max(2_8**(lbit)-1,1)

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(lunin,my_lunin,1)
    call x84(i1,my_i1,1)
    call x84(i2,my_i2,1)
    call ufbtab(my_lunin,tab,my_i1,my_i2,iret,str)
    call x48(iret,iret,1)
    im8b=.true.
    return
  endif

  ! Make sure subroutine openbf() has been called at least once before trying to call subroutine status(); otherwise,
  ! status() might try to access array space that hasn't yet been dynamically allocated.
  call openbf(0,'FIRST',0)

  lunit = abs(lunin)
  call status(lunit,lun,il,im)
  openit = il==0

  if(openit) then
    ! Open BUFR file connected to unit lunit if it isn't already open
    call openbf(lunit,'INX',lunit)
  else
    ! If BUFR file already opened, save position and rewind to first data message
    call rewnbf(lunit,0)
  endif

  ! Initialize all of the output array values to the current value for "missing"
  do j=1,i2
    do i=1,i1
      tab(i,j) = bmiss
    enddo
  enddo

  ! Set counters to zero
  iret = 0
  irec = 0
  isub = 0

  iac_prev = iac
  iac = 1

  overflow = .false.

  ! Check for count subset only option
  just_count = lunin<lunit
  if(just_count) then
    do while(ireadmg(-lunit,subset,jdate)>=0)
      iret = iret+nmsub(lunit)
    enddo
  else
    ! Check for special tags in string
    call parstr(str,tgs,maxtg,ntg,' ',.true.)
    do i=1,ntg
      if(tgs(i)=='IREC') irec = i
      if(tgs(i)=='ISUB') isub = i
    enddo
  endif

  outer: do while (.not. just_count)
    ! Read the next message from the file
    if(ireadmg(-lunit,subset,jdate)<0) exit
    call string(str,lun,i1,0)
    if(irec>0) nods(irec) = 0
    if(isub>0) nods(isub) = 0

    if(msgunp(lun)/=2) then
      ! The message is uncompressed

      inner1: do while (.true.)
        ! Get the next subset from the message
        if(nsub(lun)==msub(lun)) cycle outer
        if(iret+1>i2) then
          overflow = .true.
          exit outer
        endif
        iret = iret+1
        do i=1,nnod
          nods(i) = abs(nods(i))
        enddo
        if(msgunp(lun)==0) then
          mbit = mbyt(lun)*8 + 16
        else
          mbit = mbyt(lun)
        endif
        nbit = 0
        n = 1
        call usrtpl(lun,n,n)
        inner2: do while (.true.)
          ! Cycle through each node of the subset to look for the requested values
          if(n+1<=nval(lun)) then
            n = n+1
            node = inv(n,lun)
            mbit = mbit+nbit
            nbit = ibt(node)
            if(itp(node)==1) then
              call upb8(ival,nbit,mbit,mbay(1,lun))
              nbmp=int(ival)
              call usrtpl(lun,n,nbmp)
            endif
            do i=1,nnod
              if(nods(i)==node) then
                if(itp(node)==1) then
                  call upb8(ival,nbit,mbit,mbay(1,lun))
                  tab(i,iret) = ival
                elseif(itp(node)==2) then
                  call upb8(ival,nbit,mbit,mbay(1,lun))
                  if(ival<mps(node)) tab(i,iret) = ups(ival,node)
                elseif(itp(node)==3) then
                  cval = ' '
                  kbit = mbit
                  call upc(cval,nbit/8,mbay(1,lun),kbit,.true.)
                  tab(i,iret) = rval
                endif
                nods(i) = -nods(i)
                cycle inner2
              endif
            enddo
            do i=1,nnod
              if(nods(i)>0) cycle inner2
            enddo
          endif
          exit
        enddo inner2
        ! Update the subset pointers
        if(msgunp(lun)==0) then
          ibit = mbyt(lun)*8
          call upb(nbyt,16,mbay(1,lun),ibit)
          mbyt(lun) = mbyt(lun) + nbyt
        else
          mbyt(lun) = mbit
        endif
        nsub(lun) = nsub(lun) + 1
        if(irec>0) tab(irec,iret) = nmsg(lun)
        if(isub>0) tab(isub,iret) = nsub(lun)
      enddo inner1

    else
      ! The message is compressed

      if(iret+msub(lun)>i2) then
        overflow = .true.
        exit outer
      endif
      if(irec>0.or.isub>0) then
        do nsb=1,msub(lun)
          if(irec>0) tab(irec,iret+nsb) = nmsg(lun)
          if(isub>0) tab(isub,iret+nsb) = nsb
        enddo
      endif
      call usrtpl(lun,1,1)
      ibit = mbyt(lun)
      n = 0
      inner3: do while ( n < nval(lun) )
        ! Cycle through each node of each subset to look for the requested values
        n = n+1
        node = inv(n,lun)
        nbit = ibt(node)
        ityp = itp(node)
        if(n==1) then
          ! Reset the node indices
          do i=1,nnod
            nods(i) = abs(nods(i))
          enddo
        else
          ! Are we still looking for more values?
          need_node = .false.
          do i=1,nnod
            if(nods(i)>0) then
              need_node = .true.
              exit
            endif
          enddo
          if(.not. need_node) exit inner3
        endif
        if(ityp==1 .or. ityp==2) then
          call up8(lref,nbit,mbay(1,lun),ibit)
          call upb(linc,6,mbay(1,lun),ibit)
          nibit = ibit + linc*msub(lun)
        elseif(ityp==3) then
          cref=' '
          call upc(cref,nbit/8,mbay(1,lun),ibit,.true.)
          call upb(linc,6,mbay(1,lun),ibit)
          nibit = ibit + 8*linc*msub(lun)
        else
          cycle
        endif
        if(ityp==1) then
          ! This is a delayed replication node
          jbit = ibit + linc
          call up8(ninc,linc,mbay(1,lun),jbit)
          ival = lref+ninc
          call usrtpl(lun,n,int(ival))
          cycle
        endif
        do i=1,nnod
          if(node==nods(i)) then
            ! This is one of the requested values, so store the corresponding value from each subset in the message
            nods(i) = -nods(i)
            lret = iret
            if(ityp==1 .or. ityp==2) then
              do nsb=1,msub(lun)
                jbit = ibit + linc*(nsb-1)
                call up8(ninc,linc,mbay(1,lun),jbit)
                ival = lref+ninc
                lret = lret+1
                if(ninc<lps(linc)) tab(i,lret) = ups(ival,node)
              enddo
            elseif(ityp==3) then
              do nsb=1,msub(lun)
                if(linc==0) then
                  cval = cref(1:8)
                else
                  jbit = ibit + linc*(nsb-1)*8
                  cval = ' '
                  call upc(cval,linc,mbay(1,lun),jbit,.true.)
                endif
                lret = lret+1
                tab(i,lret) = rval
              enddo
            else
              call bort('UFBTAB - INVALID ELEMENT TYPE SPECIFIED')
            endif
          endif
        enddo
        ibit = nibit
      enddo inner3
      iret = iret+msub(lun)

    endif

  enddo outer

  if(overflow) then
    nrep = iret
    do while(ireadsb(lunit)==0)
      nrep = nrep+1
    enddo
    do while(ireadmg(-lunit,subset,jdate)>=0)
      nrep = nrep+nmsub(lunit)
    enddo
    if(iprt>=0) then
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      write ( unit=errstr, fmt='(A,A,I8,A)' ) 'BUFRLIB: UFBTAB - THE NO. OF DATA SUBSETS IN THE BUFR FILE ', &
        'IS .GT. LIMIT OF ', i2, ' IN THE 4TH ARG. (INPUT) - INCOMPLETE READ'
      call errwrt(errstr)
      write ( unit=errstr, fmt='(A,I8,A,I8,A)' ) '>>>UFBTAB STORED ', iret, ' REPORTS OUT OF ', nrep, '<<<'
      call errwrt(errstr)
      call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
      call errwrt(' ')
    endif
  endif

  if(openit) then
    ! Close BUFR file if it was opened here
    call closbf(lunit)
  else
    ! Restore BUFR file to its previous status and position
    call rewnbf(lunit,1)
  endif

  iac = iac_prev

  return
end subroutine ufbtab
