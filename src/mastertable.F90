!> @file
!> @brief Read or write master table information.
!>
!> @author J. Ator @date 2009-03-23

!> Specify the directory location and Fortran logical unit numbers to be used when reading master
!> BUFR tables on the local file system.
!>
!> @param cmtdir - Directory location of master BUFR tables on local file system (up to 240 characters)
!> @param lunmt1 - First Fortran logical unit number to use when reading master BUFR tables on local file system
!> @param lunmt2 - Second Fortran logical unit number to use when reading master BUFR tables on local file system
!>
!> See [Master BUFR Tables](@ref dfbfmstab)
!> for more information about master BUFR tables.  In particular, note
!> that this subroutine is normally only ever called after a prior call
!> has been made to subroutine openbf() with io = 'SEC3'.  But in such
!> cases, any call to this subroutine must be made prior to any
!> subsequent calls to any of the BUFR
!> [message-reading subroutines](@ref hierarchy) for the associated BUFR
!> file; otherwise, default values for cmtdir, lunmt1 and lunmt2 will be
!> used as defined within subroutine bfrini().
!>
!> For cmtdir, any full or relative directory pathname that is legal
!> on the local filesystem is permissible, up to a total maximum length
!> of 240 characters.  The library will then automatically search
!> within this directory for any necessary master table files and open and
!> read them as needed.
!>
!> The logical unit numbers lunmt1 and lunmt2 should be distinct from
!> each other but should not already be assigned to any files on the
!> local system.
!>
!> @author J. Ator @date 2009-03-23
recursive subroutine mtinfo ( cmtdir, lunmt1, lunmt2 )

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: lunmt1, lunmt2
  integer my_lunmt1, my_lunmt2, lun1, lun2, lmtd

  character*(*), intent(in) :: cmtdir
  character*240 mtdir

  common /mstinf/ lun1, lun2, lmtd, mtdir

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.

    call x84 ( lunmt1, my_lunmt1, 1 )
    call x84 ( lunmt2, my_lunmt2, 1 )
    call mtinfo ( cmtdir, my_lunmt1, my_lunmt2 )

    im8b=.true.
    return
  endif

  call strsuc ( cmtdir, mtdir, lmtd )

  lun1 = lunmt1
  lun2 = lunmt2

  return
end subroutine mtinfo

!> Based on the input arguments, determine the names of the corresponding standard and local master table files.
!>
!> After determining the corresponding file names, this subroutine then confirms the existence of those files on the
!> filesystem, using additional information obtained from the most recent call to subroutine mtinfo(), or else as
!> defined within subroutine bfrini() if subroutine mtinfo() was never called.
!>
!> @param imt - Master table number
!> @param imtv - Master table version number
!> @param iogce - Originating center
!> @param imtvl - Local table version number
!> @param tbltyp - Table type:
!> - 'TableB' = Table B
!> - 'TableD' = Table D
!> - 'CodeFlag' = Code and Flag Tables
!> @param stdfil - Standard master table path/filename
!> @param locfil - Local master table path/filename
!>
!> @author Ator @date 2017-10-16
subroutine mtfnam ( imt, imtv, iogce, imtvl, tbltyp, stdfil, locfil )

  implicit none

  integer, intent(in) :: imt, imtv, iogce, imtvl
  integer iprt, lun1, lun2, lmtd, ltbt, isize

  character*(*), intent(in) :: tbltyp
  character*(*), intent(out) :: stdfil, locfil

  character*16 tbltyp2
  character*20 fmtf
  character*240 mtdir
  character*128 bort_str

  logical found

  common /quiet/ iprt
  common /mstinf/ lun1, lun2, lmtd, mtdir

  call strsuc ( tbltyp, tbltyp2, ltbt )

  ! Determine the standard master table path/filename.

  if ( ( imt == 0 ) .and. ( imtv <= 13 ) ) then
    ! For master table 0, version 13 is a superset of all earlier versions.
    stdfil = mtdir(1:lmtd) // '/bufrtab.' // tbltyp2(1:ltbt) // '_STD_0_13'
  else
    write ( fmtf, '(A,I1,A,I1,A)' ) '(4A,I', isize(imt), ',A,I', isize(imtv), ')'
    write ( stdfil, fmtf ) mtdir(1:lmtd), '/bufrtab.', tbltyp2(1:ltbt), '_STD_', imt, '_', imtv
  endif
  if ( iprt >= 2 ) then
    call errwrt('Standard ' // tbltyp2(1:ltbt) // ':')
    call errwrt(stdfil)
  endif
  inquire ( file = stdfil, exist = found )
  if ( .not. found ) then
    bort_str = 'BUFRLIB: MTFNAM - COULD NOT FIND STANDARD FILE:'
    call bort2(bort_str, stdfil)
  endif

  ! Now determine the local master table path/filename.

  ! Use the local table corresponding to the originating center and local table version number, if such a table exists.
  ! Otherwise use the local table from NCEP.

  write ( fmtf, '(A,I1,A,I1,A,I1,A)' ) '(4A,I', isize(imt), ',A,I', isize(iogce), ',A,I',  isize(imtvl), ')'
  write ( locfil, fmtf ) mtdir(1:lmtd), '/bufrtab.', tbltyp2(1:ltbt), '_LOC_', imt, '_', iogce, '_', imtvl
  if ( iprt >= 2 ) then
    call errwrt('Local ' // tbltyp2(1:ltbt) // ':')
    call errwrt(locfil)
  endif
  inquire ( file = locfil, exist = found )
  if ( .not. found ) then
    ! Use the local table from NCEP.
    locfil = mtdir(1:lmtd) // '/bufrtab.' // tbltyp2(1:ltbt) // '_LOC_0_7_1'
    if ( iprt >= 2 ) then
      call errwrt('Local ' // tbltyp2(1:ltbt) // ' not found, so using:')
      call errwrt(locfil)
    endif
    inquire ( file = locfil, exist = found )
    if ( .not. found ) then
      bort_str = 'BUFRLIB: MTFNAM - COULD NOT FIND LOCAL FILE:'
      call bort2(bort_str, locfil)
    endif
  endif

  return
end subroutine mtfnam

!> Check the most recent BUFR message that was read via a call to one of the [message-reading subroutines](@ref hierarchy)
!> and determine whether the appropriate corresponding BUFR master tables have already been read into internal memory.
!>
!> If not, then open the appropriate master BUFR tables on the
!> local file system and read the contents into internal
!> memory, clearing any previous master BUFR table information that
!> may have previously been stored there.
!>
!> @param lun - File ID
!> @returns ireadmt - Flag indicating whether new master BUFR tables needed to be read into internal memory:
!> - 0 = No
!> - 1 = Yes
!>
!> Information about the location of master BUFR tables on the
!> local file system is obtained from the most recent call to
!> subroutine mtinfo(), or else from subroutine bfrini() if
!> subroutine mtinfo() was never called, and in which case Fortran
!> logical unit numbers 98 and 99 will be used by this function
!> for opening and reading master BUFR table files.
!>
!> @author J. Ator @date 2009-03-23
integer function ireadmt ( lun ) result ( iret )

  use bufrlib

  use modv_vars, only: maxnc, maxcd, mxmtbb, mxmtbd

  use moda_mstabs
  use moda_bitbuf
  use moda_rdmtb
  use moda_sc3bfr
  use moda_s3list
  use moda_tablef

  implicit none

  integer, intent(in) :: lun
  integer iprt, lun1, lun2, lmtd, lmt, lmtv, logce, lmtvl, imt, imtv, iogce, imtvl, ii, jj, idx, ncds3, ier, &
    ibmt, ibmtv, ibogce, ibltv, idmt, idmtv, idogce, idltv, iupbs01, ifxy, istdesc

  character*(*), parameter :: bort_str1 = 'BUFRLIB: IREADMT - COULD NOT OPEN STANDARD FILE:'
  character*(*), parameter :: bort_str2 = 'BUFRLIB: IREADMT - COULD NOT OPEN LOCAL FILE:'
  character*275 stdfil,locfil
  character*240 mtdir

  logical allstd

  common /quiet/ iprt
  common /mstinf/ lun1, lun2, lmtd, mtdir

  ! Initializing the following value ensures that new master tables are read during the first call to this subroutine.

  data lmt /-99/

  save lmt, lmtv, logce, lmtvl

  iret = 0

  ! Unpack some Section 1 information from the message that was most recently read.

  imt  = iupbs01 ( mbay(1,lun), 'BMT' )
  imtv = iupbs01 ( mbay(1,lun), 'MTV' )
  iogce = iupbs01 ( mbay(1,lun), 'OGCE' )
  imtvl = iupbs01 ( mbay(1,lun), 'MTVL' )

  ! Compare the master table and master table version numbers from this message to those from the message that was
  ! processed during the previous call to this subroutine.

  if (  ( imt /= lmt )  .or.  ( ( imt /= 0 ) .and. ( imtv /= lmtv ) )  .or. &
          ( ( imt == 0 ) .and. ( imtv /= lmtv ) .and. ( ( imtv > 13 ) .or. ( lmtv > 13 ) ) )  )  then
    ! Either the master table number has changed
    !       OR
    ! The master table number hasn't changed, but it isn't 0, and the table version number has changed
    !       OR
    ! The master table number hasn't changed and is 0, but the table version number has changed, and at least one of the
    ! table version numbers (i.e. the current or the previous) is greater than 13 (which is the last version that was a
    ! superset of all earlier versions of master table 0!)

    ! In any of these cases, we need to read in new tables!
    iret = 1

  else

    ! Unpack the list of Section 3 descriptors from the message and determine if any of them are local descriptors.
    call upds3 ( mbay(1,lun), maxnc, cds3, ncds3 )
    ii = 1
    allstd = .true.
    do while ( (allstd) .and. (ii<=ncds3) )
      if ( istdesc(ifxy(cds3(ii))) == 0 ) then
        allstd = .false.
      else
        ii = ii + 1
      endif
    enddo

    ! If there was at least one local (i.e. non-standard) descriptor, and if either the originating center or local table
    ! version number are different than those from the message that was processed during the previous call to this subroutine,
    ! then we need to read in new tables.
    if ( ( .not. allstd ) .and. ( ( iogce /= logce ) .or. ( imtvl /= lmtvl ) ) ) iret = 1

  endif

  if ( iret == 0 ) return

  lmt  = imt
  lmtv = imtv
  logce = iogce
  lmtvl = imtvl

  if ( iprt >= 2 ) then
    call errwrt(' ')
    call errwrt('+++++++++++++++++++++++++++++++++++++++++++++++++')
    call errwrt('BUFRLIB: IREADMT - OPENING/READING MASTER TABLES')
  endif

  if ( isc3(lun) /= 0 ) then

    ! Locate and open the master Table B files.  There should be one file of standard descriptors and one file of local
    ! descriptors.
    call mtfnam ( imt, imtv, iogce, imtvl, 'TableB', stdfil, locfil )
    open ( unit = lun1, file = stdfil, iostat = ier )
    if ( ier /= 0 ) call bort2(bort_str1, stdfil)
    open ( unit = lun2, file = locfil, iostat = ier )
    if ( ier /= 0 ) call bort2(bort_str2, locfil)

    ! Read the master Table B files.
    call rdmtbb ( lun1, lun2, mxmtbb, ibmt, ibmtv, ibogce, ibltv, nmtb, ibfxyn, cbscl, cbsref, cbbw, &
      cbunit, cbmnem, cmdscb, cbelem )

    ! Close the master Table B files.
    close ( unit = lun1 )
    close ( unit = lun2 )

    ! Locate and open the master Table D files.  There should be one file of standard descriptors and one file of local
    ! descriptors.
    call mtfnam ( imt, imtv, iogce, imtvl, 'TableD', stdfil, locfil )
    open ( unit = lun1, file = stdfil, iostat = ier )
    if ( ier /= 0 ) call bort2(bort_str1, stdfil)
    open ( unit = lun2, file = locfil, iostat = ier )
    if ( ier /= 0 ) call bort2(bort_str2, locfil)

    ! Read the master Table D files.
    call rdmtbd ( lun1, lun2, mxmtbd, maxcd, idmt, idmtv, idogce, idltv, nmtd, idfxyn, cdmnem, cmdscd, cdseq, &
      ndelem, iefxyn, ceelem )
    do ii = 1, nmtd
      do jj = 1, ndelem(ii)
        idx = icvidx_c ( ii-1, jj-1, maxcd ) + 1
        idefxy(idx) = iefxyn(ii,jj)
      enddo
    enddo

    ! Close the master Table D files.
    close ( unit = lun1 )
    close ( unit = lun2 )

    ! Copy master table B and D information into internal C arrays.
    call cpmstabs_c ( nmtb, ibfxyn, cbscl, cbsref, cbbw, cbunit, cbmnem, cbelem, nmtd, idfxyn, cdseq, cdmnem, &
      ndelem, idefxy, maxcd )
  endif

  if ( cdmf == 'Y' ) then

    ! Locate and open the master code and flag table files.  There should be one file corresponding to the standard Table B
    ! descriptors, and one file corresponding to the local Table B descriptors.
    call mtfnam ( imt, imtv, iogce, imtvl, 'CodeFlag', stdfil, locfil )
    open ( unit = lun1, file = stdfil, iostat = ier )
    if ( ier /= 0 ) call bort2(bort_str1, stdfil)
    open ( unit = lun2, file = locfil, iostat = ier )
    if ( ier /= 0 ) call bort2(bort_str2, locfil)

    ! Read the master code and flag table files.
    call rdmtbf ( lun1, lun2 )

    ! Close the master code and flag table files.
    close ( unit = lun1 )
    close ( unit = lun2 )
  endif

  if ( iprt >= 2 ) then
    call errwrt('+++++++++++++++++++++++++++++++++++++++++++++++++')
    call errwrt(' ')
  endif

  return
end function ireadmt

!> Read master Table B information from two separate ASCII files (one standard and one local) and then merge the
!> output into a single set of arrays.
!>
!> Each of the two ASCII files must already be individually sorted in ascending order with respect to the FXY numbers.
!>
!> @param lunstb - Fortran logical unit number for ASCII file containing standard Table B information
!> @param lunltb - Fortran logical unit number for ASCII file containing local Table B information
!> @param mxmtbb - Dimensioned size (in integers) of merged output arrays; used by the subroutine to ensure that it
!> doesn't overflow these arrays
!> @param imt - Master table
!>  - This value is read from both ASCII files and must be identical between them
!> @param imtv - Version number of master table
!>  - This value is read from the standard ASCII file
!> @param iogce - Originating center
!>  - This value is read from the local ASCII file
!> @param iltv - Version number of local table
!>  - This value is read from the local ASCII file
!> @param nmtbb - Number of entries in merged output arrays
!> @param imfxyn - Merged array containing WMO bit-wise representations of FXY numbers
!> @param cmscl - Merged array containing scale factors
!> @param cmsref - Merged array containing reference values
!> @param cmbw - Merged array containing bit widths
!> @param cmunit - Merged array containing units
!> @param cmmnem - Merged array containing mnemonics
!> @param cmdsc - Merged array containing descriptor codes
!> @param cmelem - Merged array containing element names
!>
!> @author J. Ator @date 2007-01-19
subroutine rdmtbb ( lunstb, lunltb, mxmtbb, imt, imtv, iogce, iltv, nmtbb, imfxyn, cmscl, cmsref, cmbw, &
  cmunit, cmmnem, cmdsc, cmelem )

  implicit none

  integer, intent(in) :: lunstb, lunltb, mxmtbb
  integer, intent(out) :: imt, imtv, iogce, iltv, nmtbb, imfxyn(*)
  integer isfxyn, ilfxyn, iers, ierl

  character, intent(out) :: cmelem(120,*), cmunit(24,*), cmsref(12,*), cmmnem(8,*), cmscl(4,*), cmbw(4,*), cmdsc(*)*4
  character*200 stline, ltline
  character*128 bort_str
  character*6 cmatch, adn30

  ! Read and parse the header lines of both files.

  call gettbh ( lunstb, lunltb, 'B', imt, imtv, iogce, iltv )

  ! Read through the remainder of both files, merging the contents into a unified set of master Table B arrays.

  nmtbb = 0
  call getntbe ( lunstb, isfxyn, stline, iers )
  call getntbe ( lunltb, ilfxyn, ltline, ierl )
  do while ( ( iers == 0 ) .or. ( ierl == 0 ) )
    if ( ( iers == 0 ) .and. ( ierl == 0 ) ) then
      if ( isfxyn == ilfxyn ) then
        cmatch = adn30 ( isfxyn, 6 )
        write(bort_str,'("BUFRLIB: RDMTBB - STANDARD AND LOCAL '// &
          'TABLE B FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)') cmatch(1:1), '-', cmatch(2:3), '-', cmatch(4:6)
        call bort(bort_str)
      else if ( isfxyn < ilfxyn ) then
        call sntbbe ( isfxyn, stline, mxmtbb, nmtbb, imfxyn, cmscl, cmsref, cmbw, cmunit, cmmnem, cmdsc, cmelem )
        call getntbe ( lunstb, isfxyn, stline, iers )
      else
        call sntbbe ( ilfxyn, ltline, mxmtbb, nmtbb, imfxyn, cmscl, cmsref, cmbw, cmunit, cmmnem, cmdsc, cmelem )
        call getntbe ( lunltb, ilfxyn, ltline, ierl )
      endif
    else if ( iers == 0 ) then
      call sntbbe ( isfxyn, stline, mxmtbb, nmtbb, imfxyn, cmscl, cmsref, cmbw, cmunit, cmmnem, cmdsc, cmelem )
      call getntbe ( lunstb, isfxyn, stline, iers )
    else
      call sntbbe ( ilfxyn, ltline, mxmtbb, nmtbb, imfxyn, cmscl, cmsref, cmbw, cmunit, cmmnem, cmdsc, cmelem )
      call getntbe ( lunltb, ilfxyn, ltline, ierl )
    endif
  enddo

  return
end subroutine rdmtbb

!> Read master Table D information from two separate ASCII files (one standard and one local) and then merge the
!> output into a single set of arrays.
!>
!> Each of the two ASCII files must already be individually sorted in ascending order with respect to the FXY numbers.
!>
!> @param lunstd - Fortran logical unit number for ASCII file containing standard Table D information
!> @param lunltd - Fortran logical unit number for ASCII file containing local Table D information
!> @param mxmtbd - Dimensioned size (in integers) of merged output arrays; used by the subroutine to ensure that it
!> doesn't overflow these arrays
!> @param mxelem - Maximum number of elements to be stored per Table D sequence within merged output arrays; used
!> by the subroutine to ensure that it doesn't overflow these arrays
!> @param imt - Master table
!>  - This value is read from both ASCII files and must be identical between them
!> @param imtv - Version number of master table
!>  - This value is read from the standard ASCII file
!> @param iogce - Originating center
!>  - This value is read from the local ASCII file
!> @param iltv - Version number of local table
!>  - This value is read from the local ASCII file
!> @param nmtbd - Number of entries in merged output arrays
!> @param imfxyn - Merged array containing WMO bit-wise representations of FXY numbers
!> @param cmmnem - Merged array containing mnemonics
!> @param cmdsc - Merged array containing descriptor codes
!> @param cmseq - Merged array containing sequence names
!> @param nmelem - Merged array containing number of elements stored for each sequence
!> @param iefxyn - Merged array containing WMO bit-wise representations of element FXY numbers
!> @param ceelem - Merged array containing element names
!>
!> @author J. Ator @date 2007-01-19
subroutine rdmtbd ( lunstd, lunltd, mxmtbd, mxelem, imt, imtv, iogce, iltv, nmtbd, imfxyn, cmmnem, cmdsc, cmseq, &
  nmelem, iefxyn, ceelem )

  implicit none

  integer, intent(in) :: lunstd, lunltd, mxmtbd, mxelem
  integer, intent(out) :: imt, imtv, iogce, iltv, nmtbd, imfxyn(*), nmelem(*), iefxyn(mxmtbd,mxelem)
  integer isfxyn, ilfxyn, iers, ierl

  character, intent(out) :: cmseq(120,*), cmmnem(8,*), cmdsc(*)*4, ceelem(mxmtbd,mxelem)*120

  character*200 stline, ltline
  character*128 bort_str
  character*6 cmatch, adn30

  ! Read and parse the header lines of both files.

  call gettbh ( lunstd, lunltd, 'D', imt, imtv, iogce, iltv )

  ! Read through the remainder of both files, merging the contents into a unified set of master Table D arrays.

  nmtbd = 0
  call getntbe ( lunstd, isfxyn, stline, iers )
  call getntbe ( lunltd, ilfxyn, ltline, ierl )
  do while ( ( iers == 0 ) .or. ( ierl == 0 ) )
    if ( ( iers == 0 ) .and. ( ierl == 0 ) ) then
      if ( isfxyn == ilfxyn ) then
        cmatch = adn30 ( isfxyn, 6 )
        write(bort_str,'("BUFRLIB: RDMTBD - STANDARD AND LOCAL '// &
          'TABLE D FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)') cmatch(1:1), '-', cmatch(2:3), '-', cmatch(4:6)
        call bort(bort_str)
      else if ( isfxyn < ilfxyn ) then
        call sntbde ( lunstd, isfxyn, stline, mxmtbd, mxelem, nmtbd, imfxyn, cmmnem, cmdsc, cmseq, nmelem, iefxyn, ceelem )
        call getntbe ( lunstd, isfxyn, stline, iers )
      else
        call sntbde ( lunltd, ilfxyn, ltline, mxmtbd, mxelem, nmtbd, imfxyn, cmmnem, cmdsc, cmseq, nmelem, iefxyn, ceelem )
        call getntbe ( lunltd, ilfxyn, ltline, ierl )
      endif
    else if ( iers == 0 ) then
      call sntbde ( lunstd, isfxyn, stline, mxmtbd, mxelem, nmtbd, imfxyn, cmmnem, cmdsc, cmseq, nmelem, iefxyn, ceelem )
      call getntbe ( lunstd, isfxyn, stline, iers )
    else
      call sntbde ( lunltd, ilfxyn, ltline, mxmtbd, mxelem, nmtbd, imfxyn, cmmnem, cmdsc, cmseq, nmelem, iefxyn, ceelem )
      call getntbe ( lunltd, ilfxyn, ltline, ierl )
    endif
  enddo

  return
end subroutine rdmtbd

!> Read master Code/Flag table information from two separate ASCII files (one standard and one local) and then merge the
!> output into a single set of arrays.
!>
!> Each of the two ASCII files must already be individually sorted in ascending order with respect to the FXY numbers.
!>
!> @param lunstf - Fortran logical unit number for ASCII file containing standard Code/Flag table information
!> @param lunltf - Fortran logical unit number for ASCII file containing local Code/Flag table information
!>
!> @author J. Ator @date 2017-10-17
subroutine rdmtbf ( lunstf, lunltf )

  use bufrlib

  implicit none

  integer, intent(in) :: lunstf, lunltf
  integer imt, imtv, iogce, iltv, isfxyn, ilfxyn, iers, ierl

  character*160 stline, ltline
  character*128 bort_str
  character*6 cmatch, adn30

  ! Initialize the internal memory structure, including allocating space for it in case this hasn't already been done.

  call inittbf_c

  ! Read and parse the header lines of both files.

  call gettbh ( lunstf, lunltf, 'F', imt, imtv, iogce, iltv )

  ! Read through the remainder of both files, merging the contents into a unified internal memory structure.

  call getntbe ( lunstf, isfxyn, stline, iers )
  call getntbe ( lunltf, ilfxyn, ltline, ierl )
  do while ( ( iers == 0 ) .or. ( ierl == 0 ) )
    if ( ( iers == 0 ) .and. ( ierl == 0 ) ) then
      if ( isfxyn == ilfxyn ) then
        cmatch = adn30 ( isfxyn, 6 )
        write(bort_str,'("BUFRLIB: RDMTBF - STANDARD AND LOCAL '// &
          'CODE/FLAG TABLE FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)') cmatch(1:1), '-', cmatch(2:3), '-', cmatch(4:6)
        call bort(bort_str)
      else if ( isfxyn < ilfxyn ) then
        call sntbfe ( lunstf, isfxyn )
        call getntbe ( lunstf, isfxyn, stline, iers )
      else
        call sntbfe ( lunltf, ilfxyn )
        call getntbe ( lunltf, ilfxyn, ltline, ierl )
      endif
    else if ( iers == 0 ) then
      call sntbfe ( lunstf, isfxyn )
      call getntbe ( lunstf, isfxyn, stline, iers )
    else
      call sntbfe ( lunltf, ilfxyn )
      call getntbe ( lunltf, ilfxyn, ltline, ierl )
    endif
  enddo

  ! Sort the contents of the internal memory structure.

  call sorttbf_c

  return
end subroutine rdmtbf

!> Store an entry that was previously read from an ASCII master Table B file into a set of merged Fortran arrays.
!>
!> @param ifxyn - WMO bit-wise representation of FXY number
!> @param line - Table B entry
!> @param mxmtbb - Dimensioned size (in integers) of merged output arrays; used by the subroutine
!> to ensure that it doesn't overflow these arrays
!> @param nmtbb - Number of entries in merged output arrays
!> @param imfxyn - Merged array containing WMO bit-wise representations of FXY numbers
!> @param cmscl - Merged array containing scale factors
!> @param cmsref - Merged array containing reference values
!> @param cmbw  - Merged array containing bit widths
!> @param cmunit - Merged array containing units
!> @param cmmnem - Merged array containing mnemonics
!> @param cmdsc - Merged array containing descriptor codes
!> @param cmelem - Merged array containing element names
!>
!> @author J. Ator @date 2007-01-19
subroutine sntbbe ( ifxyn, line, mxmtbb, nmtbb, imfxyn, cmscl, cmsref, cmbw, cmunit, cmmnem, cmdsc, cmelem )

  implicit none

  integer, intent(in) :: ifxyn, mxmtbb
  integer, intent(out) :: nmtbb, imfxyn(*)
  integer ntag, ii, nemock

  character, intent(out) :: cmelem(120,*), cmunit(24,*), cmsref(12,*), cmmnem(8,*), cmscl(4,*), cmbw(4,*), cmdsc(*)*4
  character*(*), intent(in) :: line
  character*(*), parameter :: bort_str1_head = 'BUFRLIB: SNTBBE - TABLE B ENTRY FOR ELEMENT DESCRIPTOR: '
  character*200 tags(10), wktag
  character*128 bort_str1, bort_str2

  if ( nmtbb >= mxmtbb ) call bort('BUFRLIB: SNTBBE - OVERFLOW OF MERGED ARRAYS')
  nmtbb = nmtbb + 1

  ! Store the FXY number.  This is the element descriptor.

  imfxyn ( nmtbb ) = ifxyn

  ! Parse the table entry.

  call parstr ( line, tags, 10, ntag, '|', .false. )
  if ( ntag < 4 ) then
    call sntbestr(bort_str1_head, ifxyn, bort_str1)
    bort_str2 = '                  HAS TOO FEW FIELDS'
    call bort2(bort_str1, bort_str2)
  endif

  ! Scale factor.

  tags(2) = adjustl( tags(2) )
  if ( tags(2) == ' ' ) then
    call sntbestr(bort_str1_head, ifxyn, bort_str1)
    bort_str2 = '                  HAS MISSING SCALE FACTOR'
    call bort2(bort_str1, bort_str2)
  endif
  tags(2)(1:4) = adjustr( tags(2)(1:4) )
  do ii = 1, 4
    cmscl ( ii, nmtbb ) = tags(2)(II:II)
  enddo

  ! Reference value.

  tags(3) = adjustl( tags(3) )
  if ( tags(3) == ' ' ) then
    call sntbestr(bort_str1_head, ifxyn, bort_str1)
    bort_str2 = '                  HAS MISSING REFERENCE VALUE'
    call bort2(bort_str1, bort_str2)
  endif
  tags(3)(1:12) = adjustr( tags(3)(1:12) )
  do ii = 1, 12
    cmsref ( ii, nmtbb ) = tags(3)(II:II)
  enddo

  ! Bit width.

  tags(4) = adjustl( tags(4) )
  if ( tags(4) == ' ' ) then
    call sntbestr(bort_str1_head, ifxyn, bort_str1)
    bort_str2 = '                  HAS MISSING BIT WIDTH'
    call bort2(bort_str1, bort_str2)
  endif
  tags(4)(1:4) = adjustr( tags(4)(1:4) )
  do ii = 1, 4
    cmbw ( ii, nmtbb ) = tags(4)(II:II)
  end do

  ! Units.  Note that this field is allowed to be blank.

  if ( ntag > 4 ) then
    tags(5) = adjustl( tags(5) )
    do ii = 1, 24
      cmunit ( ii, nmtbb ) = tags(5)(II:II)
    enddo
  else
    do ii = 1, 24
      cmunit ( ii, nmtbb ) = ' '
    enddo
  endif

  ! Comment (additional) fields.  Any of these fields may be blank.

  cmdsc ( nmtbb ) = ' '
  do ii = 1, 8
    cmmnem ( ii, nmtbb ) = ' '
  enddo
  do ii = 1, 120
    cmelem ( ii, nmtbb ) = ' '
  enddo
  if ( ntag > 5 ) then
    wktag = tags(6)
    call parstr ( wktag, tags, 10, ntag, ';', .false. )
    if ( ntag > 0 ) then
      ! The first additional field contains the mnemonic.
      tags(1) = adjustl( tags(1) )
      ! If there is a mnemonic, then make sure it's legal.
      if ( ( tags(1) /= ' ' ) .and. ( nemock ( tags(1) ) /= 0 ) ) then
        call sntbestr(bort_str1_head, ifxyn, bort_str1)
        bort_str2 = '                  HAS ILLEGAL MNEMONIC'
        call bort2(bort_str1, bort_str2)
      endif
      do ii = 1, 8
        cmmnem ( ii, nmtbb ) = tags(1)(ii:ii)
      enddo
    endif
    if ( ntag > 1 ) then
      ! The second additional field contains descriptor codes.
      tags(2) = adjustl( tags(2) )
      cmdsc ( nmtbb ) = tags(2)(1:4)
    endif
    if ( ntag > 2 ) then
      ! The third additional field contains the element name.
      tags(3) = adjustl( tags(3) )
      do ii = 1, 120
        cmelem ( ii, nmtbb ) = tags(3)(II:II)
      enddo
    endif
  endif

  return
end subroutine sntbbe

!> Store the first line of an entry that was previously read from an ASCII master Table D file into a set of
!> merged Fortran arrays, then read and store all remaining lines of that same entry into the same merged Fortran arrays.
!>
!> @param lunt - Fortran logical unit number for ASCII file containing Table D information
!> @param ifxyn - WMO bit-wise representation of FXY number
!> @param line - First line of Table D entry
!> @param mxmtbd - Dimensioned size (in integers) of merged output arrays; used by the subroutine to ensure
!> that it doesn't overflow these arrays
!> @param mxelem - Maximum number of elements to be stored per Table D sequence within merged output arrays;
!> used by the subroutine to ensure that it doesn't overflow these arrays
!> @param nmtbd - Number of entries in merged output arrays
!> @param imfxyn - Merged array containing WMO bit-wise representations of FXY numbers
!> @param cmmnem - Merged array containing mnemonics
!> @param cmdsc - Merged array containing descriptor codes
!> @param cmseq - Merged array containing sequence names
!> @param nmelem - Merged array containing number of elements stored for each sequence
!> @param iefxyn - Merged array containing WMO bit-wise representations of element FXY numbers
!> @param ceelem - Merged array containing element names
!>
!> @author J. Ator @date 2007-01-19
subroutine sntbde ( lunt, ifxyn, line, mxmtbd, mxelem, nmtbd, imfxyn, cmmnem, cmdsc, cmseq, nmelem, iefxyn, ceelem )

  implicit none

  integer, intent(in) :: lunt, ifxyn, mxmtbd, mxelem
  integer, intent(out) :: nmtbd, imfxyn(*), nmelem(*), iefxyn(mxmtbd,mxelem)
  integer ii, ipt, ntag, nelem, nemock, ifxy, igetfxy, igetntbl

  character*(*), intent(in) :: line
  character*(*), parameter :: bort_str1_head = 'BUFRLIB:  SNTBDE - TABLE D ENTRY FOR SEQUENCE DESCRIPTOR: '
  character, intent(out) :: cmseq(120,*), cmmnem(8,*), cmdsc(*)*4, ceelem(mxmtbd,mxelem)*120
  character*200 tags(10), cline
  character*128 bort_str1, bort_str2
  character*6 adsc

  logical done

  if ( nmtbd >= mxmtbd ) call bort('BUFRLIB: SNTBDE - OVERFLOW OF MERGED ARRAYS')
  nmtbd = nmtbd + 1

  ! Store the FXY number.  This is the sequence descriptor.

  imfxyn ( nmtbd ) = ifxyn

  ! Is there any other information within the first line of the table entry?  If so, it follows a "|" separator.

  do ii = 1, 8
    cmmnem ( ii, nmtbd ) = ' '
  enddo
  cmdsc ( nmtbd ) = ' '
  do ii = 1, 120
    cmseq ( ii, nmtbd ) = ' '
  enddo
  ipt = index ( line, '|' )
  if ( ipt /= 0 ) then
    ! Parse the rest of the line.  Any of the fields may be blank.
    call parstr ( line(ipt+1:), tags, 10, ntag, ';', .false. )
    if ( ntag > 0 ) then
      ! The first additional field contains the mnemonic.
      tags(1) = adjustl( tags(1) )
      ! If there is a mnemonic, then make sure it's legal.
      if ( ( tags(1) /= ' ' ) .and. ( nemock ( tags(1) ) /= 0 ) ) then
        call sntbestr(bort_str1_head, ifxyn, bort_str1)
        bort_str2 = '                  HAS ILLEGAL MNEMONIC'
        call bort2(bort_str1, bort_str2)
      endif
      do ii = 1, 8
        cmmnem ( ii, nmtbd ) = tags(1)(ii:ii)
      enddo
    endif
    if ( ntag > 1 ) then
      ! The second additional field contains descriptor codes.
      tags(2) = adjustl( tags(2) )
      cmdsc ( nmtbd ) = tags(2)(1:4)
    endif
    if ( ntag > 2 ) then
      ! The third additional field contains the sequence name.
      tags(3) = adjustl( tags(3) )
      do ii = 1, 120
        cmseq ( ii, nmtbd ) = tags(3)(ii:ii)
      enddO
    endif
  endif

  ! Now, read and parse all remaining lines from this table entry. Each line should contain an element descriptor for
  ! the sequence represented by the current sequence descriptor.

  nelem = 0
  done = .false.
  do while ( .not. done )
    if ( igetntbl ( lunt, cline ) /= 0 ) then
      call sntbestr(bort_str1_head, ifxyn, bort_str1)
      bort_str2 = '                  IS INCOMPLETE'
      call bort2(bort_str1, bort_str2)
    endif
    call parstr ( cline, tags, 10, ntag, '|', .false. )
    if ( ntag < 2 ) then
      call sntbestr(bort_str1_head, ifxyn, bort_str1)
      bort_str2 = '                  HAS BAD ELEMENT CARD'
      call bort2(bort_str1, bort_str2)
    endif
    ! The second field contains the FXY number for this element.
    if ( igetfxy ( tags(2), adsc ) /= 0 ) then
      call sntbestr(bort_str1_head, ifxyn, bort_str1)
      bort_str2 = '                  HAS BAD OR MISSING ELEMENT FXY NUMBER'
      call bort2(bort_str1, bort_str2)
    endif
    if ( nelem >= mxelem ) CALL BORT('BUFRLIB: SNTBDE - OVERFLOW OF MERGED ARRAYS')
    nelem = nelem + 1
    iefxyn ( nmtbd, nelem ) = ifxy ( adsc )
    ! The third field (if it exists) contains the element name.
    if ( ntag > 2 ) then
      tags(3) = adjustl( tags(3) )
      ceelem ( nmtbd, nelem ) = tags(3)(1:120)
    else
      ceelem ( nmtbd, nelem ) = ' '
    endif
    ! Is this the last line for this table entry?
    if ( index ( tags(2), ' >' ) == 0 ) done = .true.
  enddo
  nmelem ( nmtbd ) = nelem

  return
end subroutine sntbde

!> Read an entire entry from a previously-opened ASCII master Code/Flag table file, then store the information
!> into an internal memory structure.
!>
!> @param lunt - Fortran logical unit number for ASCII file containing Code/Flag table information
!> @param ifxyn - WMO bit-wise representation of FXY number
!>
!> @author J. Ator @date 2017-11-02
subroutine sntbfe ( lunt, ifxyn )

  use bufrlib

  implicit none

  integer, intent(in) :: lunt, ifxyn
  integer idfxy(10), idval(25), nidfxy, nidval, ntag, ii, jj, ival, ier, ipt, lt3, ifxy, igetfxy, igetntbl

  character*160 cline, tags(4), cdstr(2), adsc(10), cval(25)
  character*(*), parameter :: bort_str1_head = 'BUFRLIB: SNTBFE - TABLE F ENTRY FOR ELEMENT DESCRIPTOR: '
  character*128 bort_str1, bort_str2
  character*6 cdsc

  logical done, lstnblk

  ! We already have the FXY number.  Now we need to read and parse all of the remaining lines from the table entry for this
  ! FXY number.  The information for each individual code figure or bit number will then be stored as a separate entry within
  ! the internal memory structure.

  done = .false.
  nidfxy = 0
  nidval = 0

  do while ( .not. done )

    if ( igetntbl ( lunt, cline ) /= 0 ) then
      call sntbestr(bort_str1_head, ifxyn, bort_str1)
      bort_str2 = '                  IS INCOMPLETE'
      call bort2(bort_str1, bort_str2)
    endif

    call parstr ( cline, tags, 4, ntag, '|', .false. )
    if ( ( ntag < 2 ) .or. ( ntag > 3 ) ) then
      call sntbestr(bort_str1_head, ifxyn, bort_str1)
      bort_str2 = '                  HAS BAD CARD'
      call bort2(bort_str1, bort_str2)
    endif

    if ( ntag == 2 ) then

      ! This line contains a list of dependencies.

      call parstr ( tags(2), cdstr, 2, ntag, '=', .false. )
      if ( ntag /= 2 ) then
        call sntbestr(bort_str1_head, ifxyn, bort_str1)
        bort_str2 = '           HAS BAD DEPENDENCY CARD'
        call bort2(bort_str1, bort_str2)
      endif
      ! Parse the list of FXY numbers.
      call parstr ( cdstr(1), adsc, 10, nidfxy, ',', .false. )
      if ( ( nidfxy == 0 ) .or. ( ( nidfxy == 1 ) .and. ( adsc(1) == ' ' ) ) ) then
        call sntbestr(bort_str1_head, ifxyn, bort_str1)
        bort_str2 = '        HAS BAD DEPENDENCY LIST (FXY)'
        call bort2(bort_str1, bort_str2)
      endif
      do ii = 1, nidfxy
        if ( igetfxy ( adsc(ii), cdsc ) /= 0 ) then
          call sntbestr(bort_str1_head, ifxyn, bort_str1)
          bort_str2 = '        HAS BAD DEPENDENCY (FXY)'
          call bort2(bort_str1, bort_str2)
        endif
        idfxy(ii) = ifxy( cdsc )
      enddo
      ! Parse the list of values.
      call parstr ( cdstr(2), cval, 25, nidval, ',', .false. )
      if ( ( nidval == 0 ) .or. ( ( nidval == 1 ) .and. ( cval(1) == ' ' ) ) ) then
        call sntbestr(bort_str1_head, ifxyn, bort_str1)
        bort_str2 = '        HAS BAD DEPENDENCY LIST (VAL)'
        call bort2(bort_str1, bort_str2)
      endif
      do ii = 1, nidval
        cval(ii) = adjustl( cval(ii) )
        call strnum ( cval(ii), ival, ier )
        if ( ier /= 0 ) then
          call sntbestr(bort_str1_head, ifxyn, bort_str1)
          bort_str2 = '        HAS BAD DEPENDENCY (VAL)'
          call bort2(bort_str1, bort_str2)
        endif
        idval(ii) = ival
      enddo

    else

      ! This line contains a value (code figure or bit number) and corresponding meaning.

      ipt = index ( tags(2), ' >' )
      if ( ipt == 0 ) then
      ! This is the last line for this table entry.
        done = .true.
      else
        tags(2)(ipt+1:ipt+1) = ' '
      endif
      tags(2) = adjustl( tags(2) )
      call strnum ( tags(2), ival, ier )
      ! Find the last non-blank character in the meaning string.
      tags(3) = adjustl( tags(3) )
      lt3 = len(tags(3))
      lstnblk = .false.
      do while ( ( lt3 > 0 ) .and. ( .not. lstnblk ) )
        if ( tags(3)(lt3:lt3) /= ' ' ) then
          lstnblk = .true.
        else
          lt3 = lt3 - 1
        endif
      enddo
      ! Store the information for this value within the internal memory structure.
      if ( ( nidfxy == 0 ) .and. ( nidval == 0 ) ) then
        call strtbfe_c ( ifxyn, ival, tags(3), lt3, -1, -1 )
      else
        do ii = 1, nidfxy
          do jj = 1, nidval
            call strtbfe_c ( ifxyn, ival, tags(3), lt3, idfxy(ii), idval(jj) )
          enddo
        enddo
      endif

    endif

  enddo

  return
end subroutine sntbfe

!> Generate an error-reporting string containing an FXY number
!>
!> @param hestr - Head portion of error-reporting string
!> @param ifxyn - WMO bit-wise representation of FXY number
!> @param estr - Error-reporting string
!>
!> @author J. Ator @date 2024-04-15
subroutine sntbestr ( hestr, ifxyn, estr )

  implicit none

  character*(*), intent(in) :: hestr
  character*(*), intent(out) :: estr
  character*6 adn30, clemon

  integer, intent(in) :: ifxyn

  clemon = adn30 ( ifxyn, 6 )
  estr = hestr // clemon(1:1) // '-' // clemon(2:3) // '-' // clemon(4:6)

  return
end subroutine sntbestr

!> Read the next line from an ASCII master table B, table D or Code/Flag table file, ignoring any blank lines or comment
!> lines in the process.
!>
!> @param lunt - Fortran logical unit number for ASCII file containing table information
!> @param line - Next non-blank, non-comment line that was read from lunt
!> @returns igetntbl:
!>    -  0 = normal return
!>    - -1 = end-of-file encountered while reading from lunt
!>
!> @author J. Ator @date 2007-01-19
integer function igetntbl ( lunt, line ) result ( iret )

  implicit none

  integer, intent(in) :: lunt
  integer ier

  character*(*), intent(out) :: line

  do while (.true.)
    read ( lunt, '(A)', iostat = ier ) line
    if ( ( ier /= 0 ) .or. ( line(1:3) == 'END' ) ) then
      iret = -1
      return
    endif
    if ( ( line /= ' ' ) .and. ( line(1:1) /= '#' ) ) then
      iret = 0
      return
    endif
  enddo

end function igetntbl

!> Depending on the value of the input flag, either return the next usable scratch Table D index for the
!> current master table, or else reset the index back to its minimum value.
!>
!> @param iflag - Flag:
!>   - if set to 0, then the function will reset the scratch Table D index back to its minimum value
!>   - otherwise, the function will return the next usable scratch Table D index for the current master table
!>
!> @return - igettdi:
!>   - -1 if function was called with iflag = 0
!>   - otherwise, the next usable scratch Table D index for the current master table
!>
!> @author J. Ator @date 2009-03-23
integer function igettdi ( iflag ) result ( iret )

  implicit none

  integer, intent(in) :: iflag
  integer, parameter :: idxmin = 62976   ! = ifxy('354000')
  integer, parameter :: idxmax = 63231   ! = ifxy('354255')
  integer idx

  save idx

  if ( iflag == 0 ) then
    ! Initialize the index to one less than the actual minimum value.  That way, the next normal call will return the
    ! minimum value.
    idx = idxmin - 1
    iret = -1
  else
    idx = idx + 1
    if ( idx > idxmax ) call bort('BUFRLIB: IGETTDI - IDXMAX OVERFLOW')
    iret = idx
  endif

  return
end function igettdi

!> Read the header lines from two separate ASCII files (one standard and one local) containing master table B, table D,
!> or Code/Flag table information.
!>
!> @param luns - Fortran logical unit number for ASCII file containing standard table information
!> @param lunl - Fortran logical unit number for ASCII file containing local table information
!> @param tab - Type of table:
!>    - 'B' = Table B
!>    - 'D' = Table D
!>    - 'F' = Code/Flag table
!> @param imt - Master table
!>    - This value is read from both ASCII files and must be identical between them
!> @param imtv - Version number of master table
!>    - This value is read from the standard ASCII file
!> @param iogce - Originating center
!>    - This value is read from the local ASCII file
!> @param iltv - Version number of local table
!>    - This value is read from the local ASCII file
!>
!> @author J. Ator @date 2007-01-19
subroutine gettbh ( luns, lunl, tab, imt, imtv, iogce, iltv )

  implicit none

  integer, intent(in) :: luns, lunl
  integer, intent(out) :: imt, imtv, iogce, iltv
  integer ntag, imt2, iersn, igetntbl

  character, intent(in) :: tab

  character*128 bort_str
  character*(*), parameter :: bort_str_head = 'BUFRLIB: GETTBH - BAD OR MISSING HEADER WITHIN '
  character*40 header
  character*30 tags(5), label
  character*3 cftyp
  character*2 cttyp

  logical badlabel

  ! Statement function to check for bad header line label
  badlabel ( label ) = ( ( index ( label, cttyp ) == 0 ) .or. ( index ( label, cftyp ) == 0 ) )

  cttyp = tab // ' '

  ! Read and parse the header line of the standard file.

  cftyp = 'STD'
  if ( igetntbl ( luns, header ) /= 0 ) then
    bort_str = bort_str_head // cftyp // ' TABLE ' // tab
    call bort(bort_str)
  endif
  call parstr ( header, tags, 5, ntag, '|', .false. )
  if ( ( ntag < 3 ) .or. ( badlabel ( tags(1) ) ) ) then
    bort_str = bort_str_head // cftyp // ' TABLE ' // tab
    call bort(bort_str)
  endif
  call strnum ( tags(2), imt, iersn )
  call strnum ( tags(3), imtv, iersn )

  ! Read and parse the header line of the local file.

  cftyp = 'LOC'
  if ( igetntbl ( lunl, header ) /= 0 ) then
    bort_str = bort_str_head // cftyp // ' TABLE ' // tab
    call bort(bort_str)
  endif
  call parstr ( header, tags, 5, ntag, '|', .false. )
  if ( ( ntag < 4 ) .or. ( badlabel ( tags(1) ) ) ) then
    bort_str = bort_str_head // cftyp // ' TABLE ' // tab
    call bort(bort_str)
  endif
  call strnum ( tags(2), imt2, iersn )
  call strnum ( tags(3), iogce, iersn )
  call strnum ( tags(4), iltv, iersn )

  ! Verify that both files are for the same master table.

  if ( imt /= imt2 ) then
    write(bort_str,'("BUFRLIB: GETTBH - MASTER TABLE NUMBER MISMATCH BETWEEN STD AND LOC TABLE ",A)') tab
    call bort(bort_str)
  endif

  return
end subroutine gettbh

!> Read the first line of the next entry from the specified ASCII master table B, table D or table F (Code/Flag) file.
!> This line contains, among other things, the FXY number corresponding to this entry.
!>
!> @param lunt - Fortran logical unit number of master table B, table D or Code/Flag table file
!> @param ifxyn - WMO bit-wise representation of FXY number for next table entry
!> @param line - First line of next table entry
!> @param iret - Return code
!>  - 0 = normal return
!>  - -1 = end-of-file encountered while reading from lunt
!>
!> @author J. Ator @date 2007-01-19
subroutine getntbe ( lunt, ifxyn, line, iret )

  implicit none

  integer, intent(in) :: lunt
  integer, intent(out) :: ifxyn, iret
  integer ntag, igetfxy, ifxy, igetntbl

  character*(*), intent(out) :: line
  character*128 bort_str1, bort_str2
  character*20 tags(4)
  character*6 adsc

  ! Get the first line of the next entry in the file.

  iret = igetntbl ( lunt, line )
  if ( iret == 0 ) then
    ! The first field within this line should contain the FXY number.
    call parstr ( line(1:20), tags, 4, ntag, '|', .false. )
    if ( igetfxy ( tags(1), adsc ) /= 0 ) then
      bort_str1 = 'BUFRLIB: GETNTBE - CARD BEGINNING WITH: ' // line(1:20)
      bort_str2 = '                  HAS BAD OR MISSING FXY NUMBER'
      call bort2(bort_str1, bort_str2)
    endif
    ! Store the WMO bit-wise representation of the FXY number.
    ifxyn = ifxy ( adsc )
  endif

  return
end subroutine getntbe

!> Specify whether or not code and flag
!> table information should be included during all future reads of
!> master BUFR tables.
!>
!> @param cf - Flag indicating whether or not to include code and flag table information during all future reads of
!> master BUFR tables:
!>   - 'N' = No (the default)
!>   - 'Y' = Yes
!>
!> See [Master BUFR Tables](@ref dfbfmstab)
!> for more information about master BUFR tables.  In particlar, note
!> that Table B and Table D files are always read whenever master BUFR
!> tables are being used, but the reading of Code/Flag table files is
!> optional and should only be included if the user intends to make
!> one or more future calls to subroutine getcfmng(); otherwise, the
!> reading of Code/Flag table files will result in the unnecessary use
!> of memory and other system resources.
!>
!> If Code/Flag tables are to be read and used, they must reside in
!> the same directory as the master Table B and Table D files on the
!> local filesystem, as specified within a separate call to
!> subroutine mtinfo().
!>
!> The specified value for cf will remain
!> in effect for all future reads of master BUFR tables, unless a
!> subsequent call is made to this subroutine to reset the value of
!> cf again.  If this subroutine is never called, a default value of
!> 'N' is used for cf.
!>
!> @author J. Ator @date 2017-10-13
subroutine codflg(cf)

  use moda_tablef

  implicit none

  character, intent(in) :: cf

  character*128 bort_str

  call capit(cf)
  if(cf/='Y'.and. cf/='N') then
    write(bort_str,'("BUFRLIB: CODFLG - INPUT ARGUMENT IS ",A1,", IT MUST BE EITHER Y OR N")') cf
    call bort(bort_str)
  endif
  cdmf = cf

  return
end subroutine codflg

!> Check a mnemonic for validity.
!>
!> For a mnemonic to be valid, it must have a length of between 1 and 8 characters, and it must only
!> contain characters from the allowable character set.
!>
!> @param nemo - Mnemonic
!> @returns nemock - Indicator as to whether nemo is valid:
!>  - 0 = Yes
!>  - -1 = No, the length is not between 1 and 8 characters
!>  - -2 = No, it contains characters from outside of the allowable character set
!>
!> @author J. Woollen @date 1994-01-06
integer function nemock(nemo) result(iret)

  implicit none

  integer i, lnemo

  character*(*), intent(in) :: nemo

  ! Get the length of nemo

  lnemo = 0
  do i=len(nemo),1,-1
    if(nemo(i:i)/=' ') then
      lnemo = i
      exit
    endif
  enddo
  if(lnemo<1 .or. lnemo>8) then
    iret = -1
    return
  endif

  ! Scan nemo for allowable characters

  if ( verify(nemo(1:lnemo),'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.') == 0 ) then
    iret = 0
  else
    iret = -2
  endif

  return
end function nemock
