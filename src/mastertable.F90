!> @file
!> @brief Read master table information.
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

  if ( ( imt .eq. 0 ) .and. ( imtv .le. 13 ) ) then
    ! For master table 0, version 13 is a superset of all earlier versions.
    stdfil = mtdir(1:lmtd) // '/bufrtab.' // tbltyp2(1:ltbt) // '_STD_0_13'
  else
    write ( fmtf, '(A,I1,A,I1,A)' ) '(4A,I', isize(imt), ',A,I', isize(imtv), ')'
    write ( stdfil, fmtf ) mtdir(1:lmtd), '/bufrtab.', tbltyp2(1:ltbt), '_STD_', imt, '_', imtv
  endif
  if ( iprt .ge. 2 ) then
    call errwrt('Standard ' // tbltyp2(1:ltbt) // ':')
    call errwrt(stdfil)
  endif
  inquire ( file = stdfil, exist = found )
  if ( .not. found ) then
    bort_str = 'BUFRLIB: MTFNAM - COULD NOT FIND STANDARD FILE:'
    call bort2(bort_str,stdfil)
  endif

  ! Now determine the local master table path/filename.

  ! Use the local table corresponding to the originating center and local table version number, if such a table exists.
  ! Otherwise use the local table from NCEP.

  write ( fmtf, '(A,I1,A,I1,A,I1,A)' ) '(4A,I', isize(imt), ',A,I', isize(iogce), ',A,I',  isize(imtvl), ')'
  write ( locfil, fmtf ) mtdir(1:lmtd), '/bufrtab.', tbltyp2(1:ltbt), '_LOC_', imt, '_', iogce, '_', imtvl
  if ( iprt .ge. 2 ) then
    call errwrt('Local ' // tbltyp2(1:ltbt) // ':')
    call errwrt(locfil)
  endif
  inquire ( file = locfil, exist = found )
  if ( .not. found ) then
    ! Use the local table from NCEP.
    locfil = mtdir(1:lmtd) // '/bufrtab.' // tbltyp2(1:ltbt) // '_LOC_0_7_1'
    if ( iprt .ge. 2 ) then
      call errwrt('Local ' // tbltyp2(1:ltbt) // ' not found, so using:')
      call errwrt(locfil)
    endif
    inquire ( file = locfil, exist = found )
    if ( .not. found ) then
      bort_str = 'BUFRLIB: MTFNAM - COULD NOT FIND LOCAL FILE:'
      call bort2(bort_str,locfil)
    endif
  endif

  return
end subroutine mtfnam

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
  do while ( ( iers .eq. 0 ) .or. ( ierl .eq. 0 ) )
    if ( ( iers .eq. 0 ) .and. ( ierl .eq. 0 ) ) then
      if ( isfxyn .eq. ilfxyn ) then
        cmatch = adn30 ( isfxyn, 6 )
        write(bort_str,'("BUFRLIB: RDMTBB - STANDARD AND LOCAL '// &
          'TABLE B FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)') cmatch(1:1), '-', cmatch(2:3), '-', cmatch(4:6)
        call bort(bort_str)
      else if ( isfxyn .lt. ilfxyn ) then
        call sntbbe ( isfxyn, stline, mxmtbb, nmtbb, imfxyn, cmscl, cmsref, cmbw, cmunit, cmmnem, cmdsc, cmelem )
        call getntbe ( lunstb, isfxyn, stline, iers )
      else
        call sntbbe ( ilfxyn, ltline, mxmtbb, nmtbb, imfxyn, cmscl, cmsref, cmbw, cmunit, cmmnem, cmdsc, cmelem )
        call getntbe ( lunltb, ilfxyn, ltline, ierl )
      endif
    else if ( iers .eq. 0 ) then
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
  do while ( ( iers .eq. 0 ) .or. ( ierl .eq. 0 ) )
    if ( ( iers .eq. 0 ) .and. ( ierl .eq. 0 ) ) then
      if ( isfxyn .eq. ilfxyn ) then
        cmatch = adn30 ( isfxyn, 6 )
        write(bort_str,'("BUFRLIB: RDMTBD - STANDARD AND LOCAL '// &
          'TABLE D FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)') cmatch(1:1), '-', cmatch(2:3), '-', cmatch(4:6)
        call bort(bort_str)
      else if ( isfxyn .lt. ilfxyn ) then
        call sntbde ( lunstd, isfxyn, stline, mxmtbd, mxelem, nmtbd, imfxyn, cmmnem, cmdsc, cmseq, nmelem, iefxyn, ceelem )
        call getntbe ( lunstd, isfxyn, stline, iers )
      else
        call sntbde ( lunltd, ilfxyn, ltline, mxmtbd, mxelem, nmtbd, imfxyn, cmmnem, cmdsc, cmseq, nmelem, iefxyn, ceelem )
        call getntbe ( lunltd, ilfxyn, ltline, ierl )
      endif
    else if ( iers .eq. 0 ) then
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
  do while ( ( iers .eq. 0 ) .or. ( ierl .eq. 0 ) )
    if ( ( iers .eq. 0 ) .and. ( ierl .eq. 0 ) ) then
      if ( isfxyn .eq. ilfxyn ) then
        cmatch = adn30 ( isfxyn, 6 )
        write(bort_str,'("BUFRLIB: RDMTBF - STANDARD AND LOCAL '// &
          'CODE/FLAG TABLE FILES BOTH CONTAIN SAME FXY NUMBER: ",5A)') cmatch(1:1), '-', cmatch(2:3), '-', cmatch(4:6)
        call bort(bort_str)
      else if ( isfxyn .lt. ilfxyn ) then
        call sntbfe ( lunstf, isfxyn )
        call getntbe ( lunstf, isfxyn, stline, iers )
      else
        call sntbfe ( lunltf, ilfxyn )
        call getntbe ( lunltf, ilfxyn, ltline, ierl )
      endif
    else if ( iers .eq. 0 ) then
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

  character*(*), intent(in) :: line
  character, intent(out) :: cmelem(120,*), cmunit(24,*), cmsref(12,*), cmmnem(8,*), cmscl(4,*), cmbw(4,*), cmdsc(*)*4
  character*200 tags(10), wktag
  character*128 bort_str1, bort_str2

  if ( nmtbb .ge. mxmtbb ) call bort('BUFRLIB: SNTBBE - OVERFLOW OF MERGED ARRAYS')
  nmtbb = nmtbb + 1

  bort_str1 = 'BUFRLIB: SNTBBE - CARD BEGINNING WITH: ' // line(1:20)

  ! Store the FXY number.  This is the element descriptor.

  imfxyn ( nmtbb ) = ifxyn

  ! Parse the table entry.

  call parstr ( line, tags, 10, ntag, '|', .false. )
  if ( ntag .lt. 4 ) then
    bort_str2 = '                  HAS TOO FEW FIELDS'
    call bort2(bort_str1,bort_str2)
  endif

  ! Scale factor.

  tags(2) = adjustl( tags(2) )
  if ( tags(2) .eq. ' ' ) then
    bort_str2 = '                  HAS MISSING SCALE FACTOR'
    call bort2(bort_str1,bort_str2)
  endif
  tags(2)(1:4) = adjustr( tags(2)(1:4) )
  do ii = 1, 4
    cmscl ( ii, nmtbb ) = tags(2)(II:II)
  enddo

  ! Reference value.

  tags(3) = adjustl( tags(3) )
  if ( tags(3) .eq. ' ' ) then
    bort_str2 = '                  HAS MISSING REFERENCE VALUE'
    call bort2(bort_str1,bort_str2)
  endif
  tags(3)(1:12) = adjustr( tags(3)(1:12) )
  do ii = 1, 12
    cmsref ( ii, nmtbb ) = tags(3)(II:II)
  enddo

  ! Bit width.

  tags(4) = adjustl( tags(4) )
  if ( tags(4) .eq. ' ' ) then
    bort_str2 = '                  HAS MISSING BIT WIDTH'
    call bort2(bort_str1,bort_str2)
  endif
  tags(4)(1:4) = adjustr( tags(4)(1:4) )
  do ii = 1, 4
    cmbw ( ii, nmtbb ) = tags(4)(II:II)
  end do

  ! Units.  Note that this field is allowed to be blank.

  if ( ntag .gt. 4 ) then
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
  if ( ntag .gt. 5 ) then
    wktag = tags(6)
    call parstr ( wktag, tags, 10, ntag, ';', .false. )
    if ( ntag .gt. 0 ) then
      ! The first additional field contains the mnemonic.
      tags(1) = adjustl( tags(1) )
      ! If there is a mnemonic, then make sure it's legal.
      if ( ( tags(1) .ne. ' ' ) .and. ( nemock ( tags(1) ) .ne. 0 ) ) then
        bort_str2 = '                  HAS ILLEGAL MNEMONIC'
        call bort2(bort_str1,bort_str2)
      endif
      do ii = 1, 8
        cmmnem ( ii, nmtbb ) = tags(1)(ii:ii)
      enddo
    endif
    if ( ntag .gt. 1 ) then
      ! The second additional field contains descriptor codes.
      tags(2) = adjustl( tags(2) )
      cmdsc ( nmtbb ) = tags(2)(1:4)
    endif
    if ( ntag .gt. 2 ) then
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
!> @param ceelem - Merged array containing  element names
!>
!> @author J. Ator @date 2007-01-19
subroutine sntbde ( lunt, ifxyn, line, mxmtbd, mxelem, nmtbd, imfxyn, cmmnem, cmdsc, cmseq, nmelem, iefxyn, ceelem )

  implicit none

  integer, intent(in) :: lunt, ifxyn, mxmtbd, mxelem
  integer, intent(out) :: nmtbd, imfxyn(*), nmelem(*), iefxyn(mxmtbd,mxelem)
  integer ii, ipt, ntag, nelem, nemock, ifxy, igetfxy, igetntbl

  character*(*), intent(in) :: line
  character, intent(out) :: cmseq(120,*), cmmnem(8,*), cmdsc(*)*4, ceelem(mxmtbd,mxelem)*120
  character*200 tags(10), cline
  character*128 bort_str1, bort_str2
  character*6 adn30, adsc, clemon

  logical done

  if ( nmtbd .ge. mxmtbd ) call bort('BUFRLIB: SNTBDE - OVERFLOW OF MERGED ARRAYS')
  nmtbd = nmtbd + 1

  clemon = adn30 ( ifxyn, 6 )
  write(bort_str1,'("BUFRLIB: SNTBDE - TABLE D ENTRY FOR SEQUENCE DESCRIPTOR: ",5A)') &
    clemon(1:1), '-', clemon(2:3), '-', clemon(4:6)

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
  if ( ipt .ne. 0 ) then
    ! Parse the rest of the line.  Any of the fields may be blank.
    call parstr ( line(ipt+1:), tags, 10, ntag, ';', .false. )
    if ( ntag .gt. 0 ) then
      ! The first additional field contains the mnemonic.
      tags(1) = adjustl( tags(1) )
      ! If there is a mnemonic, then make sure it's legal.
      if ( ( tags(1) .ne. ' ' ) .and. ( nemock ( tags(1) ) .ne. 0 ) ) then
        bort_str2 = '                  HAS ILLEGAL MNEMONIC'
        call bort2(bort_str1,bort_str2)
      endif
      do ii = 1, 8
        cmmnem ( ii, nmtbd ) = tags(1)(ii:ii)
      enddo
    endif
    if ( ntag .gt. 1 ) then
      ! The second additional field contains descriptor codes.
      tags(2) = adjustl( tags(2) )
      cmdsc ( nmtbd ) = tags(2)(1:4)
    endif
    if ( ntag .gt. 2 ) then
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
    if ( igetntbl ( lunt, cline ) .ne. 0 ) then
      bort_str2 = '                  IS INCOMPLETE'
      call bort2(bort_str1,bort_str2)
    endif
    call parstr ( cline, tags, 10, ntag, '|', .false. )
    if ( ntag .lt. 2 ) then
      bort_str2 = '                  HAS BAD ELEMENT CARD'
      call bort2(bort_str1,bort_str2)
    endif
    ! The second field contains the FXY number for this element.
    if ( igetfxy ( tags(2), adsc ) .ne. 0 ) then
      bort_str2 = '                  HAS BAD OR MISSING ELEMENT FXY NUMBER'
      call bort2(bort_str1,bort_str2)
    endif
    if ( nelem .ge. mxelem ) CALL BORT('BUFRLIB: SNTBDE - OVERFLOW OF MERGED ARRAYS')
    nelem = nelem + 1
    iefxyn ( nmtbd, nelem ) = ifxy ( adsc )
    ! The third field (if it exists) contains the element name.
    if ( ntag .gt. 2 ) then
      tags(3) = adjustl( tags(3) )
      ceelem ( nmtbd, nelem ) = tags(3)(1:120)
    else
      ceelem ( nmtbd, nelem ) = ' '
    endif
    ! Is this the last line for this table entry?
    if ( index ( tags(2), ' >' ) .eq. 0 ) done = .true.
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
  character*128 bort_str1, bort_str2
  character*6 adn30, clemon, cdsc

  logical done, lstnblk

  ! We already have the FXY number.  Now we need to read and parse all of the remaining lines from the table entry for this
  ! FXY number.  The information for each individual code figure or bit number will then be stored as a separate entry within
  ! the internal memory structure.

  clemon = adn30 ( ifxyn, 6 )
  write(bort_str1,'("BUFRLIB: SNTBFE - TABLE F ENTRY FOR ELEMENT DESCRIPTOR: ",5A)') &
    clemon(1:1), '-', clemon(2:3), '-', clemon(4:6)

  done = .false.
  nidfxy = 0
  nidval = 0

  do while ( .not. done )

    if ( igetntbl ( lunt, cline ) .ne. 0 ) then
      bort_str2 = '                  IS INCOMPLETE'
      call bort2(bort_str1,bort_str2)
    endif

    call parstr ( cline, tags, 4, ntag, '|', .false. )
    if ( ( ntag .lt. 2 ) .or. ( ntag .gt. 3 ) ) then
      bort_str2 = '                  HAS BAD CARD'
      call bort2(bort_str1,bort_str2)
    endif

    if ( ntag .eq. 2 ) then

      ! This line contains a list of dependencies.

      call parstr ( tags(2), cdstr, 2, ntag, '=', .false. )
      if ( ntag .ne. 2 ) then
        bort_str2 = '           HAS BAD DEPENDENCY CARD'
        call bort2(bort_str1,bort_str2)
      endif
      ! Parse the list of FXY numbers.
      call parstr ( cdstr(1), adsc, 10, nidfxy, ',', .false. )
      if ( ( nidfxy .eq. 0 ) .or. ( ( nidfxy .eq. 1 ) .and. ( adsc(1) .eq. ' ' ) ) ) then
        bort_str2 = '        HAS BAD DEPENDENCY LIST (FXY)'
        call bort2(bort_str1,bort_str2)
      endif
      do ii = 1, nidfxy
        if ( igetfxy ( adsc(ii), cdsc ) .ne. 0 ) then
          bort_str2 = '        HAS BAD DEPENDENCY (FXY)'
          call bort2(bort_str1,bort_str2)
        endif
        idfxy(ii) = ifxy( cdsc )
      enddo
      ! Parse the list of values.
      call parstr ( cdstr(2), cval, 25, nidval, ',', .false. )
      if ( ( nidval .eq. 0 ) .or. ( ( nidval .eq. 1 ) .and. ( cval(1) .eq. ' ' ) ) ) then
        bort_str2 = '        HAS BAD DEPENDENCY LIST (VAL)'
        call bort2(bort_str1,bort_str2)
      endif
      do ii = 1, nidval
        cval(ii) = adjustl( cval(ii) )
        call strnum ( cval(ii), ival, ier )
        if ( ier .ne. 0 ) then
          bort_str2 = '        HAS BAD DEPENDENCY (VAL)'
          call bort2(bort_str1,bort_str2)
        endif
        idval(ii) = ival
      enddo

    else

      ! This line contains a value (code figure or bit number) and corresponding meaning.

      ipt = index ( tags(2), ' >' )
      if ( ipt .eq. 0 ) then
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
      do while ( ( lt3 .gt. 0 ) .and. ( .not. lstnblk ) )
        if ( tags(3)(lt3:lt3) .ne. ' ' ) then
          lstnblk = .true.
        else
          lt3 = lt3 - 1
        endif
      enddo
      ! Store the information for this value within the internal memory structure.
      if ( ( nidfxy .eq. 0 ) .and. ( nidval .eq. 0 ) ) then
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
