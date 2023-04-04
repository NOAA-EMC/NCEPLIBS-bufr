! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_4' using OPENBF IO = 'NODX' and IO = 'QUIET', and using STRCPT, WRDXTB and WRITSA
!
! J. Ator, 2/17/2023

module Share_errstr
  ! This module is needed in order to share information between the test program and subroutine errwrt, because
  ! the latter is not called by the former but rather is called directly from within the NCEPLIBS-bufr software.

  character*800 errstr

  integer errstr_len
end module Share_errstr

subroutine errwrt(str)
  ! This subroutine supersedes the subroutine of the same name within the NCEPLIBS-bufr software, so that we can
  ! easily test the generation of error messages from within the library.

  use Share_errstr

  character*(*) str

  integer str_len

  str_len = len(str)
  errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
  errstr_len = errstr_len + str_len

  return
end subroutine errwrt

program outtest4
  use Share_errstr

  implicit none

  integer*4 isetprm, ireadsb, igetmxby, icbfms, iupbs01, igetdate

  integer mxval1, mxval2, mxlvl, mxbfmg, ilena, ilenb
  parameter ( mxval1 = 200 )
  parameter ( mxval2 = 12 )
  parameter ( mxlvl = 4490 )
  parameter ( mxbfmg = 50000 )

  integer mgbf ( mxbfmg ), mgbf2 ( mxbfmg ), lmgbf, ibfdt, imgdt, iermg, iersb, nsub, nlv, nlv2
  integer idate, mear, mmon, mday, mour

  real*8 r8arr1 ( mxval1 ), r8arr2 ( mxval2, mxlvl )

  character cmgtag*8, smid*9, dummystr*9

  print *, 'Testing writing OUT_4 using OPENBF IO = NODX and IO = QUIET, and using STRCPT, WRDXTB and WRITSA'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Set some custom array sizes.
  IF ( ( isetprm ( 'NFILES', 4 ) .ne. 0 ) .or. ( isetprm ( 'MXMSGL', 400000 ) .ne. 0 ) .or. &
      ( isetprm ( 'MAXSS', 250000 ) .ne. 0 ) .or. ( isetprm ( 'MAXMEM', 100000 ) .ne. 0 ) .or. &
      ( isetprm ( 'MAXMSG', 100 ) .ne. 0 ) .or. ( isetprm ( 'MXDXTS', 5 ) .ne. 0 ) .or. &
      ( isetprm ( 'MXCDV', 100 ) .ne. 0 ) .or. ( isetprm ( 'MXCSB', 100 ) .ne. 0 ) .or. &
      ( isetprm ( 'MXLCC', 8 ) .ne. 0 ) ) stop 1

  ! Open the BUFR input and output files.

  open ( unit = 11, file = 'testfiles/OUT_4_infile1' )
  open ( unit = 12, file = 'testfiles/OUT_4_infile2' )
  open ( unit = 13, file = 'out4.bufr', form ='unformatted' )

  call openbf ( 11, 'IN', 11 )
  call openbf ( 12, 'SEC3', 12 )
  call openbf ( 13, 'NODX', 11 )
  call openbf ( 13, 'QUIET', -1 )

  ! Set the location of the master BUFR tables.
  call mtinfo ( '../tables', 90, 91 )

  ! Set a custom maximum size for output BUFR messages.
  call maxout ( mxbfmg*4 )

  ! Confirm the value from the previous maxout setting.
  if ( igetmxby ( ) .ne. mxbfmg*4 ) stop 2

  ! The following call to STDMSG will ensure that subroutine STNDRD is called internally during the
  ! subsequent calls to WRITSB and CLOSMG.
  call stdmsg ('Y')

  ! Append a (tank) receipt time to Section 1 of each output message
  call strcpt ( 'Y', 2020, 11, 4, 15, 29 )

  ! Process 1 message with 1 data subset from infile1.

  call readmg ( 11, cmgtag, imgdt, iermg )
  if ( iermg .ne. 0 ) stop 3 

  call readsb ( 11, iersb )
  if ( iersb .ne. 0 ) stop 4

  call openmb ( 13, 'NC007000', 2020022514 )

  ! Copy values from the input message to the output message.
  call ufbseq ( 11, r8arr1, mxval1, 1, nlv, 'NC007000' )
  call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'NC007000' )

  call writsb ( 13 )

  ! Close and write out the output message.
  call closmg ( 13 )

  ! Process 1 message with multiple data subsets from infile2.

  call readmg ( 12, cmgtag, imgdt, iermg )
  if ( iermg .ne. 0 ) stop 5 

  ! Turn off output message standardization.
  call stdmsg ('N')

  ! Write DX table information for this message into the output file.
  call wrdxtb ( 12, 13 )

  ! Copy values from the input message to the output message for all data subsets.
  
  nsub = 0

  do while ( ireadsb ( 12 ) .eq. 0 )

    nsub = nsub + 1

    call ufbseq ( 12, r8arr1, mxval1, 1, nlv, 'DATETMLN' )
    ibfdt = ( nint(r8arr1(2)) * 1000000 ) + ( nint(r8arr1(3)) * 10000 ) + ( nint(r8arr1(4)) * 100 ) &
             + nint(r8arr1(5))
    call openmb ( 13, 'MSTTB001', ibfdt )
    call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'DATETMLN' )

    write ( unit = smid, fmt = '(A,I1.1)' ) 'STATION#', nsub
    if ( nsub .eq. 1 ) then
      call openbf ( 12, 'QUIET', 1 )
      errstr_len = 0
      call readlc ( 12, dummystr, 'DUMMYSTR' )
      if ( index( errstr(1:errstr_len), 'NOT LOCATED IN REPORT SUBSET - RETURN WITH MISSING' ) .eq. 0 ) stop 6
      call openbf ( 12, 'QUIET', -1 )
      if ( icbfms( dummystr, 9 ) .eq. 0 ) smid = dummystr
    end if

    call ufbseq ( 12, r8arr1, mxval1, 1, nlv, 'IDLSIPTM' )
    call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'IDLSIPTM' )
    call ufbseq ( 12, r8arr1, mxval1, 1, nlv, 'HAVCOLS' )
    call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'HAVCOLS' )
    call ufbseq ( 12, r8arr1, mxval1, 1, nlv, 'CLINRVSD' )
    call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'CLINRVSD' )
    call ufbseq ( 12, r8arr2, mxval2, mxlvl, nlv2, 'TDWPRAOB' )
    if ( nsub .eq. 1 ) then
      call openbf ( 12, 'QUIET', 1 )
      errstr_len = 0
      call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'DUMMYVAL' )
      if ( index( errstr(1:errstr_len), 'UFBSEQ - NO SPECIFIED VALUES WRITTEN OUT' ) .eq. 0 ) stop 7
      call openbf ( 12, 'QUIET', -1 )
    end if

    call drfini ( 13, nlv2, 1, '(TDWPRAOB)' )
    call ufbseq ( 13, r8arr2, mxval2, nlv2, nlv, 'TDWPRAOB' )

    call hold4wlc ( 13, smid, 'SMID' )
    if ( nsub .eq. 1 ) then
      call openbf ( 12, 'QUIET', 1 )
      errstr_len = 0
      call writlc ( 13, dummystr, 'DUMMYSTR' )
      if ( index( errstr(1:errstr_len), 'INTO SUBSET, BECAUSE NO SUBSET WAS OPEN FOR WRITING' ) .eq. 0 ) stop 8
      call openbf ( 12, 'QUIET', -1 )
    end if
    call writsa ( 13, mxbfmg, mgbf, lmgbf )
    if ( nsub .eq. 1 ) then
      call openbf ( 12, 'QUIET', 1 )
      errstr_len = 0
      call writlc ( 13, dummystr, 'DUMMYSTR' )
      if ( index( errstr(1:errstr_len), 'INTO SUBSET, BECAUSE IT WASN''T FOUND IN THE SUBSET' ) .eq. 0 ) stop 9
      call openbf ( 12, 'QUIET', -1 )
    end if

  end do

  call writsa ( -13, mxbfmg, mgbf, lmgbf )

  ! Get Section 1 date.
  idate = igetdate(mgbf, mear, mmon, mday, mour)
  if (idate.ne.20100111 .or. mear.ne.20 .or. mmon.ne.10 .or. mday.ne.1 .or. mour.ne.11) stop 10

  ! Close the output file.
  call closbf ( 13 )

  ! Test atrcpt, which should add 6 bytes to mgbf.
  mgbf2 = mgbf
  ilena = iupbs01(mgbf2, 'LENM')
  call atrcpt(mgbf, lmgbf, mgbf2)
  ilenb = iupbs01(mgbf2, 'LENM')
  IF (ilenb-ilena .ne. 6) stop 11

end program outtest4
