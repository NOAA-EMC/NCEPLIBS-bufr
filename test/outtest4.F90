! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_4' using OPENBF IO = 'NODX' and IO = 'QUIET', and using STRCPT, WRDXTB and WRITSA
!
! J. Ator, 2/17/2023

module Share_errstr_outtest4
  ! This module is needed in order to share information between the test program and subroutine errwrt, because
  ! the latter is not called by the former but rather is called directly from within the NCEPLIBS-bufr software.

  character*36000 errstr

  integer errstr_len
end module Share_errstr_outtest4

subroutine errwrt(str)
  ! This subroutine supersedes the subroutine of the same name within the NCEPLIBS-bufr software, so that we can
  ! easily test the generation of error messages from within the library.

  use Share_errstr_outtest4

  character*(*) str

  integer str_len

  str_len = len(str)
  errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
  errstr_len = errstr_len + str_len

  return
end subroutine errwrt

program outtest4
  use bufrlib
  use Share_errstr_outtest4

  implicit none

  integer*4 isetprm, ireadsb, igetmxby, icbfms, iupbs01, igetdate

  integer mxval1, mxval2, mxlvl, mxbfmg, ilena, ilenb
  parameter ( mxval1 = 200 )
  parameter ( mxval2 = 12 )
  parameter ( mxlvl = 4490 )
  parameter ( mxbfmg = 50000 )

  integer mgbf ( mxbfmg ), mgbf2 ( mxbfmg ), lmgbf, ibfdt, imgdt, iermg, iersb, nsub, nlv, nlv2
  integer idate, mear, mmon, mday, mour, mmin, iret
#ifndef KIND_8
  integer lun, il, im, nctddesc, ctddesc(20)
  character stdfil*80, locfil*80
#endif

  real*8 r8arr1 ( mxval1 ), r8arr2 ( mxval2, mxlvl )

  character cmgtag*8, smid*9, dummystr*9

  print *, 'Testing writing OUT_4 using OPENBF IO = NODX and IO = QUIET, and using STRCPT, WRDXTB and WRITSA'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Set some custom array sizes.
  if ( ( isetprm ( 'NFILES', 6 ) .ne. 0 ) .or. ( isetprm ( 'MXMSGL', 400000 ) .ne. 0 ) .or. &
      ( isetprm ( 'MAXSS', 250000 ) .ne. 0 ) .or. ( isetprm ( 'MAXMEM', 100000 ) .ne. 0 ) .or. &
      ( isetprm ( 'MAXMSG', 100 ) .ne. 0 ) .or. ( isetprm ( 'MXDXTS', 5 ) .ne. 0 ) .or. &
      ( isetprm ( 'MXCDV', 100 ) .ne. 0 ) .or. ( isetprm ( 'MXCSB', 100 ) .ne. 0 ) .or. &
      ( isetprm ( 'MXLCC', 8 ) .ne. 0 ) ) stop 1

  ! Open the BUFR input and output files.

  open ( unit = 11, file = 'testfiles/OUT_4_infile1' )
  open ( unit = 12, file = 'testfiles/OUT_4_infile2' )
  open ( unit = 13, file = 'out4.bufr', form ='unformatted' )

  errstr_len = 0
  call openbf ( 11, 'FIRST', 11 )
  call openbf ( 11, 'QUIET', 2 )
  call openbf ( 11, 'IN', 11 )
  if ( index( errstr(1:errstr_len), 'TABLE FROM INPUT BUFR FILE IN UNIT' ) .eq. 0 ) stop 2
  call openbf ( 12, 'SEC3', 12 )
  errstr_len = 0
  call openbf ( 13, 'NODX', 11 )
  if ( index( errstr(1:errstr_len), 'INTERNAL ARRAYS ASSOC. W/ INPUT UNIT' ) .eq. 0 ) stop 3

  ! Test a branch in readdx, using 'NUL' and 'INUL' options so that we don't have to actually assign
  ! units 51 and 52 to files on the local system.
  errstr_len = 0
  call openbf ( 51, 'NUL', 13 )
  call openbf ( 52, 'INUL', 51 )
  if ( index( errstr(1:errstr_len), 'INTERNAL ARRAYS ASSOC. W/ OUTPUT UNIT' ) .eq. 0 ) stop 4
  call openbf ( 13, 'QUIET', -1 )

#ifndef KIND_8
  ! Test some branches in restd, using the WMO bit-wise representation value of 65148 which corresponds
  ! to local Table D descriptor 3-62-124.
  call status ( 52, lun, il, im )
  call restd_c ( lun, 65148, nctddesc, ctddesc )
  if ( any( (/ nctddesc, ctddesc(1), ctddesc(5), ctddesc(10), ctddesc(11), ctddesc(13), ctddesc(14) /) &
        .ne.(/ 14, 49419, 34307, 17152, 7937, 34317, 2757 /) ) ) stop 5
#endif

  ! Set the location of the master BUFR tables.
  call mtinfo ( '../tables', 90, 91 )

#ifndef KIND_8
  ! Test some branches in mtfnam.
  call openbf ( 52, 'QUIET', 2 )
  errstr_len = 0
  call mtfnam ( 0, 15, 7, 2, 'TableB', stdfil, locfil )
  if ( ( index( errstr(1:errstr_len), 'Standard TableB:../tables/bufrtab.TableB_STD_0_15' ) .eq. 0 ) .or. &
       ( index( errstr(1:errstr_len), 'Local TableB:../tables/bufrtab.TableB_LOC_0_7_2' ) .eq. 0 ) .or. &
       ( index( errstr(1:errstr_len), 'not found, so using:' ) .eq. 0 ) ) stop 6
  call openbf ( 52, 'QUIET', -1 )
#endif

  ! Set a custom maximum size for output BUFR messages.
  call maxout ( mxbfmg*4 )

  ! Confirm the value from the previous maxout setting.
  if ( igetmxby ( ) .ne. mxbfmg*4 ) stop 7

  ! The following call to STDMSG will ensure that subroutine STNDRD is called internally during the
  ! subsequent calls to WRITSB and CLOSMG.
  call stdmsg ('Y')

  ! Append a (tank) receipt time to Section 1 of each output message
  call strcpt ( 'Y', 2020, 11, 4, 15, 29 )

  ! Process 1 message with 1 data subset from infile1.

  call readmg ( 11, cmgtag, imgdt, iermg )
  if ( iermg .ne. 0 ) stop 8

  call readsb ( 11, iersb )
  if ( iersb .ne. 0 ) stop 9

  call openmb ( 13, 'NC007000', 2020022514 )

  ! Copy values from the input message to the output message.
  call ufbseq ( 11, r8arr1, mxval1, 1, nlv, 'NC007000' )
  call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'NC007000' )

  call writsb ( 13 )

  ! Close and write out the output message.
  call closmg ( 13 )

  ! Process 1 message with multiple data subsets from infile2.

  call readmg ( 12, cmgtag, imgdt, iermg )
  if ( iermg .ne. 0 ) stop 10

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
      if ( index( errstr(1:errstr_len), 'NOT LOCATED IN REPORT SUBSET - RETURN WITH MISSING' ) .eq. 0 ) stop 11
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
      ! Test some error branches in ufbint and ufbseq.
      call openbf ( 12, 'QUIET', 0 )
      errstr_len = 0
      call ufbint ( 13, r8arr1, mxval1, 1, nlv, 'DUMMYVAL' )
      if ( index( errstr(1:errstr_len), 'Note: Only the first occurrence of this WARNING' ) .eq. 0 ) stop 12
      errstr_len = 0
      call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'DUMMYVAL' )
      if ( index( errstr(1:errstr_len), 'Note: Only the first occurrence of this WARNING' ) .eq. 0 ) stop 13
      call openbf ( 12, 'QUIET', 1 )
      errstr_len = 0
      call ufbint ( 13, r8arr1, mxval1, 1, nlv, 'DUMMYVAL' )
      if ( index( errstr(1:errstr_len), 'UFBINT - NO SPECIFIED VALUES WRITTEN OUT' ) .eq. 0 ) stop 14
      errstr_len = 0
      call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'DUMMYVAL' )
      if ( index( errstr(1:errstr_len), 'UFBSEQ - NO SPECIFIED VALUES WRITTEN OUT' ) .eq. 0 ) stop 15
      call openbf ( 12, 'QUIET', -1 )
    end if
    call drfini ( 13, nlv2, 1, '(TDWPRAOB)' )
    call ufbseq ( 13, r8arr2, mxval2, nlv2, nlv, 'TDWPRAOB' )

    call hold4wlc ( 13, smid, 'SMID' )
    if ( nsub .eq. 1 ) then
      call openbf ( 12, 'QUIET', 1 )
      errstr_len = 0
      call writlc ( 13, dummystr, 'DUMMYSTR' )
      if ( index( errstr(1:errstr_len), 'INTO SUBSET, BECAUSE NO SUBSET WAS OPEN FOR WRITING' ) .eq. 0 ) stop 16
      call openbf ( 12, 'QUIET', -1 )
    end if
    call writsa ( 13, mxbfmg, mgbf, lmgbf )
    if ( nsub .eq. 1 ) then
      call openbf ( 12, 'QUIET', 1 )
      errstr_len = 0
      call writlc ( 13, dummystr, 'DUMMYSTR' )
      if ( index( errstr(1:errstr_len), 'INTO SUBSET, BECAUSE IT WASN''T FOUND IN THE SUBSET' ) .eq. 0 ) stop 17
      call openbf ( 12, 'QUIET', -1 )
    end if

  end do

  errstr_len = 0
  call openbf ( 13, 'QUIET', 2 )
  call writsa ( -13, mxbfmg, mgbf, lmgbf )
  if ( index( errstr(1:errstr_len), 'MSGWRT: LUNIT =' ) .eq. 0 ) stop 18
  call openbf ( 13, 'QUIET', -1 )

  ! Get Section 1 date.
  idate = igetdate(mgbf, mear, mmon, mday, mour)
  if ( any((/idate,mear,mmon,mday,mour/).ne.(/20100111,20,10,1,11/)) ) stop 19

  ! Get the tank receipt time.
  call rtrcptb ( mgbf, mear, mmon, mday, mour, mmin, iret )
  if ( any((/iret,mear,mmon,mday,mour,mmin/).ne.(/0,2020,11,4,15,29/)) ) stop 20

  ! Close the output file.
  call closbf ( 13 )

  ! Test atrcpt, which should add 6 bytes to mgbf.
  mgbf2 = mgbf
  ilena = iupbs01(mgbf2, 'LENM')
  call atrcpt(mgbf, lmgbf, mgbf2)
  ilenb = iupbs01(mgbf2, 'LENM')
  if (ilenb-ilena .ne. 6) stop 21

end program outtest4
