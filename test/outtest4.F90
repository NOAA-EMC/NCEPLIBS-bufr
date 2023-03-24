! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_4' using OPENBF IO = 'NODX' and IO = 'QUIET', and using STRCPT, WRDXTB and WRITSA
!
! J. Ator, 2/17/2023
program outtest4
  implicit none

  integer*4 isetprm, ireadsb, igetmxby, icbfms, ilena, ilenb, iupbs01

  integer mxval1, mxval2, mxlvl, mxbfmg
  parameter ( mxval1 = 200 )
  parameter ( mxval2 = 12 )
  parameter ( mxlvl = 4490 )
  parameter ( mxbfmg = 50000 )

  integer mgbf ( mxbfmg ), lmgbf, ibfdt, imgdt, iermg, iersb, nsub, nlv, nlv2

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
      call readlc ( 12, dummystr, 'DUMMYSTR' )
      if ( icbfms( dummystr, 9 ) .eq. 0 ) smid = dummystr
    end if

    call ufbseq ( 12, r8arr1, mxval1, 1, nlv, 'IDLSIPTM' )
    call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'IDLSIPTM' )
    call ufbseq ( 12, r8arr1, mxval1, 1, nlv, 'HAVCOLS' )
    call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'HAVCOLS' )
    call ufbseq ( 12, r8arr1, mxval1, 1, nlv, 'CLINRVSD' )
    call ufbseq ( 13, r8arr1, mxval1, 1, nlv, 'CLINRVSD' )
    call ufbseq ( 12, r8arr2, mxval2, mxlvl, nlv2, 'TDWPRAOB' )

    call drfini ( 13, nlv2, 1, '(TDWPRAOB)' )
    call ufbseq ( 13, r8arr2, mxval2, nlv2, nlv, 'TDWPRAOB' )

    call hold4wlc ( 13, smid, 'SMID' )
    call writsa ( 13, mxbfmg, mgbf, lmgbf )
    if ( nsub .eq. 1 ) then
      call writlc ( 13, dummystr, 'DUMMYSTR' )
    end if

  end do

  call writsa ( -13, mxbfmg, mgbf, lmgbf )

  ! Close the output file.
  call closbf ( 13 )

  ! Test atrcpt, which should add 6 bytes to mgbf
  ilena = iupbs01(mgbf, 'LENM')
  call atrcpt(mgbf, lmgbf, mgbf)
  ilenb = iupbs01(mgbf, 'LENM')
  IF ((ilenb-ilena) .ne. 6) stop 3

end program outtest4
