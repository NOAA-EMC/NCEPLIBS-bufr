! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_2' using using OPENBF IO = 'APX' and embedded tables
!
! J. Ator, 2/16/23
program outtest2
  implicit none

  integer*4 igetsc

  integer nsc, nrf, nbt, ierns, nlv

  real*8 r8ymd(3,1), r8ltl(2,1), r8oth(10,1)
  real*8 rpid, pkftbv, xmiss, getbmiss

  character libvrsn*8, cpid*8

  equivalence (rpid,cpid)

  print *, 'Testing writing OUT_2 using OPENBF IO = APX and embedded tables' 

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Get the library version number.
  call bvers ( libvrsn )
  if ( lgt( libvrsn, '10.1.1' ) ) then
     ! Specify the use of big-endian blocking.
     call setblock (1)
     !  Modify the "missing" value.
     xmiss = 9999.
     call setbmiss (xmiss)
  endif

  ! Open the input and output files.
  open  ( unit = 10, file = 'testfiles/OUT_2_preAPX' )
  open  ( unit = 11, file = 'out2.bufr', form ='unformatted')

  ! Copy the input file to the output file.
  call copybf ( 10, 11 )

  ! Now, open the BUFR tables file and re-open the output file for appending.  The re-open of the output
  ! file is needed because the previous call to copybf will have already closed it.
  open  ( unit = 12, file = 'testfiles/OUT_2_bufrtab' )
  open  ( unit = 11, file = 'out2.bufr', form ='unformatted')

  call openbf ( 11, 'APX', 12 )

  ! Check for any abnormal internal return codes so far.
  if ( igetsc ( 11 ) .ne. 0 ) stop 1

  ! Specify an originating center number to use in Section 1 of the output message, and then prepare to
  ! to write 2 subsets into the message using BUFR edition 4.
  call pkvs01 ( 'OGCE', 160 )
  call pkvs01 ( 'BEN', 4 )

  ! First subset.

  call openmb ( 11, 'NC031112', 2012101712 )

  ! Check some mnemonic specifications.
  call nemspecs ( 11, 'TMBRST', 1, nsc, nrf, nbt, ierns )
  if ( ( ierns .ne. 0 ) .or. ( nsc .ne. 3 ) .or. ( nbt .ne. 19 ) ) stop 2

  r8ymd(1,1) = 2012
  r8ymd(2,1) = 10
  r8ymd(3,1) = 17
  call ufbint ( 11, r8ymd, 3, 1, nlv, 'YEAR MNTH DAYS' )
  r8ltl(1,1) = -22.67
  r8ltl(2,1) = 72.02
  call ufbint ( 11, r8ltl, 2, 1, nlv, 'CLATH CLONH' )

  r8oth(1,1) = 13
  r8oth(2,1) = 45
  r8oth(3,1) = 216.744
  r8oth(4,1) = 85
  r8oth(5,1) = 110
  r8oth(6,1) = 17
  r8oth(7,1) = pkftbv(12,3) + pkftbv(12,9)
  r8oth(8,1) = -0.661527
  call ufbint ( 11, r8oth, 10, 1, nlv, 'HOUR MINU TMBRST SAID SACYLN ORBN OBQL SLHD1')

  call writsb ( 11 )

  ! Second subset.

  call openmb ( 11, 'NC031112', 2012101712 )

  call ufbint ( 11, r8ymd, 3, 1, nlv, 'YEAR MNTH DAYS' )
  r8ltl(2,1) = 72.13
  call ufbint ( 11, r8ltl, 2, 1, nlv, 'CLATH CLONH' )

  r8oth(2,1) = 48
  r8oth(3,1) = 214.003
  r8oth(8,1) = 0.002582
  call ufbint ( 11, r8oth, 10, 1, nlv, 'HOUR MINU TMBRST SAID SACYLN ORBN OBQL SLHD1')

  cpid = 'SUBSET#2'
  call ufbint ( 11, rpid, 1, 1, nlv, 'RPID' )

  ! Confirm the "missing" value is still the same value that was set previously via the call to setxmiss.
  IF ( nint(xmiss) .ne. nint(getbmiss()) ) stop 3

  call writsb ( 11 )

  ! Close the output file.
  call closbf ( 11 )

end program outtest2
