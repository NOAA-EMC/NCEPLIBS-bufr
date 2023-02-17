! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_6' using OPENMG and UFBSTP, and storing integer values larger than 32 bits.
!
! J. Ator, 2/17/23
program outtest6
  implicit none

  real*8 r8f5fc(8,5), r8dbss(4,3), r8wind(2,1), r8val

  integer nlv, iersvb, xrc

  character ptidc*16

  print *, 'Testing writing OUT_6 using OPENMG and UFBSTP, and storing integer values larger than 32 bits'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Open the BUFR table and output file.
  open ( unit = 11, file = 'out6.bufr', form ='unformatted')
  open ( unit = 12, file = 'testfiles/OUT_6_bufrtab' )
  call openbf ( 11, 'OUT', 12 )

  ! Open a new message for output.
  call openmg ( 11, 'F5FCMESG', 2021022312 )

  ! Set a custom minutes value in Section 1 of the message.
  call minimg ( 11, 55 )

  ! Store the subset data.

  r8wind ( 1, 1 ) = 290.
  r8wind ( 2, 1 ) = 6.5
  call ufbint ( 11, r8wind, 2, 1, nlv, 'WDIR WSPD' )

  r8val = 17.
  call setvalnb ( 11, 'WDIR', 1, 'HOUR', 1, r8val, iersvb )
  r8val = 16.
  call setvalnb ( 11, 'TMDB', 1, 'HOUR', -1, r8val, iersvb )

  r8dbss ( 1, 1 ) = 1.0
  r8dbss ( 2, 1 ) = 34.1
  r8dbss ( 3, 1 ) = 284.7
  r8dbss ( 4, 1 ) = 5.0653
  r8dbss ( 1, 2 ) = 2.5
  r8dbss ( 2, 2 ) = 34.1
  r8dbss ( 3, 2 ) = 284.8
  r8dbss ( 4, 2 ) = 5.066
  r8dbss ( 1, 3 ) = 4.0
  r8dbss ( 2, 3 ) = 34.2
  r8dbss ( 3, 3 ) = 284.6
  r8dbss ( 4, 3 ) = 5.0644
  call ufbstp ( 11, r8dbss, 4, 3, nlv, 'DBSS SALN SST1 WCON' )

  r8f5fc ( 1, 1 ) = 0.08593800
  r8f5fc ( 2, 1 ) = 0.00390625
  r8f5fc ( 3, 1 ) = 32.50110000_8
  r8f5fc ( 4, 1 ) = 0.8883
  r8f5fc ( 5, 1 ) = -0.3818
  r8f5fc ( 6, 1 ) = 0.6140
  r8f5fc ( 7, 1 ) = -0.6438
  r8f5fc ( 8, 1 ) = 3.11
  r8f5fc ( 1, 2 ) = 0.08984400
  r8f5fc ( 2, 2 ) = 0.00390625
  r8f5fc ( 3, 2 ) = 26.45480000_8
  r8f5fc ( 4, 2 ) = 0.8795
  r8f5fc ( 5, 2 ) = -0.4412
  r8f5fc ( 6, 2 ) = 0.5909
  r8f5fc ( 7, 2 ) = -0.7761
  r8f5fc ( 8, 2 ) = 3.12
  r8f5fc ( 1, 3 ) = 0.09375000
  r8f5fc ( 2, 3 ) = 0.00390625
  r8f5fc ( 3, 3 ) = 41.96410000_8
  r8f5fc ( 4, 3 ) = 0.9124
  r8f5fc ( 5, 3 ) = -0.3137
  r8f5fc ( 6, 3 ) = 0.7302
  r8f5fc ( 7, 3 ) = -0.5316
  r8f5fc ( 8, 3 ) = 3.13
  r8f5fc ( 1, 4 ) = 0.09765600
  r8f5fc ( 2, 4 ) = 0.00390625
  r8f5fc ( 3, 4 ) = 28.98830000_8
  r8f5fc ( 4, 4 ) = 0.8917
  r8f5fc ( 5, 4 ) = -0.3020
  r8f5fc ( 6, 4 ) = 0.7413
  r8f5fc ( 7, 4 ) = -0.4804
  r8f5fc ( 8, 4 ) = 3.14
  r8f5fc ( 1, 5 ) = 0.10156300
  r8f5fc ( 2, 5 ) = 0.00390628
  r8f5fc ( 3, 5 ) = 11.71090000_8
  r8f5fc ( 4, 5 ) = 0.8273
  r8f5fc ( 5, 5 ) = -0.2884
  r8f5fc ( 6, 5 ) = 0.4968
  r8f5fc ( 7, 5 ) = -0.4184
  r8f5fc ( 8, 5 ) = 3.15
  call drfini ( 11, 5, 1, '{F5FCRSEQ}' )
  call ufbseq ( 11, r8f5fc, 8, 5, nlv, 'F5FCRSEQ' )

  call writsb ( 11 )

  ! Write a long character string to the message.
  ptidc = '300534061608630'
  call writlc ( 11, ptidc, 'PTIDC' )

  ! Close the output file.
  call closbf ( 11 )

  ! Compare the output file to the baseline output file.
  call execute_command_line ( 'cmp -s out6.bufr testfiles/OUT_6', exitstat = xrc )
  if ( xrc .ne. 0 ) stop 1

  print *, 'SUCCESS!'

end program outtest6
