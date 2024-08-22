! This is a test for NCEPLIBS-bufr.
!
! Writes test file 'testfiles/OUT_12' using a bitmap and Table C marker operators.
!
! J. Ator, 8/20/2024
program outtest12
  implicit none

  real*8 r8bitmap(16), r8data(10,1), r8pccf(3), r8mrkr(3)

  integer nlv, ii, jj, iostat1, iostat2

  character ptidc*16

  print *, 'Testing writing OUT_12 using a bitmap and Table C marker operators'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  ! Open the BUFR table and output file.
  open ( unit = 11, file = 'out12.bufr', form ='unformatted', iostat = iostat1 )
  open ( unit = 12, file = 'testfiles/OUT_12_bufrtab', iostat = iostat2 )
  if ( ( iostat1 /= 0 ) .or. ( iostat2 /= 0 ) ) stop 1
  call openbf ( 11, 'NODX', 12 )
  call stdmsg ( 'Y' )
  call pkvs01 ( 'BEN', 4 )

  do jj = 1, 4
    ! The first 2 data subsets will be uncompressed, and the second 2 data subsets will be compressed.
    if ( jj == 3 ) call cmpmsg ( 'Y' )

    ! Open a new message for output.
    call openmb ( 11, 'FN004017', 2024072412 )

    ! Store the subset data.
    r8data ( 1, 1 ) = 2024.
    r8data ( 2, 1 ) = 7.
    r8data ( 3, 1 ) = 24.
    r8data ( 4, 1 ) = 12.
    r8data ( 5, 1 ) = 55. + jj
    r8data ( 6, 1 ) = 10.
    r8data ( 7, 1 ) = -24.55750 + (jj * 0.001)
    r8data ( 8, 1 ) = 86.23435 + (jj * 0.001)
    r8data ( 9, 1 ) = 10500.
    call ufbint ( 11, r8data, 10, 1, nlv, 'YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH HMSL' )
    r8data ( 1, 1 ) = 283.5 - jj
    r8data ( 2, 1 ) = 170.
    r8data ( 3, 1 ) = 290. - (jj * 5)
    r8data ( 4, 1 ) = 6.5 + (jj * 0.1)
    r8data ( 5, 1 ) = 3.
    r8data ( 6, 1 ) = 0.
    r8data ( 7, 1 ) = 10.
    call ufbint ( 11, r8data, 10, 1, nlv, 'TMDBST SMMO WDIR WSPD POAF ROLQ FOST' )

    ! Store the bitmap.
    do ii = 1, 16
      r8bitmap(ii) = 1.
    end do
    r8bitmap(11) = 0.
    r8bitmap(13) = 0.
    r8bitmap(14) = 0.
    call ufbrep ( 11, r8bitmap, 1, 16, nlv, 'DPRI' )

    ! Store the percent confidences.
    r8pccf(1) = 93. - jj
    r8pccf(2) = 94. - jj
    r8pccf(3) = 87. - jj
    call ufbrep ( 11, r8pccf, 1, 3, nlv, 'PCCF' )

    ! Store the marker operators containing standard deviations.
    r8mrkr(1) = 0.3 + (jj * 0.1)
    r8mrkr(2) = 1.0 * jj  ! WDIR has scale 0, so the corresponding marker operator will also be scale 0
    r8mrkr(3) = 0.1 + (jj * 0.3)
    call ufbrep ( 11, r8mrkr, 1, 3, nlv, '224255' )

    call writsb ( 11 )

    ! Write a long character string to the message.
    ptidc = 'HC888V3497074363'
    call writlc ( 11, ptidc, 'PTIDC' )

    if ( jj == 2 ) call closmg ( 11 )

  end do

  ! Close the output file.
  call closbf ( 11 )

end program outtest12
