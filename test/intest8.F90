! This is a test for NCEPLIBS-bufr.
!
! Test using RDMGSB, UFBEVN, UFBQCD, and UFBQCP to read prepbufr file
!
! J. Ator, 2/24/2023

module Share_errstr_intest8
  ! This module is needed in order to share information between the test program and subroutine errwrt, because
  ! the latter is not called by the former but rather is called directly from within the NCEPLIBS-bufr software.

  character*1500 errstr

  integer errstr_len
end module Share_errstr_intest8

subroutine errwrt(str)
  ! This subroutine supersedes the subroutine of the same name within the NCEPLIBS-bufr software, so that we can
  ! easily test the generation of error messages from within the library.

  use Share_errstr_intest8

  character*(*) str

  integer str_len

  str_len = len(str)
  errstr ( errstr_len + 1 : errstr_len + str_len + 1 ) = str
  errstr_len = errstr_len + str_len

  return
end subroutine errwrt

program intest8

  use Share_errstr_intest8

  implicit none

  integer, parameter :: mxr8pm = 15
  integer, parameter :: mxr8lv = 500
  integer, parameter :: mxr8en = 10

  integer ibit(6), nib, ier, ilv, iqcd

  real*8 hdr(6,1), r8vals( mxr8pm, mxr8lv, mxr8en ), r8v

  character*8 mnem

  print *, 'Test using RDMGSB, UFBEVN, UFBQCD, and UFBQCP to read prepbufr file.'

#ifdef KIND_8
  call setim8b ( .true. )
#endif

  open ( unit = 11, file = 'testfiles/data/prepbufr', form ='unformatted' )

  ! Read the 3rd subset from the 27th message of the prepbufr file and check some values.
  call rdmgsb ( 11, 27, 3 )
  call ufbint ( 11, hdr, 6, 1, ier, 'XOB YOB ELV TYP T29 ITP' )
  if ( ( nint(hdr(1,1)*100) /= 30233 ) .or. ( nint(hdr(2,1)*100) /= -1900 ) .or. &
       ( nint(hdr(3,1)) /= 142 ) .or. ( nint(hdr(4,1)) /= 120 ) .or. &
       ( nint(hdr(5,1)) /= 11 ) .or. ( nint(hdr(6,1)) /= 80 ) ) stop 1

  ! Get all of the moisture data from this subset and check some values.
  call ufbevn ( 11, r8vals, mxr8pm, mxr8lv, mxr8en, ilv, 'QOB QQM QPC QRC' )
  if ( ( ilv /= 51 ) .or. &
       ( nint(r8vals(1,2,2)) /= 17895 ) .or. ( nint(r8vals(2,2,2)) /= 2 ) .or. &
       ( nint(r8vals(3,2,2)) /= 1 ) .or. ( nint(r8vals(4,2,2)) /= 100 ) .or. &
       ( nint(r8vals(1,36,1)) /= 126 ) .or. ( nint(r8vals(2,36,1)) /= 9 ) .or. &
       ( nint(r8vals(3,36,1)) /= 8 ) .or. ( nint(r8vals(4,36,1)) /= 1 ) .or. &
       ( nint(r8vals(1,50,3)) /= 3 ) .or. ( nint(r8vals(2,50,3)) /= 15 ) .or. &
       ( nint(r8vals(3,50,3)) /= 1 ) .or. ( nint(r8vals(4,50,3)) /= 100 ) ) stop 2

  ! Now, get all of the temperature data from this subset which meets the conditions of being on
  ! a level where the pressure is between 490mb and 44mb, and check some of those values.
  call ufbevn ( 11, r8vals, mxr8pm, mxr8lv, mxr8en, ilv, 'POB<490 POB>44 POB TOB TQM TPC TRC' )
  if ( ( ilv /= 33 ) .or. &
       ( nint(r8vals(1,5,1)) /= 378 ) .or. ( nint(r8vals(2,5,1)*10) /= -149 ) .or. &
       ( nint(r8vals(4,5,1)) /= 8 ) .or. ( nint(r8vals(2,5,2)*10) /= -151 ) .or. &
       ( nint(r8vals(5,5,2)) /= 100 ) .or. ( nint(r8vals(1,29,1)*10) /= 699 ) .or. &
       ( nint(r8vals(2,29,1)*10) /= -809 ) .or. ( nint(r8vals(3,29,1)) /= 2 ) ) stop 3

  ! Other checks.

  call ufbqcd ( 11, 'RADCOR', iqcd )
  if ( iqcd /= 6 ) stop 4
  call ufbqcd ( 11, 'ACARSQC', iqcd )
  if ( iqcd /= 14 ) stop 5

  call ufbqcp ( 11, 2, mnem )
  if ( mnem(1:7) .ne. 'SYNDATA' ) stop 6
  call ufbqcp ( 11, 8, mnem )
  if ( mnem(1:6) .ne. 'VIRTMP' ) stop 7

  r8v = 224.
  call upftbv ( 11, 'RSRD', r8v, 6, ibit, nib )
  if ( ( nib /= 3 ) .or. ( ibit(1) /= 2 ) .or. ( ibit(2) /= 3 ) .or. ( ibit(3) /= 4 ) ) stop 8
  r8v = 264192.
  call upftbv ( 11, 'WVCQ', r8v, 6, ibit, nib )
  if ( ( nib /= 2 ) .or. ( ibit(1) /= 6 ) .or. ( ibit(2) /= 13 ) ) stop 9

  ! Test some errwrt cases in ufbevn
  errstr_len = 0
  call ufbevn ( 11, r8vals, (-1)*mxr8pm, mxr8lv, mxr8en, ilv, 'QOB QQM QPC QRC' )
  if ( ( index( errstr(1:errstr_len), 'UFBEVN - 3rd ARG. (INPUT) IS .LE. 0' ) .eq. 0 ) ) stop 10
  errstr_len = 0
  call ufbevn ( 11, r8vals, mxr8pm, (-1)*mxr8lv, mxr8en, ilv, 'QOB QQM QPC QRC' )
  if ( ( index( errstr(1:errstr_len), 'UFBEVN - 4th ARG. (INPUT) IS .LE. 0' ) .eq. 0 ) ) stop 11
  errstr_len = 0
  call ufbevn ( 11, r8vals, mxr8pm, mxr8lv, (-1)*mxr8en, ilv, 'QOB QQM QPC QRC' )
  if ( ( index( errstr(1:errstr_len), 'UFBEVN - 5th ARG. (INPUT) IS .LE. 0' ) .eq. 0 ) ) stop 12

  print *, 'SUCCESS!'
end program intest8
