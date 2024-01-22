! This is a test for NCEPLIBS-bufr.
!
! Reads test file 'testfiles/IN_3' using nested delayed
! replication, OPENBF IO = IN, and LUNIN = LUNDX.
!
! Ed Hartnett, J. Ator, 2/22/2023
program intest3
  implicit none

  integer*4 ireadns, ifbget, lcmgdf, ibfms

  integer mxr8pm, mxr8lv
  parameter (mxr8pm = 6)
  parameter (mxr8lv = 50)

  integer isct, imgdt, ityr, itmo, itdy, ithr, itmi, ier, &
        nr8lv, nr8rr, nr8rf, nr8rhr, nr8rh, nr8rdr, nr8rd

  real*8 r8arr(mxr8pm, mxr8lv), r8arf(mxr8pm, mxr8lv), r8arhr(1, mxr8lv), &
       r8arh(mxr8pm, mxr8lv), r8ardr(1, mxr8lv), r8ard (mxr8pm, mxr8lv), r8val

  character cmgtag*8, celem*40, cunit*20, c8val*8

  equivalence (r8val, c8val)

  print *, 'Testing reading IN_3, using nested delayed replication, OPENBF IO = IN, and LUNIN = LUNDX'

#ifdef KIND_8
  call setim8b(.true.)
#endif

  open(unit = 11, file = 'testfiles/IN_3', form ='unformatted')

  ! First, read some values from all of the data subsets.
  call ufbtab(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'CLAT CLON HSMSL {SHRVFFSQ}')
  if (nr8lv .ne. 10 .or. nint(r8arr(1,1)*100) .ne. 4025 .or. nint(r8arr(3,1)) .ne. 88 .or. &
       nint(r8arr(4,1)) .ne. 12 .or. nint(r8arr(2,2)*100) .ne. -8852 .or. &
       nint(r8arr(4,2)) .ne. 20 .or. nint(r8arr(1,5)*100) .ne. 3352 .or. &
       ibfms(r8arr(3,5)) .ne. 1 .or. nint(r8arr(1,8)*100) .ne. 3277 .or. &
       nint(r8arr(1,9)*100) .ne. 3693 .or. nint(r8arr(2,9)*100) .ne. -9496 .or. &
       nint(r8arr(3,9)) .ne. 228 .or. nint(r8arr(4,9)) .ne. 20) stop 1
  call ufbtab(11, r8arr, mxr8pm, mxr8lv, nr8lv, 'RPID')
  if (nr8lv .ne. 10) stop 2
  r8val = r8arr(1,1)
  if (c8val(1:5) .ne. 'HARP1') stop 3
  r8val = r8arr(1,3)
  if (c8val(1:5) .ne. 'IOLK1') stop 4
  r8val = r8arr(1,8)
  if (c8val(1:5) .ne. 'WLDA1') stop 5
  r8val = r8arr(1,9)
  if (c8val(1:5) .ne. 'COMO2') stop 6

  ! (Re)open the file for usual reading of each subset one at a time.
  call openbf ( 11, 'IN', 11 )
  isct = 0

  do while (ireadns( 11, cmgtag, imgdt ) .eq. 0)
     ! Continue checking with the next subset.
     isct = isct + 1

     call ufbint ( 11, r8arr, mxr8pm, mxr8lv, nr8rr, 'YEAR MNTH DAYS HOUR MINU' )
     call ufbrep ( 11, r8arf, mxr8pm, mxr8lv, nr8rf, 'TSIG YEAR MNTH DAYS HOUR MINU' )
     call ufbint ( 11, r8arhr, 1, mxr8lv, nr8rhr, '{SHRVHTSQ}' )
     call ufbseq ( 11, r8arh, mxr8pm, mxr8lv, nr8rh, 'SHRVHTSQ' )
     call ufbint ( 11, r8ardr, 1, mxr8lv, nr8rdr, '{SHRVDCSQ}' )
     call ufbseq ( 11, r8ard, mxr8pm, mxr8lv, nr8rd, 'SHRVDCSQ' )

     if (isct .eq. 1) then
        call rtrcpt ( 11, ityr, itmo, itdy, ithr, itmi, ier )
        if ( ier .ne. -1 ) stop 11

        if ( ( nr8rr .ne. 1 ) .or. ( nint(r8arr(2,1)) .ne. 2 ) .or. &
             ( nint(r8arr(4,1)) .ne. 14 ) .or. ( nint(r8arr(5,1)) .ne. 3 ) ) stop 12

        if ( ( nr8rf .ne. 12 ) .or. ( nint(r8arf(1,1)) .ne. 4 ) .or. ( nint(r8arf(2,1)) .ne. 2015 ) .or. &
             ( nint(r8arf(3,1)) .ne. 2 ) .or. ( nint(r8arf(4,1)) .ne. 12 ) .or. ( nint(r8arf(5,1)) .ne. 18 ) &
             .or. ( nint(r8arf(4,4)) .ne. 13 ) .or. ( nint(r8arf(5,4)) .ne. 12 ) .or. &
             ( nint(r8arf(4,10)) .ne. 15 ) .or. ( nint(r8arf(5,10)) .ne. 0 ) .or. &
             ( nint(r8arf(1,11)) .ne. 4 ) .or. ( nint(r8arf(4,11)) .ne. 15 ) .or. &
             ( nint(r8arf(5,11)) .ne. 6 ) ) stop 13

        if ( ( nr8rhr .ne. 12 ) .or. ( nint(r8arhr(1,1)) .ne. 1 ) .or. ( nint(r8arhr(1,2)) .ne. 1 ) .or. &
             ( nint(r8arhr(1,3)) .ne. 1 ) .or. ( nint(r8arhr(1,8)) .ne. 1 ) .or. ( nr8rh .ne. 12 ) .or. &
             ( nint(r8arh(3,1)*1000) .ne. 1402 ) .or. ( nint(r8arh(5,1)) .ne. 26 ) .or. ( nint(r8arh(3,2)*1000) .ne. 1372 ) .or. &
             ( nint(r8arh(1,8)) .ne. 0 ) .or. ( nint(r8arh(2,8)) .ne. 0 ) .or. ( nr8rdr .ne. 12 ) .or. &
             ( nint(r8ardr(1,1)) .ne. 0 ) .or. ( nint(r8ardr(1,2)) .ne. 1 ) .or. ( nint(r8ardr(1,4)) .ne. 0 ) .or. &
             ( nint(r8ardr(1,6)) .ne. 1 ) .or. ( nint(r8ardr(1,9)) .ne. 0 ) .or. ( nint(r8ardr(1,10)) .ne. 1 ) .or. &
             ( nr8rd .ne. 3 ) .or. ( nint(r8ard(3,1)*100) .ne. 33980 ) .or. ( nint(r8ard(3,2)*100) .ne. 33131 ) .or. &
             ( nint(r8ard(5,2)) .ne. 26 ) .or. ( nint(r8ard(1,3)) .ne. 0 ) .or. ( nint(r8ard(2,3)) .ne. 1 ) .or. &
             ( nint(r8ard(3,3)*100) .ne. 32564 ) ) stop 14

     else if ( isct .eq. 4 ) then
        call rtrcpt ( 11, ityr, itmo, itdy, ithr, itmi, ier )
        if ( ( ier .ne. 0 ) .or. ( ityr .ne. 2014 ) .or. ( itmo .ne. 10 ) .or. ( itdy .ne. 5 ) .or. &
             ( ithr .ne. 12 ) .or. ( itmi .ne. 52 ) ) stop 41

        if ( ( nr8rhr .ne. 20 ) .or. ( NINT(r8arhr(1,1)) .ne. 2 ) .or. ( NINT(r8arhr(1,2)) .ne. 2 ) .or. &
             ( nint(r8arhr(1,12)) .ne. 2 ) .or. ( nint(r8arhr(1,19)) .ne. 2 ) .or. ( nr8rh .ne. 40 ) .or. &
             ( nint(r8arh(1,1)) .ne. 0 ) .or. ( nint(r8arh(3,1)*1000) .ne. 2286 ) .or. ( nint(r8arh(1,2)) .ne. 2 ) .or. &
             ( nint(r8arh(3,2)*1000) .ne. 2286 ) .or. ( nint(r8arh(1,3)) .ne. 0 ) .or. ( nint(r8arh(3,3)*1000) .ne. 2256 ) .or. &
             ( nint(r8arh(1,4)) .ne. 2 ) .or. ( nint(r8arh(3,4)*1000) .ne. 2256 ) .or. ( nint(r8arh(1,37)) .ne. 0 ) .or. &
             ( nint(r8arh(3,37)*1000) .ne. 2225 ) .or. ( nint(r8arh(1,38)) .ne. 2 ) .or. ( nint(r8arh(3,38)*1000) .ne. 2225 ) .or. &
             ( nr8rdr .ne. 20 ) .or. ( nint(r8ardr(1,4)) .ne. 0 ) .or. ( nint(r8ardr(1,6)) .ne. 0 ) .or. &
             ( nint(r8ardr(1,9)) .ne. 0 ) .or. &
             ( nint(r8ardr(1,10)) .ne. 0 ) .or. ( nint(r8ardr(1,18)) .ne. 0 ) .or. ( nr8rd .ne. 0 ) ) stop 42

     else if ( isct .eq. 6 ) then
        if ( ifbget ( 11 ) .ne. 0 ) stop 61

        if ( lcmgdf ( 11, cmgtag ) .ne. 0 ) stop 62

        if ( ( nr8rr .ne. 1 ) .or. ( nint(r8arr(2,1)) .ne. 10 ) .or. ( nint(r8arr(4,1)) .ne. 12 ) .or. &
             ( nint(r8arr(5,1)) .ne. 49 ) ) stop 63

        if ( ( nr8rf .ne. 20 ) .or. ( nint(r8arf(1,1)) .ne. 4 ) .or. ( nint(r8arf(2,1)) .ne. 2014 ) .or. &
             ( nint(r8arf(3,1)) .ne. 10 ) .or. &
             ( nint(r8arf(4,1)) .ne. 5 ) .or. ( nint(r8arf(5,1)) .ne. 18 ) .or. &
             ( nint(r8arf(4,3)) .ne. 6 ) .or. ( nint(r8arf(5,3)) .ne. 6 ) .or. &
             ( nint(r8arf(4,9)) .ne. 7 ) .or. ( nint(r8arf(5,9)) .ne. 18 ) .or. &
             ( nint(r8arf(1,16)) .ne. 4 ) .or. ( nint(r8arf(4,16)) .ne. 9 ) .or. &
             ( nint(r8arf(5,16)) .ne. 12 ) .or. ( nint(r8arf(4,18)) .ne. 10 ) .or. &
             ( nint(r8arf(5,18)) .ne. 0 ) ) stop 64

     else if ( isct .eq. 7 ) then
        if ( ( nr8rhr .ne. 20 ) .or. ( nint(r8arhr(1,1)) .ne. 0 ) .or. &
             ( nint(r8arhr(1,5)) .ne. 0 ) .or. ( nint(r8arhr(1,13)) .ne. 0 ) .or. &
             ( nint(r8arhr(1,18)) .ne. 0 ) .or. ( nr8rh .ne. 0 ) .or. &
             ( nr8rdr .ne. 20 ) .or. ( nint(r8ardr(1,1)) .ne. 1 ) .or. &
             ( nint(r8ardr(1,2)) .ne. 1 ) .or. ( nint(r8ardr(1,11)) .ne. 1 ) .or. &
             ( nint(r8ardr(1,12)) .ne. 1 ) .or. ( nint(r8ardr(1,13)) .ne. 1 ) .or. &
             ( nint(r8ardr(1,15)) .ne. 1 ) .or. ( nr8rd .ne. 20 ) .or. &
             ( nint(r8ard(3,1)*100) .ne. 10421 ) .or. ( nint(r8ard(3,4)*100) .ne. 8976 ) .or. &
             ( nint(r8ard(3,11)*100) .ne. 5069 ) .or. ( nint(r8ard(3,12)*100) .ne. 4616 ) .or. &
             ( nint(r8ard(1,13)) .ne. 0 ) .or. ( nint(r8ard(2,13)) .ne. 0 ) .or. &
             ( nint(r8ard(3,13)*100) .ne. 4163 ) .or. ( nint(r8ard(4,13)) .ne. 1 ) .or. &
             ( nint(r8ard(5,13)) .ne. 26 ) .or. ( nint(r8ard(3,15)*100) .ne. 3766 ) ) stop 71

     else if ( isct .eq. 10 ) then
        if ( ifbget ( 11 ) .eq. 0 ) stop 101

     endif
  enddo

  ! Verify that all available subsets were successfully read.
  if ( isct .ne. 10 ) stop 112

  ! Check some mnemonic definitions.
  call nemdefs ( 11, 'HSMSL', celem, cunit, ier )
  if ( ( ier .ne. 0 ) .or. ( celem(1:36) .ne. 'HEIGHT OF STATION GROUND ABOVE MSL  ' ) .or. &
      ( cunit(1:9) .ne. 'METERS   ' ) ) stop 113
  call nemdefs ( 11, 'SHRV', celem, cunit, ier )
  if ( ( ier .ne. 0 ) .or. ( celem(1:24) .ne. 'SHEF DATA REVISION FLAG ' ) .or. &
      ( cunit(1:12) .ne. 'CODE TABLE  ' ) ) stop 114
  call nemdefs ( 11, 'DCHG', celem, cunit, ier )
  if ( ( ier .ne. 0 ) .or. ( celem(1:15) .ne. 'DISCHARGE      ' ) .or. &
      ( cunit(1:20) .ne. 'METERS**3/SECOND    ' ) ) stop 115

  print *, 'SUCCESS!'
end program intest3
