!> @file
!> @brief Test for "missing" data values
!>
!> @author J. Ator @date 2007-01-19

!> Check whether a real*8 data value returned from a previous call to any of the
!> NCEPLIBS-bufr [values-reading subroutines](@ref hierarchy)
!> contains the current placeholder value for "missing" data.
!>
!> @param r8val - Data value to be tested
!> @returns ibfms - Return code:
!> - 0 = r8val is not "missing"
!> - 1 = r8val is "missing"
!>
!> The current placeholder value for "missing" data
!> is always equal to the value xmiss as specified during the
!> most recent call to subroutine setbmiss(), or to a default
!> value of 10E10 if setbmiss() was never called.  In either
!> case, a return value of 1 means that the corresponding
!> value was encoded as "missing" (all bits set to 1)
!> within the actual BUFR data subset.
!>
!> @author J. Ator @date 2007-01-19
integer function ibfms ( r8val ) result ( iret )

  use modv_vars, only: bmiss

  implicit none

  real*8, intent(in) :: r8val

  if ( r8val .eq. bmiss ) then
    iret = 1
  else
    iret = 0
  endif

  return
end function ibfms

!> Check whether a character string returned from a previous call to subroutine
!> readlc() was encoded as "missing" (all bits set to 1) within the actual BUFR data subset.
!>
!> @param str - String
!> @param lstr - Length of string, i.e. number of characters within str to be tested
!> @returns icbfms - return code:
!>   - 0 = str is not "missing"
!>   - 1 = str is "missing"
!>
!> @remarks
!> - The use of an integer return code allows this function to be called in a logical context from application programs
!> written in C as well as in Fortran
!>
!> @author J. Ator @date 2012-06-07
recursive integer function icbfms ( str, lstr ) result ( iret )

  use modv_vars, only: im8b

  implicit none

  character*(*), intent(in) :: str
  character*8 strz
  character*16 zz
  character*16, parameter :: zm_be = '202020E076483742'   ! 10E10 stored as hexadecimal on a big-endian system
  character*16, parameter :: zm_le = '42374876E8000000'   ! 10E10 stored as hexadecimal on a little-endian system

  real*8 rl8z

  integer, intent(in) :: lstr
  integer my_lstr, numchr, ii, iupm

  equivalence(strz,rl8z)

  ! Check for I8 integers.

  if ( im8b ) then
    im8b = .false.

    call x84 ( lstr, my_lstr, 1 )
    iret = icbfms ( str, my_lstr )

    im8b = .true.
    return
  end if

  iret = 0

  numchr = min(lstr,len(str))

  ! Beginning with version 10.2.0 of the NCEPLIBS-bufr, all "missing" strings have been explicitly encoded with all bits set
  ! to 1, and which is the correct encoding per WMO regulations.  However, prior to version 10.2.0, the library encoded a
  ! "missing" string by storing the real*8 value of 10E10 into the string.  So for consistency with historical archives,
  ! the following logic attempts to identify some of these earlier cases, at least for strings between 4 and 8 bytes in length.

  if ( numchr.ge.4 .and. numchr.le.8 ) then
    do ii = 1, numchr
      strz(ii:ii) = str(ii:ii)
    end do
    write (zz,'(z16.16)') rl8z
    ii = 2*(8-numchr)+1
    if ( zz(ii:16).eq.zm_be(ii:16) .or. zz(ii:16).eq.zm_le(ii:16) ) then
      iret = 1
      return
    end if
  end if

  ! Otherwise, the logic below will check for "missing" strings of any length which are correctly encoded with all bits set
  ! to 1, including those encoded by NCEPLIBS-bufr version 10.2.0 or later.

  do ii=1,numchr
    strz(1:1) = str(ii:ii)
    if ( iupm(strz(1:1),8).ne.255 ) return
  enddo

  iret = 1

  return
end function icbfms

!> Get the current placeholder value which represents "missing" data when reading from or writing to BUFR files.
!>
!> @returns getbmiss - Current placeholder value for "missing" data
!>
!> This subroutine can be called at any time from within an
!> application program, and the returned value can then be
!> used to represent "missing" data within the context of
!> future calls to any of the other NCEPLIBS-bufr
!> [values-reading subroutines](@ref hierarchy) or
!> [values-writing subroutines](@ref hierarchy).
!> This placeholder value can also be changed at any
!> time via a separate call to subroutine setbmiss().
!>
!> @author J. Woollen @date 2012-09-15
real*8 function getbmiss() result(r8val)

  use modv_vars, only: bmiss

  implicit none

  r8val = bmiss

  return
end function getbmiss

!> Specify a customized value to represent "missing" data when reading from or writing to BUFR files.
!>
!> This subroutine can be called at any time from within an
!> application program, and the value xmiss will then be treated as
!> "missing" when reading or writing BUFR data during all future
!> calls to any of the other NCEPLIBS-bufr
!> [values-reading subroutines](@ref hierarchy) or
!> [values-writing subroutines](@ref hierarchy).
!> Otherwise, if this subroutine is never called, a default
!> placeholder value of 10E10_8 is used for "missing".
!>
!> Any data value can always be checked for equivalence to the current "missing" value via a call to function ibfms().
!> See also function getbmiss().
!>
!> @remarks
!> - The value xmiss is never actually encoded within a BUFR data
!> subset; rather, xmiss is a user-friendly placeholder value to
!> represent "missing" data values within the scope of the
!> application program.  In any actual BUFR data subset, "missing"
!> values are always encoded as all bits set to 1, per WMO
!> regulations.
!>
!> @param xmiss - New placeholder value to represent "missing" data
!>
!> @author J. Woollen @date 2012-09-15
subroutine setbmiss(xmiss)

  use modv_vars, only: bmiss

  implicit none

  real*8, intent(in) :: xmiss

  bmiss = xmiss

  return
end subroutine setbmiss
