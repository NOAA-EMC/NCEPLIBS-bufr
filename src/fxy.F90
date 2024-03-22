!> @file
!> @brief Process FXY values, including converting between character and WMO bit-wise representations.
!>
!> @author J. Ator @date 2024-02-29

!> Convert an FXY value from its WMO bit-wise representation to a character string of length 5 or 6.
!>      
!> For an description of the WMO bit-wise representation of the FXY value, see ifxy().
!>
!> This function is the logical inverse of function idn30().
!>
!> @param[in] idn - integer: WMO bit-wise representation of FXY value
!> @param[in] ldn - integer: Length of string to be returned; can be either 5 or 6 characters
!> @returns adn30 - character*(*): FXY character string
!>
!> @author J. Woollen @date 1994-01-06
function adn30(idn,ldn)

  use modv_vars, only: nbitw

  implicit none

  integer, intent(in) :: idn, ldn

  integer i, idf, idx, idy

  character*(*) adn30
  character*128 bort_str

  if(len(adn30).lt.ldn) call bort('BUFRLIB: ADN30 - FUNCTION RETURN STRING TOO SHORT') 
  if(idn.lt.0 .or. idn.gt.65535) call bort('BUFRLIB: ADN30 - INTEGER REPRESENTATION OF DESCRIPTOR OUT OF 16-BIT RANGE')

  if(ldn.eq.5) then
    write(adn30,'(i5)') idn
  elseif(ldn.eq.6) then
    idf = ishft(idn,-14)
    idx = ishft(ishft(idn,nbitw-14),-(nbitw-6))
    idy = ishft(ishft(idn,nbitw- 8),-(nbitw-8))
    write(adn30,'(i1,i2,i3)') idf,idx,idy
  else
    write(bort_str,'("BUFRLIB: ADN30 - CHARACTER LENGTH (",I4,") MUST BE EITHER 5 OR 6")') ldn
    call bort(bort_str)
  endif

  do i=1,ldn
    if(adn30(i:i).eq.' ') adn30(i:i) = '0'
  enddo

  return
end function adn30

!> Convert an FXY value from its WMO bit-wise representation to its 6 character representation.
!>
!> This subroutine is similar to function adn30(), except that it always returns 6 characters,
!> and it always returns its output as a call parameter instead of a function value, which in turn
!> allows it to be more easily called from within a C language function.
!>
!> For a description of the WMO bit-wise representation of an FXY value, see ifxy().
!>
!> @param[in] idn - integer: WMO bit-wise representation of FXY value
!> @param[out] adn - character*6: FXY value
!>
!> @author J. Ator @date 2004-08-18
subroutine cadn30( idn, adn )

  implicit none

  integer, intent(in) :: idn

  character*(*), intent(out) :: adn

  character*6 adn30

  adn = adn30( idn, 6 )

  return
end subroutine cadn30

!> Convert an FXY value from a character string to the WMO bit-wise representation.
!>
!> For an description of the WMO bit-wise representation of the FXY value, see ifxy().
!>
!> This function is the logical inverse of function adn30().
!>
!> @param[in] adn - character*(*): FXY value; must be of length 5 or 6
!> @param[in] ldn - integer: Length of adn; can be either 5 or 6 characters
!> @returns idn30 - integer: WMO bit-wise representation of FXY value
!>
!> @author J. Woollen @date 1994-01-06
integer function idn30(adn,ldn) result(iret)

  implicit none

  integer, intent(in) :: ldn

  character*(*), intent(in) :: adn

  character*128 bort_str

  integer ifxy

  if(len(adn).lt.ldn) then
    write(bort_str,'("BUFRLIB: IDN30 - FUNCTION INPUT STRING ",A," CHARACTER LENGTH (",I4,") IS TOO SHORT (< LDN,",I5)') &
          adn, len(adn), ldn
    call bort(bort_str)
  endif

  if(ldn.eq.5) then
    read(adn,'(i5)') iret
    if(iret.lt.0 .or. iret.gt.65535) then
      write(bort_str, &
        '("BUFRLIB: IDN30 - DESCRIPTOR INTEGER REPRESENTATION, IDN30 (",I8,"), IS OUTSIDE 16-BIT RANGE (0-65535)")') iret
      call bort(bort_str)
    endif
  elseif(ldn.eq.6) then
    iret = ifxy(adn)
  else
    write(bort_str,'("BUFRLIB: IDN30 - FUNCTION INPUT STRING ",A," CHARACTER LENGTH (",I4,") MUST BE EITHER 5 OR 6")') &
          adn,ldn
    call bort(bort_str)
  endif

  return
end function idn30

!> Convert an FXY value from its 6 character representation to its WMO bit-wise representation.
!>
!> Per the [official WMO BUFR regulations](@ref manual), an FXY value
!> can be represented as a bit-wise integer in 16 bits, ordered from
!> left (most significant) to right (least significant), and where the
!> F value occupies the first 2 bits, the X value occupies the next 6
!> bits, and the Y value occupies the last 8 bits.
!>
!>     For example, if ADSC = '063022'
!>
!>        F |     X     |      Y
!>        0 |     63    |      22
!>       0 0 1 1 1 1 1 1 0 0 0 1 0 1 1 0
!>
!>     then the corresponding ifxy
!>
!>       = ( 2**13 + 2**12 + 2**11 + 2**10 + 2**9 + 2**8 +
!>           2**4 + 2**2 + 2**1 )
!>
!>       = 16150
!>
!> @param[in] adsc - character*6: FXY value
!> @returns ifxy - integer: WMO bit-wise representation of FXY value
!>
!> @author J. Woollen @date 1994-01-06
integer function ifxy(adsc) result(iret)

  implicit none

  integer if, ix, iy

  character*6, intent(in) :: adsc

  read(adsc,'(i1,i2,i3)') if,ix,iy
  iret = if*2**14 + ix*2**8 + iy

  return
end function ifxy

!> Search for and return a valid FXY number from within a character string.
!>
!> The FXY number may be in format of either FXXYYY or F-XX-YYY within the input string, but
!> it is always returned in format FXXYYY upon output.
!>
!> @param[in] str - character*(*): input string
!> @param[out] cfxy - character*6: FXY number in format FXXYYY
!>
!> @return
!> - 0 normal return.
!> - -1 could not find a valid FXY number in str
!>
!> @author Ator @date 2007-01-19
integer function igetfxy ( str, cfxy ) result ( iret )

  implicit none

  character*(*), intent(in) :: str
  character*6, intent(out) :: cfxy

  integer, parameter :: lstr2 = 120
  character*(lstr2) str2

  integer lstr, numbck

  iret = -1

  lstr = len ( str )
  if ( lstr .lt. 6 ) return

  ! Left-justify a copy of the input string.

  if ( lstr .gt. lstr2 ) then
    str2(1:lstr2) = str(1:lstr2)
  else
    str2 = str
  endif
  str2 = adjustl ( str2 )
  if ( str2 .eq. ' ' ) return

  ! Look for an FXY number.

  if ( index ( str2, '-' ) .ne. 0 ) then
    ! Format of field is F-XX-YYY.
    cfxy(1:1) = str2(1:1)
    cfxy(2:3) = str2(3:4)
    cfxy(4:6) = str2(6:8)
  else
    ! Format of field is FXXYYY.
    cfxy = str2(1:6)
  endif

  ! Check that the FXY number is valid.

  if ( numbck ( cfxy ) .eq. 0 ) iret = 0

  return
end function igetfxy

!> Check the input character string to determine whether it contains a valid FXY (descriptor) value.
!>
!> @param[in] numb - character*6: FXY value to be checked
!>
!> @return indicator as to whether numb is valid:
!> - 0 yes
!> - -1 no, the first character ("F" value) is not '0', '1', '2', or '3'
!> - -2 no, characters 2-6 ("X" and "Y" values) are not all numeric
!> - -3 no, characters 2-3 ("X" value) are not between '00' and '63'
!> - -4 no, characters 4-6 ("Y" value) are not between '000' and '255'
!>
!> @author Woollen @date 1994-01-06
integer function numbck(numb) result(iret)

  implicit none

  character*6, intent(in) :: numb

  integer ix, iy

  ! Check the first character of numb.

  if( llt(numb(1:1),'0') .or. lgt(numb(1:1),'3') ) then
    iret = -1
    return
  endif

  ! Check for a valid descriptor.

  if( verify(numb(2:6),'1234567890') == 0 ) then
    read(numb,'(1x,i2,i3)') ix,iy
  else
    iret = -2
    return
  endif

  if(ix.lt.0 .or. ix.gt. 63) then
    iret = -3
    return
  else if(iy.lt.0 .or. iy.gt.255) then
    iret = -4
    return
  endif

  iret = 0

  return
end function numbck
