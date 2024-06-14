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
!> @param idn - WMO bit-wise representation of FXY value
!> @param ldn - Length of string to be returned; can be either 5 or 6 characters
!> @returns adn30 - FXY character string
!>
!> @author J. Woollen @date 1994-01-06
function adn30(idn,ldn)

  use modv_vars, only: nbitw

  implicit none

  integer, intent(in) :: idn, ldn

  integer i, idf, idx, idy

  character*(*) adn30
  character*128 bort_str

  if(len(adn30)<ldn) call bort('BUFRLIB: ADN30 - FUNCTION RETURN STRING TOO SHORT')
  if(idn<0 .or. idn>65535) call bort('BUFRLIB: ADN30 - INTEGER REPRESENTATION OF DESCRIPTOR OUT OF 16-BIT RANGE')

  if(ldn==5) then
    write(adn30,'(i5)') idn
  elseif(ldn==6) then
    idf = ishft(idn,-14)
    idx = ishft(ishft(idn,nbitw-14),-(nbitw-6))
    idy = ishft(ishft(idn,nbitw- 8),-(nbitw-8))
    write(adn30,'(i1,i2,i3)') idf,idx,idy
  else
    write(bort_str,'("BUFRLIB: ADN30 - CHARACTER LENGTH (",I4,") MUST BE EITHER 5 OR 6")') ldn
    call bort(bort_str)
  endif

  do i=1,ldn
    if(adn30(i:i)==' ') adn30(i:i) = '0'
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
!> @param idn - WMO bit-wise representation of FXY value
!> @param adn - FXY value
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
!> @param adn - FXY value; must be of length 5 or 6
!> @param ldn - Length of adn; can be either 5 or 6 characters
!> @returns idn30 - WMO bit-wise representation of FXY value
!>
!> @author J. Woollen @date 1994-01-06
integer function idn30(adn,ldn) result(iret)

  implicit none

  integer, intent(in) :: ldn

  character*(*), intent(in) :: adn

  character*128 bort_str

  integer ifxy

  if(len(adn)<ldn) then
    write(bort_str,'("BUFRLIB: IDN30 - FUNCTION INPUT STRING ",A," CHARACTER LENGTH (",I4,") IS TOO SHORT (< LDN,",I5)') &
          adn, len(adn), ldn
    call bort(bort_str)
  endif

  if(ldn==5) then
    read(adn,'(i5)') iret
    if(iret<0 .or. iret>65535) then
      write(bort_str, &
        '("BUFRLIB: IDN30 - DESCRIPTOR INTEGER REPRESENTATION, IDN30 (",I8,"), IS OUTSIDE 16-BIT RANGE (0-65535)")') iret
      call bort(bort_str)
    endif
  elseif(ldn==6) then
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
!> @param adsc - FXY value
!> @returns ifxy - WMO bit-wise representation of FXY value
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
!> @param str - input string
!> @param cfxy - FXY number in format FXXYYY
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
  if ( lstr < 6 ) return

  ! Left-justify a copy of the input string.

  if ( lstr > lstr2 ) then
    str2(1:lstr2) = str(1:lstr2)
  else
    str2 = str
  endif
  str2 = adjustl ( str2 )
  if ( str2 == ' ' ) return

  ! Look for an FXY number.

  if ( index ( str2, '-' ) /= 0 ) then
    ! Format of field is F-XX-YYY.
    cfxy(1:1) = str2(1:1)
    cfxy(2:3) = str2(3:4)
    cfxy(4:6) = str2(6:8)
  else
    ! Format of field is FXXYYY.
    cfxy = str2(1:6)
  endif

  ! Check that the FXY number is valid.

  if ( numbck ( cfxy ) == 0 ) iret = 0

  return
end function igetfxy

!> Check an FXY number for validity.
!>
!> @param numb - FXY value
!>
!> @returns numbck - Indicator as to whether numb is valid:
!> - 0 = Yes
!> - -1 = No, the first character ("F" value) is not '0', '1', '2', or '3'
!> - -2 = No, characters 2-6 ("X" and "Y" values) are not all numeric
!> - -3 = No, characters 2-3 ("X" value) are not between '00' and '63'
!> - -4 = No, characters 4-6 ("Y" value) are not between '000' and '255'
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

  if(ix<0 .or. ix> 63) then
    iret = -3
    return
  else if(iy<0 .or. iy>255) then
    iret = -4
    return
  endif

  iret = 0

  return
end function numbck

!> Get information about a Table B or Table D descriptor, based on the WMO bit-wise representation of an FXY value.
!>
!> For a description of the WMO bit-wise representation of the FXY value, see ifxy().
!>
!> @param lun - File ID associated with DX BUFR tables
!> @param idn - WMO bit-wise representation of FXY value for Table B or Table D descriptor
!> @param nemo - Mnemonic associated with idn
!> @param tab - Type associated with idn:
!>  - 'B' Table B descriptor
!>  - 'D' Table D descriptor
!> @param iret - Return code:
!>  - Positional index of idn within internal Table B, if tab = 'B'
!>  - Positional index of idn within internal Table D, if tab = 'D'
!>  - 0 otherwise
!>
!> @author J. Woollen @date 2002-05-14
subroutine numtbd(lun,idn,nemo,tab,iret)

  use moda_tababd

  implicit none

  integer, intent(in) :: lun, idn
  integer, intent(out) :: iret
  integer i, ifxy

  character*(*), intent(out) :: nemo
  character, intent(out) :: tab

  nemo = ' '
  iret = 0
  tab = ' '

  if(idn>=ifxy('300000')) then
    ! Look for idn in Table D
    do i=1,ntbd(lun)
      if(idn==idnd(i,lun)) then
        nemo = tabd(i,lun)(7:14)
        tab  = 'D'
        iret = i
        return
      endif
    enddo
  else
    ! Look for idn in Table B
    do i=1,ntbb(lun)
      if(idn==idnb(i,lun)) then
        nemo = tabb(i,lun)(7:14)
        tab  = 'B'
        iret = i
        return
      endif
    enddo
  endif

  return
end subroutine numtbd

!> Get information about a descriptor, based on the WMO bit-wise representation of an FXY value.
!>
!> For a description of the WMO bit-wise representation of the FXY value, see ifxy().
!>
!> @param lun - File ID associated with DX BUFR tables
!> @param idn - WMO bit-wise representation of FXY value for descriptor
!> @param nemo - Mnemonic associated with idn
!> @param tab - Type associated with idn:
!>  - 'B' Table B descriptor
!>  - 'D' Table D descriptor
!>  - 'C' Table C operator
!>  - 'R' Replication descriptor
!>  - 'F' Replication factor
!> @param iret - Return code:
!>  - Positional index of idn within internal Table B, if tab = 'B'
!>  - Positional index of idn within internal Table D, if tab = 'D'
!>  - The X portion of the FXY value in idn, if tab = 'C'
!>  - ((-1) * the Y portion of the FXY value in idn), if tab = 'R' and the replication is regular (i.e. non-delayed)
!>  - 5 if tab = 'R' or tab = 'F' and the replication is 1-bit delayed
!>  - 4 if tab = 'R' or tab = 'F' and the replication is 8-bit delayed (stack)
!>  - 3 if tab = 'R' or tab = 'F' and the replication is 8-bit delayed
!>  - 2 if tab = 'R' or tab = 'F' and the replication is 16-bit delayed
!>  - 0 otherwise
!>
!> @author J. Woollen @date 1994-01-06
subroutine numtab(lun,idn,nemo,tab,iret)

  use modv_vars, only: idnr

  implicit none

  integer, intent(in) :: lun, idn
  integer, intent(out) :: iret
  integer i, iokoper

  character*(*), intent(out) :: nemo
  character, intent(out) :: tab
  character*6 adn30, cid

  nemo = ' '
  iret = 0
  tab = ' '

  ! Look for a replicator or a replication factor descriptor

  if(idn>=idnr(1) .and. idn<=idnr(6)) then
    ! Note that the above test is checking whether idn is the bit-wise representation of a FXY (descriptor) value
    ! denoting F=1 regular (i.e. non-delayed) replication, since, as was initialized within subroutine bfrini(),
    ! idnr(1) = ifxy('101000'), and idnr(6) = ifxy('101255').
    tab = 'R'
    iret = -mod(idn,256)
    return
  endif

  do i=2,5
    if(idn==idnr(i)) then
      tab = 'R'
      iret = i
      return
    elseif(idn==idnr(i+5)) then
      tab = 'F'
      iret = i
      return
    endif
  enddo

  ! Look for idn in Table B and Table D

  call numtbd(lun,idn,nemo,tab,iret)
  if(iret/=0) return

  ! Look for idn in Table C

  cid = adn30(idn,6)
  if (iokoper(cid)==1) then
    nemo = cid(1:6)
    read(nemo,'(1X,I2)') iret
    tab = 'C'
    return
  endif

  return
end subroutine numtab

!> Get information about a descriptor, based on a mnemonic.
!>
!> @param lun - File ID associated with DX BUFR tables
!> @param nemo - Mnemonic
!> @param idn - WMO bit-wise representation of FXY value for descriptor for descriptor associated with nemo
!> @param tab - Type associated with idn:
!> - 'B' Table B descriptor.
!> - 'D' Table D descriptor.
!> - 'C' Table C operator.
!> @param iret - Return code:
!>  - Positional index of idn within internal Table B, if tab = 'B'
!>  - Positional index of idn within internal Table D, if tab = 'D'
!>  - The X portion of the FXY value in idn, if tab = 'C'.
!>  - 0, otherwise
!>
!> @author J. Woollen @date 1994-01-06
subroutine nemtab(lun,nemo,idn,tab,iret)

  use moda_tababd

  implicit none

  integer, intent(in) :: lun
  integer, intent(out) :: idn, iret
  integer i, j, ifxy, iokoper

  character*(*), intent(in) :: nemo
  character, intent(out) :: tab
  character*8 nemt

  logical folval

  folval = nemo(1:1)=='.'
  iret = 0
  tab = ' '

  ! Look for nemo in Table B

  outer: do i=1,ntbb(lun)
    nemt = tabb(i,lun)(7:14)
    if(nemt==nemo) then
      idn = idnb(i,lun)
      tab = 'B'
      iret = i
      return
    elseif(folval.and.nemt(1:1)=='.') then
      do j=2,len(nemt)
        if(nemt(j:j)/='.' .and. nemt(j:j)/=nemo(j:j)) cycle outer
      enddo
      idn = idnb(i,lun)
      tab = 'B'
      iret = i
      return
    endif
  enddo outer

  ! Don't look in Table D for following value-mnemonics

  if(folval) return

  ! Look in Table D if we got this far

  do i=1,ntbd(lun)
    nemt = tabd(i,lun)(7:14)
    if(nemt==nemo) then
      idn = idnd(i,lun)
      tab = 'D'
      iret = i
      return
    endif
  enddo

  ! If still nothing, check for Table C operator descriptors

  if (iokoper(nemo)==1) then
    read(nemo,'(1X,I2)') iret
    idn = ifxy(nemo)
    tab = 'C'
    return
  endif

  return
end subroutine nemtab
