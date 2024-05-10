!> @file
!> @brief Read or write data values within a BUFR data subset.
!>
!> @authors J. Woollen, J. Ator @date 1994-01-06

!> Write a data value corresponding to
!> a specific occurrence of a mnemonic within a data subset, based on
!> its position relative to a different mnemonic within the subset.
!>
!> The subroutine first searches for a specific occurrence of a pivot
!> mnemonic, counting from the beginning of the subset.  From there,
!> it then searches in either a forward or backward direction for a
!> specific occurrence of a nearby mnemonic, and if found
!> stores the specified data value in the corresponding location
!> within the subset.
!>
!> Before calling this subroutine, a BUFR message should already be
!> opened and initialized for output via a previous call to one of the
!> NCEPLIBS-bufr [message-writing subroutines](@ref hierarchy).
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param tagpv - Pivot mnemonic; the subroutine will first search for the (ntagpv)th occurrence of this mnemonic, counting
!> from the beginning of the overall subset definition
!> @param ntagpv - Ordinal occurrence of tagpv to search for, counting from the beginning of the overall subset definition
!> @param tagnb - Nearby mnemonic; assuming tagpv is successfully found, the subroutine will then search nearby for the
!> (ntagnb)th occurrence of tagnb and store r8val as the corresponding value
!> @param ntagnb - Ordinal occurrence of tagnb to search for, counting from the location of tagpv within the overall subset
!> definition
!>  - If ntagnb is positive, the subroutine will search in a forward direction from the location of tagpv
!>  - If ntagnb is negative, the subroutine will search in a backwards direction from the location of tagpv
!> @param r8val - Value to be stored corresponding to (ntagnb)th occurrence of tagnb within the subset
!> @param iret - return code:
!>  - 0 = r8val was successfully stored
!>  - -1 = the (ntagnb)th occurence of mnemonic tagnb could not be found, or some other error occurred
!>
!> @author J. Ator @date 2016-07-29
recursive subroutine setvalnb ( lunit, tagpv, ntagpv, tagnb, ntagnb, r8val, iret )

  use modv_vars, only: im8b

  use moda_usrint
  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: lunit, ntagpv, ntagnb
  integer, intent(out) :: iret
  integer my_lunit, my_ntagpv, my_ntagnb, lun, il, im, npv, nnb, ierft

  character*(*), intent(in) :: tagpv, tagnb

  real*8, intent(in) ::  r8val

  ! Check for I8 integers.
  if(im8b) then
    im8b=.false.
    call x84 ( lunit, my_lunit, 1 )
    call x84 ( ntagpv, my_ntagpv, 1 )
    call x84 ( ntagnb, my_ntagnb, 1 )
    call setvalnb ( my_lunit, tagpv, my_ntagpv, tagnb, my_ntagnb, r8val, iret )
    call x48 ( iret, iret, 1 )
    im8b=.true.
    return
  endif

  iret = -1

  ! Get lun from lunit.
  call status (lunit, lun, il, im )
  if ( il .le. 0 ) return
  if ( inode(lun) .ne. inv(1,lun) ) return

  ! Starting from the beginning of the subset, locate the (ntagpv)th occurrence of tagpv.
  call fstag( lun, tagpv, ntagpv, 1, npv, ierft )
  if ( ierft .ne. 0 ) return

  ! Now, starting from the (ntagpv)th occurrence of tagpv, search forward or backward for the (ntagnb)th occurrence of tagnb.
  call fstag( lun, tagnb, ntagnb, npv, nnb, ierft )
  if ( ierft .ne. 0 ) return

  iret = 0
  val(nnb,lun) = r8val

  return
end subroutine setvalnb

!> Read a data value corresponding to
!> a specific occurrence of a mnemonic within a data subset, based on
!> its position relative to a different mnemonic within the subset.
!>
!> The function first searches for a specific occurrence of a pivot
!> mnemonic, counting from the beginning of the subset.  From there,
!> it then searches in either a forward or backward direction for a
!> specific occurrence of a nearby mnemonic, and if found
!> returns the data value from the corresponding location
!> within the subset.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param tagpv - Pivot mnemonic; the subroutine will first search for the (ntagpv)th occurrence of this mnemonic, counting
!> from the beginning of the overall subset definition
!> @param ntagpv - Ordinal occurrence of tagpv to search for, counting from the beginning of the overall subset definition
!> @param tagnb - Nearby mnemonic; assuming tagpv is successfully found, the subroutine will then search nearby for the
!> (ntagnb)th occurrence of tagnb and return the corresponding value
!> @param ntagnb - Ordinal occurrence of tagnb to search for, counting from the location of tagpv within the overall subset
!> definition
!>  - If ntagnb is positive, the subroutine will search in a forward direction from the location of tagpv
!>  - If ntagnb is negative, the subroutine will search in a backwards direction from the location of tagpv
!> @returns getvalnb - Value corresponding to (ntagnb)th occurrence of tagnb
!>  - If for any reason this value cannot be located, then the current placeholder value for "missing" data will be returned
!>  instead
!>
!> The current placeholder value for "missing" data can be determined
!> via a separate call to function getbmiss().
!>
!> Before calling this function, a BUFR data subset should already be
!> open for reading via a previous call to one of the NCEPLIBS-bufr
!> [subset-reading subroutines](@ref hierarchy).
!>
!> @author J. Ator @date 2012-09-12
recursive real*8 function getvalnb ( lunit, tagpv, ntagpv, tagnb, ntagnb ) result ( r8val )

  use modv_vars, only: im8b, bmiss

  use moda_usrint
  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: lunit, ntagpv, ntagnb
  integer my_lunit, my_ntagpv, my_ntagnb, lun, il, im, npv, nnb, ierft

  character*(*), intent(in) :: tagpv, tagnb

  ! Check for I8 integers.
  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call x84(ntagpv,my_ntagpv,1)
    call x84(ntagnb,my_ntagnb,1)
    r8val=getvalnb(my_lunit,tagpv,my_ntagpv,tagnb,my_ntagnb)
    im8b=.true.
    return
  endif

  r8val = bmiss

  ! Get lun from lunit.
  call status (lunit, lun, il, im )
  if ( il .ge. 0 ) return
  if ( inode(lun) .ne. inv(1,lun) ) return

  ! Starting from the beginning of the subset, locate the (ntagpv)th occurrence of tagpv.
  call fstag( lun, tagpv, ntagpv, 1, npv, ierft )
  if ( ierft .ne. 0 ) return

  ! Now, starting from the (ntagpv)th occurrence of tagpv, search forward or backward for the (ntagnb)th occurrence of tagnb.
  call fstag( lun, tagnb, ntagnb, npv, nnb, ierft )
  if ( ierft .ne. 0 ) return

  r8val = val(nnb,lun)

  return
end function getvalnb
