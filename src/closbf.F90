!> @file
!> @brief Close a previously opened file and disconnect it from the NCEPLIBS-bufr software.
!>
!> @author J. Woollen, J. Ator @date 1994-01-06

!> Close the connection between logical unit lunit and the NCEPLIBS-bufr software.
!>
!> @remarks
!> - This subroutine will execute a Fortran "CLOSE" on logical unit lunit, even though subroutine openbf() didn't
!> previously handle the corresponding Fortran "OPEN" of the same file.
!> - It's a good idea to call this subroutine for every lunit that was opened to the software via openbf(); however, it's
!> especially important to do so when writing/encoding a BUFR file, in order to ensure that all output is properly flushed
!> to lunit.
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file.
!>
!> @author J. Woollen, J. Ator @date 1994-01-06
recursive subroutine closbf(lunit)

  use bufrlib

  use modv_vars, only: im8b

  use moda_nulbfr

  character*128 errstr

  integer, intent(in) :: lunit
  integer my_lunit, lun, il, im

  ! Check for i8 integers

  if(im8b) then
     im8b=.false.

     call x84(lunit,my_lunit,1)
     call closbf(my_lunit)

     im8b=.true.
     return
  endif

  if ( .not. allocated(null) ) then
    call errwrt('++++++++++++++++++++WARNING++++++++++++++++++++++')
    errstr = 'BUFRLIB: CLOSBF WAS CALLED WITHOUT HAVING PREVIOUSLY CALLED OPENBF'
    call errwrt(errstr)
    call errwrt('++++++++++++++++++++WARNING++++++++++++++++++++++')
    return
  ENDIF

  call status(lunit,lun,il,im)
  if(il.gt.0 .and. im.ne.0) call closmg(lunit)
  if(il.ne.0 .and. null(lun).eq.0) call closfb_c(lun)
  call wtstat(lunit,lun,0,0)

  ! Close Fortran unit if null(lun) = 0

  if(null(lun).eq.0) close(lunit)

  return
end subroutine closbf
