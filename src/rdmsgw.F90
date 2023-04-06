!> @file
!> @brief Read a BUFR message.
!>
!> @author J. Ator @date 2005-11-29

!> Read the next BUFR message from logical unit lunit as an array of integer words.
!>
!> @param[in] lunit - integer: Fortran logical unit number for BUFR file.
!> @param[out] mesg - integer(*): BUFR message.
!> @param[out] iret - integer: return code:
!> - 0 normal return.
!> - -1 end-of-file encountered while reading from lunit.
!>
!> @author J. Ator @date 2005-11-29
subroutine rdmsgw(lunit,mesg,iret)

  use bufrlib

  use modv_mxmsgl

  implicit none

  integer mesg(*), lunit, iret, lun, il, im

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

  call status(lunit,lun,il,im)
  iret = -2
  do while (iret.le.-2)
     iret = crdbufr_c(lun,mesg,mxmsgld4)
     if(iret.eq.-3) call errwrt('BUFRLIB: RDMSGW - SKIPPING OVERLARGE MESSAGE')
     if(iret.eq.-2) call errwrt('BUFRLIB: RDMSGW - SKIPPING CORRUPTED MESSAGE')
  end do

  return

end subroutine rdmsgw
