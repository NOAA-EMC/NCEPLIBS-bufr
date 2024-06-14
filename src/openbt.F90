!> @file
!> @brief Specify DX BUFR table file of last resort.
!>
!> @author J. Woollen @date 1998-07-08

!> Specify a DX BUFR table of last resort, in case subroutine
!> cktaba() is unable to locate a DX BUFR table on its own
!> when reading/decoding a BUFR message.
!>
!> Within the NCEPLIBS-bufr distribution package, this subroutine
!> is a default placeholder which always returns lundx = 0
!> and is only included to allow application programs to compile
!> without generating a link error for an unresolved external
!> reference.  However, users are free to define their own inline
!> version of this subroutine with the same name and calling sequence
!> and include it within the compilation of their application program
!> in order to override the default placeholder version of this
!> subroutine.  In such cases, subroutine cktaba() will then use
!> the inline version of this subroutine as a last resort when
!> attempting to locate the required DX BUFR table file.
!>
!> @param mtyp - Data category of BUFR message for which subroutine cktaba() was unable to locate a DX BUFR table file
!> @param lundx - Fortran logical unit number for file containing DX BUFR table information to be used in decoding message
!>   - 0 = No such file is available
!>
!> @remarks
!> - See [DX BUFR Tables](@ref dfbftab) for more information about the format and contents of DX BUFR table files.
!>
!> @author J. Woollen @date 1998-07-08
recursive subroutine openbt(lundx,mtyp)

  use modv_vars, only: im8b

  implicit none

  integer, intent(in) :: mtyp
  integer, intent(out) :: lundx
  integer iprt, my_mtyp

  common /quiet/ iprt

  character*128 errstr

  ! Check for I8 integers
  if(im8b) then
    im8b=.false.
    call x84(mtyp,my_mtyp,1)
    call openbt(lundx,my_mtyp)
    call x48(lundx,lundx,1)
    im8b=.true.
    return
  endif

  if(iprt>=0) then
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    errstr = 'BUFRLIB: OPENBT - THIS IS A DUMMY BUFRLIB ROUTINE'// &
      ' CALLED BY CKTABA OR APPL. PGM; OPENBT SHOULD BE INCL. IN-LINE IN APPL. PGM'
    call errwrt(errstr)
    call errwrt('+++++++++++++++++++++WARNING+++++++++++++++++++++++')
    call errwrt(' ')
  endif

  lundx = 0

  return
end subroutine openbt
