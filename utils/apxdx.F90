!> @file
!> @brief Append a specified DX BUFR table to a specified BUFR file
!>
!> @author J. Ator @date 2009-07-01

!> Generate BUFR messages corresponding to a given DX BUFR table and append them to a given BUFR file.
!>
!> This program is primarily intended for use when implementing DX BUFR table changes to one or more BUFR tankfiles in the
!> NCEP observational database, but it has other potential uses as well.
!>
!> Usage: apxdx BUFRfile DXtable
!>
!> Upon successful output, the original BUFRfile will have BUFR messages appended to it corresponding to the given DXtable.
!>
!> @return 0 for success, error message otherwise
!>
!> @remarks
!> - The original BUFRfile must contain embedded DX BUFR table messages corresponding to any existing BUFR messages in the file
!> - Fortran logical unit numbers 10, 11, and 12 are reserved for use within this program
!>
!> @author J. Ator @date 2009-07-01
program apxdx

  implicit none

  character*240 cdxtbl, cbffil

  integer, parameter :: lunin = 10, lundx = 11, lunot = 12
  integer narg

  logical exists

  ! Read and verify the input arguments.
  narg = command_argument_count()
  if ( narg /= 2 ) then
    print *,'Usage: apxdx BUFRfile DXtable'
    call exit(1)
  endif
  call get_command_argument( 1, cbffil )
  inquire ( file = cbffil, exist = exists )
  if ( .not. exists ) then
    print *,'Specified BUFR file ' // trim(cbffil) // ' does not exist'
    call exit(2)
  endif
  call get_command_argument( 2, cdxtbl )
  inquire ( file = cdxtbl, exist = exists )
  if ( .not. exists ) then
    print *,'Specified DX table ' // trim(cdxtbl) // ' does not exist'
    call exit(3)
  endif

  ! Read the user DX table into the NCEPLIBS-bufr library.
  cdxtbl = trim(cdxtbl) // char(0)  ! append trailing null
  open ( unit = lundx, file = cdxtbl )
  call openbf ( lunin, 'INUL', lundx )

  ! Open the BUFR file for append.
  cbffil = trim(cbffil) // char(0)  ! append trailing null
  open ( unit = lunot, file = cbffil, form = 'unformatted' )
  call openbf ( lunot, 'APX', lunot )

  ! Generate DX messages from the table and append them to the BUFR file.
  call wrdxtb ( lunin, lunot )

  ! Close the BUFR file.
  call closbf ( lunot )

  stop
end program apxdx
