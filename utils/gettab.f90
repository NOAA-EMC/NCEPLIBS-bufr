!> @file
!> @brief Read BUFR file containing embedded DX BUFR tables,
!> and print the tables to stdout
!> @author WOOLLEN @date 2000-01-01

program gettab

  use bufr_procedures

  implicit none

  character(len=255) :: finput      !> name of filename to read
  integer, parameter :: lunit = 20
  logical :: file_exists

  !> get the name of filename to read as an input argument
  call getarg(1, finput)
  !> if no input argument, 'fort.20' is the default filename
  if (trim(adjustl(finput)) == '') finput = 'fort.20'
  !> ensure file exists; if not abort
  inquire(file=trim(adjustl(finput)), exist=file_exists)
  if (.not. file_exists) then
    write(6,'(3(A,X))') "File", trim(adjustl(finput)), 'does not exist, ABORT!'
    stop
  endif

  !> open file
  open(lunit,file=trim(adjustl(finput)), status='old', form='unformatted')
  call openbf(lunit, 'IN', lunit)
  !> dump table to stdout (unit 6)
  call dxdump(lunit, 6)
  !> close file
  call closbf(lunit)
  close(lunit)

  stop
end program gettab

