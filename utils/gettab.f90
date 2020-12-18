!> @file
!> @brief Read BUFR file containing embedded DX tables,
!> and print the table to stdout

program gettab

implicit none

character(len=255) :: finput
integer, parameter :: lunit = 20
logical :: file_exists

call getarg(1, finput)
if (trim(adjustl(finput)) == '') finput = 'fort.20'
inquire(file=trim(adjustl(finput)), exist=file_exists)
if (.not. file_exists) then
  write(6,'(3(A,X))') "File", trim(adjustl(finput)), 'does not exist, ABORT!'
  stop
endif

open(lunit,file=trim(adjustl(finput)), status='old', form='unformatted')
call openbf(lunit, 'IN', lunit)
call dxdump(lunit, 6)
call closbf(lunit)
close(lunit)

stop
end
